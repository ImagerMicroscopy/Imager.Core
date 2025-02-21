{-# LANGUAGE OverloadedStrings #-}

module Equipment.Devices.PIStage (
    initializePIStage,
    PIStage
)

where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Format as T
import qualified Data.Text.Lazy as LT
import Data.Word
import System.Environment
import System.FilePath
import qualified System.Timeout as ST
import Text.Printf

import Equipment.Equipment
import Equipment.EquipmentTypes
import Utils.MiscUtils
import RCSerialPort

data PIStage = PIStage {
                      pisName :: !EqName
                    , pisPort :: !SerialPort
                    , pisAxes :: ![StageAxis]
                  }

-- todo: only reference if needed: FRF? return 1=1\n2=1 if referenced, or =0 if not referenced

initializePIStage :: EquipmentDescription -> IO EquipmentW
initializePIStage (PIStageDesc name portName) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS115200}) (TimeoutMillis 30000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port ->
        isPIStage port >>= \isStage ->
        when (not isStage) (
            throwIO $ userError "not a PI stage") >>
        determineAvailableAxes port >>= \axes ->
        enableDisableServos port axes True >>
        sendPIStageMessageNoResponse port "FRF" >>  -- reference axes (move to home)
        pure (EquipmentW (PIStage (EqName name) port axes))
    where
        determineAvailableAxes port =
            handlePIStageMessage port "SAI?" >>= \response ->
            pure [snd ax | ax <- [(fromIntegral (fromEnum '1'), XAxis), (fromIntegral (fromEnum '2'), YAxis), (fromIntegral (fromEnum '3'), ZAxis)], ((fst ax) `B.elem` response)]
        isPIStage :: SerialPort -> IO Bool
        isPIStage port = 
            handlePIStageMessage port "*IDN?" >>= \resp ->
            pure ("Physik" `B.isInfixOf` resp)

instance Equipment PIStage where
    equipmentName = pisName
    flushSerialPorts p = flushSerialPort (pisPort p)
    closeDevice p = enableDisableServos p.pisPort p.pisAxes False >> closeSerialPort (pisPort p)
    hasMotorizedStage _ = True
    motorizedStageName _ = StageName "PI Stage"
    supportedStageAxes = pisAxes
    getStagePosition (PIStage _ port axes) =
        StagePosition <$> (if (XAxis `elem` axes) then posOfAxis 1 else pure (-1))
                      <*> (if (YAxis `elem` axes) then posOfAxis 2 else pure (-1))
                      <*> (if (ZAxis `elem` axes) then posOfAxis 3 else pure (-1))
                      <*> pure False <*> pure 0
        where
          posOfAxis :: Int -> IO Double
          posOfAxis idx =
              handlePIStageMessage port (formatBS "POS? {}" (T.Only idx)) >>= \response ->
              (pure . (*) 1000 . read . byteStringAsString) (B.drop 2 response)

    setStagePosition (PIStage _ port axes) (StagePosition x y z _ _) =
        doMove `onException` abortMovement
        where
          doMove = forM_ posMsgs (sendPIStageMessageNoResponse port) >> waitUntilMovementStops
          axisSpecs :: [(StageAxis, Int, Double)]
          axisSpecs = filter (\(ax, _, _) -> ax `elem` axes) [(XAxis, 1, x / 1000), (YAxis, 2, y / 1000), (ZAxis, 3, z / 1000)]
          posMsgs = map (\(_, idx, pos) -> formatBS "MOV {} {}" (idx, pos)) axisSpecs
          waitUntilMovementStops = isMoving >>= \moves ->
                                   if (moves)
                                   then threadDelay 5000 >> waitUntilMovementStops
                                   else pure ()
          isMoving :: IO Bool
          isMoving = forM axes (getAxisStatus port) >>=
                     pure . any (\status -> (status .&. (shift 1 13)) /= 0)
          abortMovement = sendPIStageMessageNoResponse port "HLT"

enableDisableServos :: SerialPort -> [StageAxis] -> Bool -> IO ()
enableDisableServos port axes enabled = 
    when (XAxis `elem` axes) (sendPIStageMessageNoResponse port (msg 1)) >>
    when (YAxis `elem` axes) (sendPIStageMessageNoResponse port (msg 2)) >>
    when (ZAxis `elem` axes) (sendPIStageMessageNoResponse port (msg 3))
    where
        msg :: Int -> ByteString
        msg idx =
            let flag = if enabled then 1 else 0
            in  formatBS "SVO {} {}" (idx, flag :: Int)

getAxisStatus :: SerialPort -> StageAxis -> IO Int
getAxisStatus port axis =
    handlePIStageMessage port (formatBS "SRG? {} 1" (T.Only axisID)) >>= pure . parseResponse
    where
        parseResponse :: ByteString -> Int
        parseResponse bs = let length = B.length bs
                               numPart = B.drop (length - 7) bs
                           in readHexIntB numPart
        axisID = case axis of
                     XAxis -> 1 :: Int
                     YAxis -> 2
                     ZAxis -> 3

handlePIStageMessage :: SerialPort -> ByteString -> IO ByteString
handlePIStageMessage port bs = 
    serialWriteAndReadUntilCustom port (bs <> "\n") satisfiedP
    where
        -- PI says: if multiline response then non-last lines terminated by " \n"
        -- with a leading space character. Last line has no leading space.
        satisfiedP :: ByteString -> Bool
        satisfiedP bs | (length >= 2) && ((B.drop (length - 2) bs) == " \n") = False
                      | ((not (B.null bs)) && (B.last bs == fromIntegral (fromEnum '\n'))) = True
                      | otherwise                                            = False
            where
                length = B.length bs

handlePIStageMessage_ port bs = handlePIStageMessage port bs >> pure ()

handlePISingleByteMessage :: SerialPort -> Word8 -> IO Word8
handlePISingleByteMessage port byte =
    serialWriteAndReadUntilCustom port ((B.singleton byte)) satisfiedP >>= pure . B.head
    where
        satisfiedP :: ByteString -> Bool
        satisfiedP bs = not (B.null bs)

sendPIStageMessageNoResponse :: SerialPort -> ByteString -> IO ()
sendPIStageMessageNoResponse port bs =
    serialWrite port (bs <> "\n")

readPIStageError :: SerialPort -> IO Int
readPIStageError port = handlePIStageMessage port "ERR?" >>= pure . readB
