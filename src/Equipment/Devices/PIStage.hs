{-# LANGUAGE OverloadedStrings #-}

module Equipment.Devices.PIStage where

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

initializePriorStage :: EquipmentDescription -> IO EquipmentW
initializePriorStage (PIStageDesc name portName) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS115200}) (TimeoutMillis 30000) SerialPortDebugText
    in  openSerialPort portName serialSettings >>= \port ->
        isPIStage port >>= \isStage ->
        when (not isStage) (
            throwIO userError "not a PI stage"
        )
        determineAvailableAxes port >>= \axes ->
        pure (EquipmentW (PIStage (EqName name) port axes))
    where
        determineAvailableAxes port =
            handlePIStageMessage port "SAI?" >>= \response ->
            pure [second ax | ax <- [(1, XAxis), (2, YAxis), (3, ZAxis)], (first ax `elem` response)]
        isPIStage :: IO Bool
        isPIStage port = 
            handlePIStageMessage port "*IDN?" >>= \resp ->
            pure ("Physik" `B.isInfixOf` resp)

instance Equipment PriorStage where
    equipmentName = psName
    flushSerialPorts p = flushSerialPort (pisPort p)
    closeDevice p = closeSerialPort (pisPort p)
    hasMotorizedStage _ = True
    motorizedStageName _ = StageName "stage"
    supportedStageAxes = pisAxes
    getStagePosition (PriorStage _ port axes) =
        StagePosition <$> (if (XAxis `elem` axes) then posOfAxis 1 else -1)
                      <*> (if (YAxis `elem` axes) then posOfAxis 2 else -1)
                      <*> (if (ZAxis `elem` axes) then posOfAxis 3 else -1)
                      <*> pure False <*> pure 0
        where
          posOfAxis :: Int -> IO Double
          posOfAxis idx =
              handlePIStageMessage port (formatBS "POS? {}" (T.Only idx)) >>= \response ->
              (pure . read . B.toString) (B.drop 2 response))
          readNumberP :: SerialPort -> ByteString -> IO Double
          readNumberP port query = handlePriorMessage port query >>=
                                   return . read . T.unpack . T.decodeUtf8
    setStagePosition (PriorStage _ port _) (StagePosition x y z _ _) =
        doMove `onException` abortMovement
        where
          doMove = sendPriorCommand port posMsg >> waitUntilMovementStops
          axisPosMsg :: Int -> Double -> ByteString
          axisPosMsg idx pos = formatBS "MOV {} {} " (idx, pos)
          msgs = map f axes
              where
                  f XAxis = axisPosMsg 1 x
                  f YAxis = axisPosMsg 2 y
                  f ZAxis = axisPosMsg 3 z
          posMsg = T.encodeUtf8 . LT.toStrict $ T.format "G {}, {}, {}" ((round x, round y, round (z * 10)) :: (Int, Int, Int))
          waitUntilMovementStops = isMoving . read . T.unpack . T.decodeUtf8 <$> handlePriorMessage port "$" >>= \moves ->
                                   if (moves)
                                   then threadDelay 20000 >> waitUntilMovementStops
                                   else pure ()
          isMoving :: Int -> Bool
          isMoving a = any ((/=) 0) [a .&. (2^0), a .&. (2^1), a .&. (2^2)]
          abortMovement = sendPriorCommand port "I"

handlePIStageMessage :: SerialPort -> ByteString -> IO ByteString
handlePIStageMessage port bs = 
    serialReadAndWriteUntilCustom port (bs <> "\n") satisfiedP
    where
        -- PI says: if multiline response then non-last lines terminated by " \n"
        satisfiedP :: ByteString -> Bool
        satisfiedP bs | (length >= 2) && (B.drop (length - 2) bs == " \n" = False
                      | B.last bs == '\n'                                 = True
                      | otherwise                                         = False
        where
            length = B.length bs

sendPIStageCommand :: SerialPort -> ByteString -> IO ()
sendPIStageCommand port bs = handlePIStageMessage port bs >>= \resp ->
                            case resp of
                                "R\r" -> return ()
                                e     -> throwIO (userError ("unexpected response " ++ show e ++ " from Prior stage"))
