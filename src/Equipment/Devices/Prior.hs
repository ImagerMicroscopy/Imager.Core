{-# LANGUAGE OverloadedStrings #-}

module Equipment.Devices.Prior where

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

data PriorStage = PriorStage {
                      psName :: !EqName
                    , psPort :: !SerialPort
                    , psAxes :: ![StageAxis]
                  }

initializePriorStage :: EquipmentDescription -> IO EquipmentW
initializePriorStage (PriorDesc name portName) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS9600}) (TimeoutMillis 30000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port ->
        handlePriorMessage port "COMP 0" >>= \resp ->
        when (resp /= "0\r") (throwIO (userError "unexpected reply from Prior stage")) >>
        determineAvailableAxes port >>= \axes ->
        pure (EquipmentW (PriorStage (EqName name) port axes))
    where
        determineAvailableAxes port =
            B.split (fromIntegral $ fromEnum '\r') <$> serialWriteAndReadUntilSequence port "?\r" "END\r" >>= \responses ->
            if ("FOCUS = NONE" `elem` responses)
            then pure [XAxis, YAxis]
            else pure [XAxis, YAxis, ZAxis]

instance Equipment PriorStage where
    equipmentName = psName
    flushSerialPorts p = flushSerialPort (psPort p)
    closeDevice p = closeSerialPort (psPort p)
    hasMotorizedStage _ = True
    motorizedStageName _ = StageName "stage"
    supportedStageAxes = psAxes
    getStagePosition (PriorStage _ port _) =
        StagePosition <$> readNumberP port "PX"
                      <*> readNumberP port "PY"
                      <*> ((/10) <$> readNumberP port "PZ")
                      <*> pure False <*> pure 0
        where
          readNumberP :: SerialPort -> ByteString -> IO Double
          readNumberP port query = handlePriorMessage port query >>=
                                   return . read . T.unpack . T.decodeUtf8
    setStagePosition (PriorStage _ port _) (StagePosition x y z _ _) =
        doMove `onException` abortMovement
        where
          doMove = sendPriorCommand port posMsg >> waitUntilMovementStops
          posMsg = T.encodeUtf8 . LT.toStrict $ T.format "G {}, {}, {}" ((round x, round y, round (z * 10)) :: (Int, Int, Int))
          waitUntilMovementStops = isMoving . read . T.unpack . T.decodeUtf8 <$> handlePriorMessage port "$" >>= \moves ->
                                   if (moves)
                                   then threadDelay 20000 >> waitUntilMovementStops
                                   else pure ()
          isMoving :: Int -> Bool
          isMoving a = any ((/=) 0) [a .&. (2^0), a .&. (2^1), a .&. (2^2)]
          abortMovement = sendPriorCommand port "I"

handlePriorMessage :: SerialPort -> ByteString -> IO ByteString
handlePriorMessage port bs = serialWriteAndReadUntilChar port (bs <> "\r") '\r'

sendPriorCommand :: SerialPort -> ByteString -> IO ()
sendPriorCommand port bs = handlePriorMessage port bs >>= \resp ->
                           case resp of
                               "R\r" -> return ()
                               e     -> throwIO (userError ("unexpected response " ++ show e ++ " from Prior stage"))
