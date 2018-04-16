{-# LANGUAGE OverloadedStrings #-}
module Prior where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
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

import Equipment
import EquipmentTypes
import MiscUtils
import RCSerialPort

data PriorStage = PriorStage !EqName !SerialPort

initializePriorStage :: EquipmentDescription -> IO EquipmentW
initializePriorStage (PriorDesc name portName) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS9600}) (TimeoutMillis 30000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port ->
        handlePriorMessage port "COMP 0" >>= \resp ->
        when (resp /= "0\r") (throwIO (userError "unexpected reply from Prior stage")) >>
        pure (EquipmentW (PriorStage (EqName name) port))

instance Equipment PriorStage where
    equipmentName (PriorStage n _) = n
    flushSerialPorts (PriorStage _ port) = flushSerialPort port
    closeDevice (PriorStage _ port) = closeSerialPort port
    hasMotorizedStage _ = True
    motorizedStageName (PriorStage n _) = StageName "stage"
    supportedStageAxes _ = [XAxis, yAxis, zAxis]
    getStagePosition (PriorStage _ port) = (,,) <$> readNumberP port "PX"
                                                <*> readNumberP port "PY"
                                                <*> ((/10) <$> readNumberP port "PZ")
        where
          readNumberP :: SerialPort -> ByteString -> IO Double
          readNumberP port query = handlePriorMessage port query >>=
                                   return . read . T.unpack . T.decodeUtf8
    setStagePosition (PriorStage _ port) (x, y, z) =
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
