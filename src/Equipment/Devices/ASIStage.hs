{-# LANGUAGE OverloadedStrings #-}

module Equipment.Devices.ASIStage where

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

data ASIStage = ASIStage {
                      asiName :: !EqName
                    , asiPort :: !SerialPort
                  }

initializeASIStage :: EquipmentDescription -> IO EquipmentW
initializeASIStage (ASIStageDesc name portName) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS115200}) (TimeoutMillis 3000) SerialPortDebugText
    in  openSerialPort portName serialSettings >>= \port ->
        verifyIsASIStage port >>
        mapM_ (\s -> sendASICommand port s) ["UM X=10000", "UM Z=10000", "UM Z=10000"] >>   -- all positions expressed in units of 0.1 µm
        pure (EquipmentW (ASIStage (EqName name) port))

instance Equipment ASIStage where
    equipmentName = asiName
    flushSerialPorts p = flushSerialPort (asiPort p)
    closeDevice p = closeSerialPort (asiPort p)
    hasMotorizedStage _ = True
    motorizedStageName _ = StageName "stage"
    supportedStageAxes _ = [XAxis, YAxis, ZAxis]
    getStagePosition (ASIStage _ port ) =
        StagePosition <$> ((/ 10) <$> readNumberP port "WHERE X")  -- divide by 10 because positions are in units of 0.1 µm
                      <*> ((/ 10) <$> readNumberP port "WHERE Y")
                      <*> ((/ 10) <$> readNumberP port "WHERE X")
                      <*> pure False <*> pure 0
        where
          readNumberP :: SerialPort -> ByteString -> IO Double
          readNumberP port query = handleASIMessage port query >>= \resp ->
                                   case (B.stripPrefix "A: " resp) of
                                       Nothing -> throwIO (userError "unable to read ASI position")
                                       Just r  -> (return . read . T.unpack . T.decodeUtf8) r
                                   
    setStagePosition (ASIStage _ port) (StagePosition x y z _ _) =
        doMove `onException` abortMovement
        where
          doMove = sendASICommand port posMsg >> waitUntilMovementStops
          posMsg = T.encodeUtf8 . LT.toStrict $ T.format "MOVE X {} Y {} Z {}" ((round (x * 10), round (y * 10), round (z * 10)) :: (Int, Int, Int))
          waitUntilMovementStops = isMoving . read . T.unpack . T.decodeUtf8 <$> handleASIMessage port "/" >>= \moves ->
                                   if (moves)
                                   then threadDelay 20000 >> waitUntilMovementStops
                                   else pure ()
          isMoving :: ByteString -> Bool
          isMoving b | b == "N"  = False
                     | b == "B"  = True
                     | otherwise = throw (userError "invalid response from ASI stage")
          abortMovement = handleASIMessage port "\\" >> pure ()

handleASIMessage :: SerialPort -> ByteString -> IO ByteString
handleASIMessage port bs = serialWriteAndReadUntilChar port (bs <> "\r") '\n' >>=
                           return . B.init . B.init -- drop the trailing CR LF

verifyIsASIStage :: SerialPort -> IO ()
verifyIsASIStage port = handleASIMessage port "BUILD" >>= \resp ->
                       when (resp /= "TIGER_COMM") (throwIO (userError "does not seem to be an ASI stage")) >>
                       pure ()

sendASICommand :: SerialPort -> ByteString -> IO ()
sendASICommand port bs = handleASIMessage port bs >>= \resp ->
                           case resp of
                               ":A" -> return ()
                               e     -> throwIO (userError ("unexpected response " ++ show e ++ " from ASI stage"))
