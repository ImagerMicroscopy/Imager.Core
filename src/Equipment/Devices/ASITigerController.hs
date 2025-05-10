{-# LANGUAGE OverloadedStrings, NumDecimals #-}

module Equipment.Devices.ASITigerController where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List
import Data.Maybe
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

data ASITigerController = ASITigerController {
                      asiName :: !EqName
                    , asiFilters :: ![(Text, Int)]
                    , asiPort :: !SerialPort
                  }

initializeASITigerController :: EquipmentDescription -> IO EquipmentW
initializeASITigerController (ASITigerControllerDesc name filters portName) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS115200}) (TimeoutMillis 3000) SerialPortDebugText
    in  openSerialPort portName serialSettings >>= \port ->
        verifyIsASIStage port >>
        mapM_ (\s -> sendASICommand port s) ["UM X=10000", "UM Z=10000", "UM Z=10000"] >>   -- all positions expressed in units of 0.1 µm
        pure (EquipmentW (ASITigerController (EqName name) filters port))

instance Equipment ASITigerController where
    equipmentName = asiName
    flushSerialPorts p = flushSerialPort (asiPort p)
    closeDevice p = closeSerialPort (asiPort p)

    availableMovableComponents tigc@(ASITigerController{}) = if (null tigc.asiFilters)
                                                             then []
                                                             else [DiscreteMovableComponent "FW" (map fst tigc.asiFilters)]
    moveComponent tigc@(ASITigerController{}) [DiscreteComponentSetting "FW" fName] =
        when (null tigc.asiFilters) (throwIO $ userError "ASI Tiger moving component but not filters") >>
        let filterIdx = fromJust (lookup fName tigc.asiFilters)
            port = tigc.asiPort
            msg = formatBS "MP {}\r\n" (T.Only filterIdx)
        in  ST.timeout 10e6 (serialWriteAndReadUntilChar port msg '\n' >> waitUntilMovementStops) >>= \resp ->
            case resp of
                Just _  -> pure ()
                Nothing -> throwIO $ userError "timeout communicating with ASI filter wheel"
        where
            waitUntilMovementStops = serialWriteByteAndReadByte tigc.asiPort (fromIntegral $ fromEnum '?') >>= \resp ->
                                     case (toEnum $ fromIntegral resp) of
                                        '0' -> pure ()
                                        _   -> waitUntilMovementStops

    hasMotorizedStage _ = True
    motorizedStageName _ = StageName "stage"
    supportedStageAxes _ = [XAxis, YAxis, ZAxis]
    getStagePosition (ASITigerController _ _ port ) =
        StagePosition <$> ((/ 10) <$> readNumberP port "WHERE X")  -- divide by 10 because positions are in units of 0.1 µm
                      <*> ((/ 10) <$> readNumberP port "WHERE Y")
                      <*> ((/ 10) <$> readNumberP port "WHERE Z")
                      <*> pure False <*> pure 0
        where
          readNumberP :: SerialPort -> ByteString -> IO Double
          readNumberP port query = handleASIMessage port query >>= \resp ->
                                   case (B.stripPrefix ":A " resp) of
                                       Nothing -> throwIO (userError "unable to read ASI position")
                                       Just r  -> -- the ASI stage sometimes just returns ":A " without a position
                                                  -- so detect that and try again until we get a number
                                                  let str = T.unpack . T.decodeUtf8 $ r
                                                  in  if (any (`elem` ['0' .. '9']) str)
                                                      then pure (read str)
                                                      else readNumberP port query
                                   
    setStagePosition (ASITigerController _ _ port) (StagePosition x y z _ _) =
        doMove `onException` abortMovement
        where
          doMove = sendASICommand port posMsg >> waitUntilMovementStops
          posMsg = T.encodeUtf8 . LT.toStrict $ T.format "MOVE X={} Y={} Z={}" ((round (x * 10), round (y * 10), round (z * 10)) :: (Int, Int, Int))
          waitUntilMovementStops = isMoving <$> handleASIMessage port "/" >>= \moves ->
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
sendASICommand port bs = dropSpaces <$> handleASIMessage port bs >>= \resp ->
                           case resp of
                               ":A" -> return ()
                               e     -> throwIO (userError ("unexpected response " ++ show e ++ " from ASI stage"))
    where
        dropSpaces b = B.filter (\c -> c /= 32) b
