{-# LANGUAGE BangPatterns, OverloadedStrings, NumDecimals #-}
module MotorizedStage where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Environment
import System.FilePath
import System.Hardware.Serialport hiding(timeout)
import System.Timeout

import MiscUtils

data MotorizedStageDesc = PriorDesc {
                              psDescName :: !Text
                            , psDescPortName :: !String
                          }

data MotorizedStage = PriorStage {
                          psName :: !Text
                        , psPort :: !SerialPort
                      }

readAvailableMotorizedStages :: IO [MotorizedStageDesc]
readAvailableMotorizedStages =
  getExecutablePath >>= \exePath ->
  readFile (takeDirectory exePath </> confFilename) >>=
  return . read
  where
    confFilename = "motorizedstages.txt"

withMotorizedStages :: [MotorizedStageDesc] -> ([MotorizedStage] -> IO a) -> IO a
withMotorizedStages descs action =
    bracket (openMotorizedStages descs) closeMotorizedStages action

openMotorizedStages :: [MotorizedStageDesc] -> IO [MotorizedStage]
openMotorizedStages = mapM openMotorizedStage
    where
        openMotorizedStage (PriorDesc name portName) =
            timeout 2e6 (
              openSerial portName (defaultSerialSettings {commSpeed = CS9600}) >>= \port ->
              send port "COMP 1\r" >> readFromSerialUntilChar '\r' >>= \resp ->
              if ((resp /= "0\r") && (resp /= "R\r"))
              then error "unexpected reply from prior stage"
              else return (PriorStage name port)) >>= \result ->
            case result of
              Nothing -> error "timeout connecting to the prior stage"
              Just r -> return r

closeMotorizedStages :: [MotorizedStageDesc] -> IO ()
openMotorizedStages = mapM_ closeMotorizedStage
    where
      closeMotorizedStage (PriorStage _ port) = closeSerial port

getStagePosition :: MotorizedStage -> IO (Double, Double, Double)
getStagePosition (PriorStage _ port) =
    (,,) <$> readNumberP "PX\r" <*> readNumberP "PY\r" <*> readNumberP "PZ\r"
    where
      readNumberP :: SerialPort -> ByteString -> IO Double
      readNumberP port query = flush port >> send port query >>
                               readFromSerialUntilChar '\n' >>= return . read

setStagePosition :: MotorizedStage -> (Double, Double, Double) -> IO (Either String ())
setStagePosition p pos =
    timeout 20e6 (setStagePosition' p pos) >>= \result ->
    case result of
      Nothing -> return (Left "timeout communicating to stage")
      Just v  -> return v
    where
        setStagePosition' (PriorStage _ port) (x, y, z) =
            flush port >> send port posStr >> readFromSerialUntilChar '\r' >>= \resp ->
            case resp of
              "R\r" -> return (Right ())
              _     -> return (Left "unexpected response from stage")
            where
              posStr = T.encodeUtf8 . T.pack $ printf "G %d, %d, %d\r" x y z
