{-# LANGUAGE BangPatterns, OverloadedStrings, NumDecimals #-}
module MotorizedStage where

import Control.Exception
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Environment
import System.FilePath
import System.Hardware.Serialport hiding(timeout)
import System.Timeout
import Text.Printf

import MiscUtils

data MotorizedStageDesc = PriorDesc {
                              psDescName :: !Text
                            , psDescPortName :: !String
                          }
                        | DummyStageDesc {
                              dsName :: !Text
                          }
                        deriving (Show, Read)

data MotorizedStage = PriorStage {
                          psName :: !Text
                        , psPort :: !SerialPort
                      }
                    | DummyStage !Text

instance ToJSON MotorizedStage where
  toJSON s = object ["name" .= motorizedStageName s]

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

motorizedStageName :: MotorizedStage -> Text
motorizedStageName (PriorStage name _) = name
motorizedStageName (DummyStage name) = name

openMotorizedStages :: [MotorizedStageDesc] -> IO [MotorizedStage]
openMotorizedStages = mapM openMotorizedStage
    where
        openMotorizedStage (PriorDesc name portName) =
            timeout 2e6 (
              openSerial portName (defaultSerialSettings {commSpeed = CS9600}) >>= \port ->
              send port "COMP 1\r" >> readFromSerialUntilChar port '\r' >>= \resp ->
              if ((resp /= "0\r") && (resp /= "R\r"))
              then error "unexpected reply from prior stage"
              else return (PriorStage name port)) >>= \result ->
            case result of
              Nothing -> error "timeout connecting to the prior stage"
              Just r -> return r
        openMotorizedStage (DummyStageDesc name) =
            putStrLn ("dummy stage " ++ (T.unpack name) ++ " open") >>
            return (DummyStage name)

closeMotorizedStages :: [MotorizedStage] -> IO ()
closeMotorizedStages = mapM_ closeMotorizedStage'
    where
      closeMotorizedStage' (PriorStage _ port) = closeSerial port
      closeMotorizedStage' (DummyStage n) = putStrLn ("dummy stage " ++ (T.unpack n) ++ " closed")

getStagePositionLookup :: [MotorizedStage] -> Text -> IO (Either String (Double, Double, Double))
getStagePositionLookup mss name =
    case eligibleStages of
      [s] -> getStagePosition s
      []  -> return (Left ("no stage named " ++ (T.unpack name)))
      _   -> return (Left ("more than one stage with the same name"))
    where
      eligibleStages = filter ((== name) . motorizedStageName) mss

getStagePosition :: MotorizedStage -> IO (Either String (Double, Double, Double))
getStagePosition s = timeout 20e6 (getStagePosition' s) >>= \result ->
                     case result of
                        Nothing -> error "timeout getting stage position"
                        Just v  -> return (Right v)
    where
      getStagePosition' :: MotorizedStage -> IO (Double, Double, Double)
      getStagePosition' (PriorStage _ port) =
          (,,) <$> readNumberP "PX\r" <*> readNumberP "PY\r" <*> readNumberP "PZ\r"
          where
            readNumberP :: ByteString -> IO Double
            readNumberP query = flush port >> send port query >>
                                readFromSerialUntilChar port '\n' >>= return . read . T.unpack . T.decodeUtf8

setStagePosition :: MotorizedStage -> (Double, Double, Double) -> IO (Either String ())
setStagePosition p pos =
    timeout 20e6 (setStagePosition' p pos) >>= \result ->
    case result of
      Nothing -> return (Left "timeout communicating to stage")
      Just v  -> return v
    where
        setStagePosition' (PriorStage _ port) (x, y, z) =
            flush port >> send port posStr >> readFromSerialUntilChar port '\r' >>= \resp ->
            case resp of
              "R\r" -> return (Right ())
              _     -> return (Left "unexpected response from stage")
            where
              posStr = T.encodeUtf8 . T.pack $ printf "G %d, %d, %d\r" x y z
