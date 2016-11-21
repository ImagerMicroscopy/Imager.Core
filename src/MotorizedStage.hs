{-# LANGUAGE BangPatterns, OverloadedStrings, NumDecimals #-}
module MotorizedStage where

import Control.Concurrent.MVar
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
                        , psPort :: !(MVar SerialPort)
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
            putStrLn "Connecting to Prior stage..." >>
            timeout 2e6 (
              openSerial portName (defaultSerialSettings {commSpeed = CS9600}) >>= \port ->
              send port "COMP 1\r" >> readFromSerialUntilChar port '\r' >>= \resp ->
              if ((resp /= "0\r") && (resp /= "R\r"))
              then error "unexpected reply from prior stage"
              else PriorStage name <$> newMVar port) >>= \result ->
            case result of
              Nothing -> error "timeout connecting to the prior stage"
              Just r -> putStrLn "Connected to Prior stage" >> return r
        openMotorizedStage (DummyStageDesc name) =
            putStrLn ("dummy stage " ++ (T.unpack name) ++ " open") >>
            return (DummyStage name)

closeMotorizedStages :: [MotorizedStage] -> IO ()
closeMotorizedStages = mapM_ closeMotorizedStage'
    where
      closeMotorizedStage' (PriorStage _ portVar) = withMVar portVar $ (\port -> closeSerial port)
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
                        Nothing -> return (Left "timeout getting stage position")
                        Just v  -> return (Right v)
    where
      getStagePosition' :: MotorizedStage -> IO (Double, Double, Double)
      getStagePosition' (DummyStage n) = putStrLn ("read position of " ++ T.unpack n) >> return (0.0, 0.0, 0.0)
      getStagePosition' (PriorStage _ portVar) =
          (,,) <$> readNumberP "PX\r" <*> readNumberP "PY\r" <*> readNumberP "PZ\r"
          where
            readNumberP :: ByteString -> IO Double
            readNumberP query = withMVar portVar $ \port ->
                                  flush port >> send port query >>
                                  readFromSerialUntilChar port '\r' >>= return . read . T.unpack . T.decodeUtf8

setStagePositionLookup :: [MotorizedStage] -> Text -> (Double, Double, Double) -> IO (Either String ())
setStagePositionLookup mss name ds =
    case eligibleStages of
      [s] -> setStagePosition s ds
      []  -> return (Left ("no stage named " ++ (T.unpack name)))
      _   -> return (Left ("more than one stage with the same name"))
    where
      eligibleStages = filter ((== name) . motorizedStageName) mss

setStagePosition :: MotorizedStage -> (Double, Double, Double) -> IO (Either String ())
setStagePosition p pos =
    timeout 20e6 (setStagePosition' p pos) >>= \result ->
    case result of
      Nothing -> return (Left "timeout communicating to stage")
      Just v  -> return v
    where
        setStagePosition' (DummyStage n) ds =
            putStrLn ("set position of " ++ T.unpack n ++ " to " ++ show ds) >>
            return (Right ())
        setStagePosition' (PriorStage _ portVar) (x, y, z) =
            (withMVar portVar $ \port ->
                flush port >> send port posStr >> readFromSerialUntilChar port '\r') >>= \resp ->
            case resp of
              "R\r" -> return (Right ())
              _     -> return (Left "unexpected response from stage")
            where
              posStr = T.encodeUtf8 . T.pack $ printf "G %d, %d, %d\r" (round x :: Int) (round y :: Int) (round z :: Int)
