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
import qualified System.Timeout as ST
import Text.Printf

import EquipmentTypes
import MiscUtils
import RCSerialPort

type StagePosition = (Double, Double, Double)

motorizedStageName :: Equipment -> Text
motorizedStageName (PriorStage name _) = name
motorizedStageName (DummyStage name) = name

getStagePosition :: Equipment -> IO StagePosition
getStagePosition = getStagePosition'
    where
      getStagePosition' :: Equipment -> IO StagePosition
      getStagePosition' (DummyStage n) = putStrLn ("read position of " ++ T.unpack n) >> return (0.0, 0.0, 0.0)
      getStagePosition' (PriorStage _ portVar) = withMVar portVar $ \port ->
          (,,) <$> readNumberP port "PX\r" <*> readNumberP port "PY\r" <*> readNumberP port "PZ\r"
          where
            readNumberP :: SerialPort -> ByteString -> IO Double
            readNumberP port query = flushSerialPort port >> serialWrite port query >>
                                     serialReadUntilChar port '\r' >>=
                                     return . read . T.unpack . T.decodeUtf8

lookupStage :: [Equipment] -> Text -> Equipment
lookupStage mss name = case eligibleStages of
                        [s] -> s
                        []  -> throw (userError ("no stage named " ++ (T.unpack name)))
                        _   -> throw (userError ("more than one stage with the same name"))
    where
      eligibleStages = filter ((== name) . motorizedStageName) mss

setStagePositionLookup :: [Equipment] -> Text -> StagePosition -> IO ()
setStagePositionLookup mss name ds =
    let s = lookupStage mss name
    in  setStagePosition s ds

setStagePosition :: Equipment -> StagePosition -> IO ()
setStagePosition p pos =
    ST.timeout 20e6 (setStagePosition' p pos) >>= \result ->
    case result of
      Nothing -> throwIO (userError "timeout communicating to stage")
      Just v  -> return v
    where
        setStagePosition' (DummyStage n) ds =
            putStrLn ("set position of " ++ T.unpack n ++ " to " ++ show ds)
        setStagePosition' (PriorStage _ portVar) (x, y, z) =
            (withMVar portVar $ \port ->
                flushSerialPort port >> serialWrite port posStr >> serialReadUntilChar port '\r') >>= \resp ->
            case resp of
              "R\r" -> return ()
              _     -> throwIO (userError "unexpected response from stage")
            where
              posStr = T.encodeUtf8 . T.pack $ printf "G %d, %d, %d\r" (round x :: Int) (round y :: Int) (round z :: Int)
