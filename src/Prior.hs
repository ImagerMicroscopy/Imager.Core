{-# LANGUAGE OverloadedStrings #-}
module Prior where

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
import Equipment

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

import Equipment
import EquipmentTypes
import MiscUtils
import RCSerialPort

data PriorStage = PriorStage !Text !(MVar SerialPort)

initializePriorStage :: EquipmentDescription -> IO EquipmentW
initializePriorStage (PriorDesc name portName) =
    let serialSettings = RCSerialPortSettings (defaultSerialSettings {commSpeed = CS9600}) (TimeoutMillis 30000) SerialPortNoDebug
    in  openSerialPort portName serialSettings >>= \port ->
        serialWrite port "COMP 1\r" >> serialReadUntilChar port '\r' >>= \resp ->
        if ((resp /= "0\r") && (resp /= "R\r"))
        then throwIO (userError "unexpected reply from prior stage")
        else putStrLn "Connected to Prior stage" >>
             EquipmentW <$> (PriorStage name <$> newMVar port)

instance Equipment PriorStage where
    equipmentName _ = (EqName "Prior stage")
    closeDevice (PriorStage _ portVar) = withMVar portVar $ (\port -> closeSerialPort port)
    hasMotorizedStage _ = True
    motorizedStageName (PriorStage n _) = n
    getStagePosition (PriorStage _ portVar) = withMVar portVar $ \port ->
        (,,) <$> readNumberP port "PX\r" <*> readNumberP port "PY\r" <*> ((/10) <$> readNumberP port "PZ\r")
        where
          readNumberP :: SerialPort -> ByteString -> IO Double
          readNumberP port query = flushSerialPort port >> serialWrite port query >>
                                   serialReadUntilChar port '\r' >>=
                                   return . read . T.unpack . T.decodeUtf8
    setStagePosition (PriorStage _ portVar) (x, y, z) =
        (withMVar portVar $ \port ->
            flushSerialPort port >> serialWrite port posStr >> serialReadUntilChar port '\r') >>= \resp ->
        case resp of
          "R\r" -> return ()
          _     -> throwIO (userError "unexpected response from stage")
        where
          posStr = T.encodeUtf8 . T.pack $ printf "G %d, %d, %d\r" (round x :: Int) (round y :: Int) (round (z * 10) :: Int)
