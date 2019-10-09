{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, NumDecimals #-}

module Equipment.Devices.RemoteStage where

import Control.Concurrent
import Control.Exception
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Equipment.Equipment
import Equipment.EquipmentTypes
import Utils.MiscUtils

import Debug.Trace as DT

data RemoteStage = RemoteStage {
                    rsName :: !EqName
                  , rsIPAddress :: !String
                  , rsPortNum :: !Int
                }

initializeRemoteStage :: EquipmentDescription -> IO EquipmentW
initializeRemoteStage (RemoteStageDesc name ip port) = return (EquipmentW $ RemoteStage (EqName name) ip port)

instance Equipment RemoteStage where
    equipmentName = rsName
    flushSerialPorts _ = pure ()
    closeDevice (RemoteStage _ ip port) = return ()
    hasMotorizedStage _ = True
    motorizedStageName _ = StageName "stage"
    supportedStageAxes _ = [XAxis, YAxis, ZAxis] -- TODO: Priorstage has psAxes
    getStagePosition rs = putStrLn "get stage position" >> handleRemoteStageRequest rs GetStagePosition >>= \(Position ps) -> putStrLn (show ps) >> return ps
    setStagePosition rs ps = putStrLn "set stage position" >> _handleRemoteStageRequest rs (SetStagePosition ps) -- TODO: wait until moving is done?

handleRemoteStageRequest :: RemoteStage -> RemoteStageRequest -> IO RemoteStageResponse
handleRemoteStageRequest (RemoteStage _ ip port) req = putStrLn ("handle remote stage request: " ++ (show req)) >>
    let serverParams = QueryServerParams ip port (floor 1e6) isCompleteJSONObject decodeJSONObject
        queryMsg     = (LB.toStrict . encode) req
    in  putStrLn (show queryMsg) >> queryServer serverParams queryMsg `catch` (\(e :: IOException) -> throw (userError "can't communicate with RemoteStage program")) >>= \response ->
        putStrLn (show response) >> case response of
            ErrorRemoteStageResponse err -> throwIO (userError ("error from RemoteStage: " ++ T.unpack err))
            resp                         -> return resp

_handleRemoteStageRequest :: RemoteStage -> RemoteStageRequest -> IO ()
_handleRemoteStageRequest r req = handleRemoteStageRequest r req >> return ()

data RemoteStageRequest = GetStagePosition
                        | SetStagePosition !StagePosition
                        -- | IsMovingRequest
                        deriving (Show)

data RemoteStageResponse = OKRemoteStageResponse
                         | ErrorRemoteStageResponse !Text
                         | Position !StagePosition
                         --m| IsMoving !Bool
                         deriving (Show)

instance ToJSON RemoteStageRequest where
  toJSON GetStagePosition = object ["type" .= ("getposition" :: Text)]
  toJSON (SetStagePosition sp) = object ["type" .= ("setposition" :: Text), "position" .= sp]
  -- toJSON IsMovingRequest = object ["type" .= ("ismoving" :: Text)]

instance FromJSON RemoteStageResponse where
  parseJSON (Object v) =
      v .: "type" >>= \(typ :: Text) ->
      case typ of
        "status"      -> v .: "status" >>= \(stat :: Text) ->
                          case stat of
                              "ok"    -> return OKRemoteStageResponse
                              "error" -> ErrorRemoteStageResponse <$> v .: "error"
                              _       -> fail "unknown status"
        "position" -> Position <$> v .: "stageposition"
        -- "ismoving" -> IsMoving <$> v .: "status"
        _             -> fail "can't decode remote stage response type"
  parseJSON _ = fail "can't decode remote stage response"
