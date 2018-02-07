{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, NumDecimals #-}
module Robottor where

import Control.Concurrent
import Control.Exception
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Equipment
import EquipmentTypes
import MiscUtils

data Robottor = Robottor {
                    roName :: !Text
                  , roIPAddress :: !String
                  , roPortNum :: !Int
                }

initializeRobottor :: EquipmentDescription -> IO EquipmentW
initializeRobottor (RobottorDesc name ip port) = return (EquipmentW $ Robottor name ip port)

instance Equipment Robottor where
    equipmentName _ = (EqName "Robottor")
    closeDevice (Robottor _ ip port) = return ()
    hasRobot _ = True
    robotName (Robottor n _ _) = n
    listRobotPrograms r =
        handleRobottorRequest r ListRobottorPrograms >>= \(RobottorProgramListResponse ps) ->
        return ps
    robotAcceptsExternalCommands r =
        handleRobottorRequest r AllowsProgramExecution >>= \(AllowsProgramExecutionResponse b) -> return b
    executeRobotProgram r progName waitForCompletion =
        executeRobotProgram' `onException` (abortRobotProgramExecution r)
        where
            executeRobotProgram' =
                _handleRobottorRequest r (ExecuteRobottorProgram progName) >>
                if (not waitForCompletion)
                then return ()
                else waitUntilDone
            waitUntilDone = robotIsExecuting r >>= \isExecuting ->
                            if (not isExecuting)
                            then return ()
                            else threadDelay 250e3 >> waitUntilDone
    abortRobotProgramExecution r = _handleRobottorRequest r AbortProgramExecution

robotIsExecuting :: Robottor -> IO Bool
robotIsExecuting r =
    handleRobottorRequest r IsExecuting >>= \(IsExecutingResponse b) -> return b

handleRobottorRequest :: Robottor -> RobottorRequest -> IO RobottorResponse
handleRobottorRequest (Robottor _ ip port) req =
    let serverParams = QueryServerParams ip port (floor 1e6) isCompleteJSONObject decodeJSONObject
        queryMsg = (LB.toStrict . encode) req
    in  queryServer serverParams queryMsg `catch` (\(e :: IOException) -> throw (userError "can't communicate with Robottor program")) >>= \response ->
        case response of
            ErrorRobottorResponse err -> throwIO (userError ("error from robottor: " ++ T.unpack err))
            resp                      -> return resp

_handleRobottorRequest :: Robottor -> RobottorRequest -> IO ()
_handleRobottorRequest r req = handleRobottorRequest r req >> return ()

data RobottorRequest = ListRobottorPrograms
                     | ExecuteRobottorProgram !Text
                     | AbortProgramExecution
                     | AllowsProgramExecution
                     | IsExecuting
                     deriving (Show)

data RobottorResponse = OKRobottorResponse
                      | ErrorRobottorResponse !Text
                      | RobottorProgramListResponse ![Text]
                      | AllowsProgramExecutionResponse !Bool
                      | IsExecutingResponse !Bool
                      deriving (Show)

instance ToJSON RobottorRequest where
  toJSON ListRobottorPrograms = object ["type" .= ("listprograms" :: Text)]
  toJSON (ExecuteRobottorProgram prog) = object ["type" .= ("executeprogram" :: Text), "programname" .= prog]
  toJSON AbortProgramExecution = object ["type" .= ("abortprogramexecution" :: Text)]
  toJSON AllowsProgramExecution = object ["type" .= ("acceptingexternalcommands" :: Text)]
  toJSON IsExecuting = object ["type" .= ("isexecuting" :: Text)]

instance FromJSON RobottorResponse where
  parseJSON (Object v) =
      v .: "type" >>= \(typ :: Text) ->
      case typ of
        "status"      -> v .: "status" >>= \(stat :: Text) ->
                          case stat of
                              "ok"    -> return OKRobottorResponse
                              "error" -> ErrorRobottorResponse <$> v .: "error"
                              _       -> fail "unknown status"
        "programlist" -> RobottorProgramListResponse <$> v .: "programnames"
        "acceptingexternalcommands" -> AllowsProgramExecutionResponse <$> v .: "status"
        "isexecuting" -> IsExecutingResponse <$> v .: "status"
        _             -> fail "can't decode robottor response type"
  parseJSON _ = fail "can't decode robottor response"
