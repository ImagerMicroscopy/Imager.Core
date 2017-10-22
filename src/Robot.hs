{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables, NumDecimals #-}
module Robot where

import Control.Concurrent
import Control.Exception
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Environment
import System.FilePath
import System.Hardware.Serialport
import System.IO

import EquipmentTypes
import MiscUtils

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

robotName :: Equipment -> Text
robotName (Robottor name _ _) = name

availableRobotsAndPrograms :: [Equipment] -> IO [(Text, [Text])]
availableRobotsAndPrograms rss =
    let robotNames = map robotName rss
    in  zip robotNames <$> mapM listRobotPrograms rss

lookupRobot :: [Equipment] -> Text -> Maybe Equipment
lookupRobot mrs name = case (filter ((==) name . robotName) mrs) of
                           []      -> Nothing
                           (m : _) -> Just m
lookupRobotThrows :: [Equipment] -> Text -> Equipment
lookupRobotThrows rs n = case lookupRobot rs n of
                             Just r  -> r
                             Nothing -> throw (userError ("no robot " ++ T.unpack n))

isKnownRobot :: [Equipment] -> Text -> Bool
isKnownRobot mrs name = isJust (lookupRobot mrs name)

listRobotPrograms :: Equipment -> IO [Text]
listRobotPrograms r =
    handleRobottorRequest r ListRobottorPrograms >>= \(RobottorProgramListResponse ps) ->
    return ps

executeRobotProgram :: Equipment -> Text -> Bool -> IO ()
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

abortRobotProgramExecution :: Equipment -> IO ()
abortRobotProgramExecution r = _handleRobottorRequest r AbortProgramExecution

robotAcceptsExternalCommands :: Equipment -> IO Bool
robotAcceptsExternalCommands r =
    putStrLn "testing ext" >>
    handleRobottorRequest r AllowsProgramExecution >>= \(AllowsProgramExecutionResponse b) -> return b

robotIsExecuting :: Equipment -> IO Bool
robotIsExecuting r =
    handleRobottorRequest r IsExecuting >>= \(IsExecutingResponse b) -> return b

handleRobottorRequest :: Equipment -> RobottorRequest -> IO RobottorResponse
handleRobottorRequest (Robottor _ ip port) req =
    let serverParams = QueryServerParams ip port (floor 1e6) isCompleteJSONObject decodeJSONObject
        queryMsg = (LB.toStrict . encode) req
    in  queryServer serverParams queryMsg `catch` (\(e :: IOException) -> throw (userError "can't communicate with Robottor program")) >>= \response ->
        case response of
            ErrorRobottorResponse err -> throwIO (userError ("error from robottor: " ++ T.unpack err))
            resp                      -> return resp

_handleRobottorRequest :: Equipment -> RobottorRequest -> IO ()
_handleRobottorRequest r req = handleRobottorRequest r req >> return ()
