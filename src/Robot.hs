{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables #-}
module Robot where

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
                     | ExecuteRobottorProgram !Text !Bool
                     | AbortProgramExecution
                     deriving (Show)

data RobottorResponse = OKRobottorResponse
                      | ErrorRobottorResponse !Text
                      | RobottorProgramListResponse ![Text]
                      deriving (Show)

instance ToJSON RobottorRequest where
  toJSON ListRobottorPrograms = object ["type" .= ("listprograms" :: Text)]
  toJSON (ExecuteRobottorProgram prog wait) = object ["type" .= ("executeprogram" :: Text), "programname" .= prog, "waitforcompletion" .= wait]
  toJSON AbortProgramExecution = object ["type" .= ("abortprogramexecution" :: Text)]

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
listRobotPrograms (Robottor _ ip port) =
    let serverParams = QueryServerParams ip port (floor 1e6) isCompleteJSONObject decodeJSONObject
        queryMsg = (LB.toStrict . encode) ListRobottorPrograms
    in  (queryServer serverParams queryMsg >>= \(RobottorProgramListResponse ps) ->
      return ps) `catch` \(e :: IOException) -> throw (userError "can't communicate with Robottor program")

executeRobotProgram :: Equipment -> Text -> Bool -> IO ()
executeRobotProgram r@(Robottor _ ip port) progName waitForCompletion = handleRobottorRequest r (ExecuteRobottorProgram progName waitForCompletion)

abortRobotProgramExecution :: Equipment -> IO ()
abortRobotProgramExecution r@(Robottor _ ip port) = handleRobottorRequest r AbortProgramExecution

handleRobottorRequest :: Equipment -> RobottorRequest -> IO ()
handleRobottorRequest (Robottor _ ip port) req =
    let serverParams = QueryServerParams ip port (floor 1e6) isCompleteJSONObject decodeJSONObject
        queryMsg = (LB.toStrict . encode) req
    in  queryServer serverParams queryMsg `catch` (\(e :: IOException) -> throw (userError "can't communicate with Robottor program")) >>= \response ->
        case response of
            OKRobottorResponse        -> return ()
            ErrorRobottorResponse err -> throwIO (userError ("error from robottor: " ++ T.unpack err))
