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

import MiscUtils

data RobotDesc = RobottorDesc {
                              roDescName :: !Text
                            , roDescIPAddress :: !String
                            , roDescPortNum :: !Int
                          }
                        deriving (Show, Read)

data Robot = Robottor {
                          roName :: !Text
                        , roIPAddress :: !String
                        , roPortNum :: !Int
                       }

instance ToJSON Robot where
   toJSON s = object ["name" .= robotName s]

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

readAvailableRobots :: IO [RobotDesc]
readAvailableRobots =
   getExecutablePath >>= \exePath ->
   readFile (takeDirectory exePath </> confFilename) >>=
   return . read
   where
     confFilename = "robots.txt"

robotName :: Robot -> Text
robotName (Robottor name _ _) = name

withRobots :: [RobotDesc] -> ([Robot] -> IO a) -> IO a
withRobots descs action =
    bracket (openRobots descs) closeRobots action

openRobots :: [RobotDesc] -> IO [Robot]
openRobots = mapM openRobot
    where
        openRobot (RobottorDesc name ip port) = return (Robottor name ip port)
        openRobot _ = throwIO (userError "opening unknown type of microscope robot")

closeRobots :: [Robot] -> IO ()
closeRobots _ = return ()

availableRobotsAndPrograms :: [Robot] -> IO [(Text, [Text])]
availableRobotsAndPrograms rss =
    let robotNames = map robotName rss
    in  zip robotNames <$> mapM listRobotPrograms rss

lookupRobot :: [Robot] -> Text -> Maybe Robot
lookupRobot mrs name = case (filter ((==) name . robotName) mrs) of
                           []      -> Nothing
                           (m : _) -> Just m
lookupRobotThrows :: [Robot] -> Text -> Robot
lookupRobotThrows rs n = case lookupRobot rs n of
                             Just r  -> r
                             Nothing -> throw (userError ("no robot " ++ T.unpack n))

isKnownRobot :: [Robot] -> Text -> Bool
isKnownRobot mrs name = isJust (lookupRobot mrs name)

listRobotPrograms :: Robot -> IO [Text]
listRobotPrograms (Robottor _ ip port) =
    let serverParams = QueryServerParams ip port (floor 1e6) isCompleteJSONObject decodeJSONObject
        queryMsg = (LB.toStrict . encode) ListRobottorPrograms
    in  (queryServer serverParams queryMsg >>= \(RobottorProgramListResponse ps) ->
      return ps) `catch` \(e :: IOException) -> throw (userError "can't communicate with Robottor program")

executeRobotProgram :: Robot -> Text -> Bool -> IO ()
executeRobotProgram r@(Robottor _ ip port) progName waitForCompletion = handleRobottorRequest r (ExecuteRobottorProgram progName waitForCompletion)

abortRobotProgramExecution :: Robot -> IO ()
abortRobotProgramExecution r@(Robottor _ ip port) = handleRobottorRequest r AbortProgramExecution

handleRobottorRequest :: Robot -> RobottorRequest -> IO ()
handleRobottorRequest (Robottor _ ip port) req =
    let serverParams = QueryServerParams ip port (floor 1e6) isCompleteJSONObject decodeJSONObject
        queryMsg = (LB.toStrict . encode) req
    in  queryServer serverParams queryMsg `catch` (\(e :: IOException) -> throw (userError "can't communicate with Robottor program")) >>= \response ->
        case response of
            OKRobottorResponse        -> return ()
            ErrorRobottorResponse err -> throwIO (userError ("error from robottor: " ++ T.unpack err))
