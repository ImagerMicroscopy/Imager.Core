{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables #-}
module MicroscopeRobot where

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

data MicroscopeRobotDesc = RobottorDesc {
                              roDescName :: !Text
                            , roDescIPAddress :: !String
                            , roDescPortNum :: !Int
                          }
                        deriving (Show, Read)

data MicroscopeRobot = Robottor {
                          roName :: !Text
                        , roIPAddress :: !String
                        , roPortNum :: !Int
                       }

instance ToJSON MicroscopeRobot where
   toJSON s = object ["name" .= microscopeRobotName s]

data RobottorRequest = ListRobottorPrograms
                     | ExecuteRobottorProgram !Text
                     deriving (Show)

data RobottorResponse = OKRobottorResponse
                      | ErrorRobottorResponse !Text
                      | RobottorProgramListResponse ![Text]
                      deriving (Show)

instance ToJSON RobottorRequest where
  toJSON ListRobottorPrograms = object ["type" .= ("listprograms" :: Text)]
  toJSON (ExecuteRobottorProgram prog) = object ["type" .= ("executeprogram" :: Text), "programname" .= prog]

instance FromJSON RobottorResponse where
  parseJSON (Object v) =
      v .: "type" >>= \(typ :: Text) ->
      case typ of
        "status"      -> v .: "status" >>= \(stat :: Text) ->
                          case stat of
                              "ok"    -> return OKRobottorResponse
                              "error" -> ErrorRobottorResponse <$> v .: "error"
                              _       -> fail "unknown status"
        "programlist" -> RobottorProgramListResponse <$> v .: "programNames"
        _             -> fail "can't decode robottor response type"
  parseJSON _ = fail "can't decode robottor response"

readAvailableMicroscopeRobots :: IO [MicroscopeRobotDesc]
readAvailableMicroscopeRobots =
   getExecutablePath >>= \exePath ->
   readFile (takeDirectory exePath </> confFilename) >>=
   return . read
   where
     confFilename = "microscoperobots.txt"

microscopeRobotName :: MicroscopeRobot -> Text
microscopeRobotName (Robottor name _ _) = name

withMicroscopeRobots :: [MicroscopeRobotDesc] -> ([MicroscopeRobot] -> IO a) -> IO a
withMicroscopeRobots descs action =
    bracket (openMicroscopeRobots descs) closeMicroscopeRobots action

openMicroscopeRobots :: [MicroscopeRobotDesc] -> IO [MicroscopeRobot]
openMicroscopeRobots = mapM openRobot
    where
        openRobot (RobottorDesc name ip port) = return (Robottor name ip port)
        openRobot _ = throwIO (userError "opening unknown type of microscope robot")

closeMicroscopeRobots :: [MicroscopeRobot] -> IO ()
closeMicroscopeRobots _ = return ()

lookupRobot :: [MicroscopeRobot] -> Text -> Maybe MicroscopeRobot
lookupRobot mrs name = case (filter ((==) name . microscopeRobotName) mrs) of
                           []      -> Nothing
                           (m : _) -> Just m
isKnownMicroscopeRobot :: [MicroscopeRobot] -> Text -> Bool
isKnownMicroscopeRobot mrs name = isJust (lookupRobot mrs name)

listRobotPrograms :: [MicroscopeRobot] -> Text -> IO [Text]
listRobotPrograms mrs name
    | not (isKnownMicroscopeRobot mrs name) = throwIO (userError ("unknown robot " ++ T.unpack name))
    | otherwise = let robot = fromJust (lookupRobot mrs name)
                  in  listRobotPrograms' robot
    where
      listRobotPrograms' (Robottor _ ip port) =
          let serverParams = QueryServerParams ip port (floor 1e6) isCompleteJSONObject decodeJSONObject
              queryMsg = (LB.toStrict . encode) (ListRobottorPrograms)
          in  queryServer serverParams queryMsg >>= \(RobottorProgramListResponse ps) ->
              return ps

executeRobotProgram :: [MicroscopeRobot] -> Text -> Text -> IO ()
executeRobotProgram mrs name progName
    | not (isKnownMicroscopeRobot mrs name) = throwIO (userError ("unknown robot " ++ T.unpack name))
    | otherwise = let robot = fromJust (lookupRobot mrs name)
                  in  executeRobotProgram' robot progName
    where
        executeRobotProgram' (Robottor _ ip port) progName =
            let serverParams = QueryServerParams ip port (floor 1e6) isCompleteJSONObject decodeJSONObject
                queryMsg = (LB.toStrict . encode) (ExecuteRobottorProgram progName)
            in  queryServer serverParams queryMsg >>= \response ->
                case response of
                    OKRobottorResponse        -> return ()
                    ErrorRobottorResponse err -> throwIO (userError ("error from robottor: " ++ T.unpack err))
