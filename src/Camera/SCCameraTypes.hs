{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Camera.SCCameraTypes where

import Control.DeepSeq
import Data.Aeson
import Data.MessagePack
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Word
import GHC.Generics
import System.Clock
import Text.Read (readPrec)
import qualified Text.ParserCombinators.ReadP as ReadP

newtype DetectorName = DetectorName {fromDetectorName :: Text} deriving (Show, Eq, Ord, Generic, NFData)

data DetectorProperty = NumericProperty {
                          npID :: !Int
                        , npDescription :: !Text
                        , npValue :: !Double
                      }
                    | DiscreteProperty {
                          dpID :: !Int
                        , dpDescription :: !Text
                        , dpCurrentOption :: !Text
                        , dpAvailableOptions :: ![Text]
                      } deriving (Show)

instance Eq DetectorProperty where
    (NumericProperty id1 _ v1) == (NumericProperty id2 _ v2) = (id1 == id2) && (v1 == v2)
    (DiscreteProperty id1 _ v1 _) == (DiscreteProperty id2 _ v2 _) = (id1 == id2) && (v1 == v2)
    _ == _ = False

data DetectorPropertyList = DetectorPropertyList {fromCPList :: ![DetectorProperty]}
                          deriving (Show)

data MeasuredImage = MeasuredImage {
                          miNRows :: !Int
                        , miNCols :: !Int
                        , miTimeStamp :: !SecondsSinceStartOfDetection
                        , miData :: !(V.Vector Word16)
                      }

newtype TimeAtStartOfEvent = TimeAtStartOfEvent {taseAsTimeSpec :: TimeSpec} deriving (Show)
newtype TimeAtStartOfExperiment = TimeAtStartOfExperiment {tasexAsTimeSpec :: TimeSpec} deriving (Show)
newtype SecondsSinceStartOfExperiment = SecondsSinceStartOfExperiment {sseAsSeconds :: Double} deriving (Show)
newtype SecondsSinceStartOfDetection = SecondsSinceStartOfDetection {ssdAsSeconds :: Double} deriving (Show)

instance NFData SecondsSinceStartOfExperiment where
    rnf _ = ()
instance NFData SecondsSinceStartOfDetection where
    rnf _ = ()

data OrientationOp = RotateCWOp
                   | RotateCCWOp
                   | FlipHorizontalOp
                   | FlipVerticalOp

instance ToJSON DetectorProperty where
    toJSON np@(NumericProperty _ _ _) = object ["propertycode" .= npID np, "descriptor" .= npDescription np,
                                                "kind" .= ("numeric" :: Text), "value" .= npValue np]
    toJSON dp@(DiscreteProperty _ _ _ _) = object ["propertycode" .= dpID dp, "descriptor" .= dpDescription dp,
                                                   "kind" .= ("discrete" :: Text), "current" .= dpCurrentOption dp,
                                                   "availableoptions" .= dpAvailableOptions dp]
instance FromJSON DetectorProperty where
    parseJSON (Object v) =
        v .: "kind" >>= \(kind :: Text) ->
        case kind of
            "numeric" -> NumericProperty <$> v .: "propertycode"
                                         <*> v .: "descriptor"
                                         <*> v .: "value"
            "discrete" -> DiscreteProperty <$> v .: "propertycode"
                                           <*> v .: "descriptor"
                                           <*> v .: "current"
                                           <*> v .: "availableoptions"
            _ -> fail "invalid kind of property object"
    parseJSON _ = fail "expected a JSON object"

instance FromJSON DetectorPropertyList where
  parseJSON (Object v) = DetectorPropertyList <$> v .: "properties"
  parseJSON _ = fail "expected a JSON object"

instance ToJSON DetectorName where
    toJSON (DetectorName n) = toJSON n
instance FromJSON DetectorName where
    parseJSON = fmap DetectorName . parseJSON

instance MessagePack DetectorName where
    toObject (DetectorName n) = toObject n
  
instance Read DetectorName where
    readsPrec _ = ReadP.readP_to_S (DetectorName . T.pack <$> ReadP.munch1 (/= '\"')) -- TODO !! This is ugly, because now Read cannot understand what Show produced.
                                                                                      -- It works in this way we because store the detector name directly in cameraoptions.txt.
