module SCCameraTypes where

import Control.DeepSeq
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Word

data CameraProperty = NumericProperty {
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

instance Eq CameraProperty where
    (NumericProperty id1 _ v1) == (NumericProperty id2 _ v2) = (id1 == id2) && (v1 == v2)
    (DiscreteProperty id1 _ v1 _) == (DiscreteProperty id2 _ v2 _) = (id1 == id2) && (v1 == v2)
    _ == _ = False

data CameraPropertyList = CameraPropertyList {fromCPList :: ![CameraProperty]}
                          deriving (Show)

data MeasuredImages = MeasuredImages {
                          miNRows :: !Int
                        , miNCols :: !Int
                        , miTimeStamp :: !Double    -- seconds since beginning of acquisition
                        , miData :: !(V.Vector Word16)
                      }

data OrientationOp = RotateCWOp
                   | RotateCCWOp
                   | FlipHorizontalOp
                   | FlipVerticalOp

instance ToJSON CameraProperty where
    toJSON np@(NumericProperty _ _ _) = object ["propertycode" .= npID np, "descriptor" .= npDescription np,
                                                "kind" .= ("numeric" :: Text), "value" .= npValue np]
    toJSON dp@(DiscreteProperty _ _ _ _) = object ["propertycode" .= dpID dp, "descriptor" .= dpDescription dp,
                                                   "kind" .= ("discrete" :: Text), "current" .= dpCurrentOption dp,
                                                   "availableoptions" .= dpAvailableOptions dp]
instance FromJSON CameraProperty where
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

instance FromJSON CameraPropertyList where
  parseJSON (Object v) = CameraPropertyList <$> v .: "properties"
  parseJSON _ = fail "expected a JSON object"
