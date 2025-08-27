{-# LANGUAGE OverloadedStrings #-}
module Equipment.EquipmentPlugins (
    loadPlugins
) where

import Control.Monad
import Data.List
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Format as T
import qualified Data.Text.IO as T
import System.Directory
import System.Environment
import System.FilePath

import Equipment.Equipment
import Equipment.EquipmentPluginsInternal
import Equipment.EquipmentTypes
import Utils.MiscUtils

loadPlugins :: IO [(EquipmentW, [PluginDetector])]
loadPlugins =
    takeDirectory <$> getExecutablePath >>= \appPath ->
    pure (appPath </> "Plugins") >>= \pluginsDirPath ->
    createDirectoryIfMissing False pluginsDirPath >>
    addDirectoryToLoaderPath (T.pack pluginsDirPath) >>
    map T.pack <$> listDirectory pluginsDirPath >>= \fileNames ->
    let pluginNames = filter (\fn -> takeExtension (T.unpack fn) == ".imagerplugin") fileNames
    in  forM pluginNames ( \pn ->
            putStrLn ("Loading plugin " ++ T.unpack pn ++ "...") >>
            loadPlugin pn >>= \contents ->
            T.putStr (describePluginContents contents) >> pure contents
    )

describePluginContents :: (EquipmentW, [PluginDetector]) -> Text
describePluginContents (eq, cams) =
    let eqName = fromEqName $ equipmentName eq
        lightSourcesStr = mconcat . intersperse ", " . map (fromLSName . lsdName) $ availableLightSources eq
        movableComponentsStr = mconcat . intersperse ", " . map componentName $ availableMovableComponents eq
        stageStr = if (hasMotorizedStage eq) then "Motorized stage with " <> axisStr else "No motorized stage"
        axisStr = mconcat . intersperse " " . map (T.pack . show) $ supportedStageAxes eq
        cameraNamesStr = mconcat . intersperse ", " .  map pdCamName $ cams
    in  (mconcat . intersperse "\n") [
            formatT "Found equipment \"{}\"" (T.Only eqName),
            formatT "\tLight sources: {}" (T.Only lightSourcesStr),
            formatT "\tMovable components: {}" (T.Only movableComponentsStr),
            formatT "\t{}" (T.Only stageStr),
            formatT "\tCameras: {}" (T.Only cameraNamesStr),
            "\n"
        ]
            
            
