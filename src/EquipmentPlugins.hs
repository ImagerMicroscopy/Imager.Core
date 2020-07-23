{-# LANGUAGE OverloadedStrings #-}
module EquipmentPlugins (
    loadPlugins
) where

import Control.Monad
import Data.Text(Text)
import qualified Data.Text as T
import System.Directory
import System.Environment
import System.FilePath

import Equipment
import EquipmentPluginsInternal

loadPlugins :: IO [EquipmentW]
loadPlugins =
    takeDirectory <$> getExecutablePath >>= \appPath ->
    pure (appPath </> "Plugins") >>= \pluginsDirPath ->
    createDirectoryIfMissing False pluginsDirPath >>
    addDirectoryToLoaderPath (T.pack pluginsDirPath) >>
    map T.pack <$> listDirectory pluginsDirPath >>= \fileNames ->
    let pluginNames = filter (\fn -> takeExtension (T.unpack fn) == ".imagerplugin") fileNames
    in  forM pluginNames ( \pn ->
            putStrLn ("Loading plugin " ++ T.unpack pn ++ "...") >>
            loadPlugin pn
    )
