{-# LANGUAGE OverloadedStrings #-}
module Equipment.EquipmentPluginsInternal (
    EquipmentPlugin
  , loadPlugin
  , addDirectoryToLoaderPath
) where

import Control.Monad
import Data.ByteString(ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Int
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable

import Equipment.Equipment
import Equipment.EquipmentTypes
import Utils.DLLUtils
import Utils.MiscUtils

data EquipmentPlugin = EquipmentPlugin {
                           epEquipmentName :: !EqName
                         , epCloseDevice :: IO ()
                         , epAvailableLightSources :: ![LightSourceDescription]
                         , epActivateLightSource :: LSName -> [(LSChannelName, LSIlluminationPower)] -> IO ()
                         , epDeactivateLightSource :: IO ()
                         , epAvailableFilterWheels :: ![FilterWheelDescription]
                         , epSwitchToFilter :: FWName -> FName -> IO ()
                         , epHasMotorizedStage :: !Bool
                         , epStageName :: !StageName
                         , epSupportedStageAxes :: ![StageAxis]
                         , epGetStagePositionFunc :: IO StagePosition
                         , epSetStagePositionFunc :: StagePosition -> IO ()
                       }

instance Equipment EquipmentPlugin where
    equipmentName = epEquipmentName
    flushSerialPorts _ = return ()
    closeDevice = epCloseDevice
    availableLightSources = epAvailableLightSources
    activateLightSource = epActivateLightSource
    deactivateLightSource = epDeactivateLightSource
    --availableFilterWheels = epAvailableFilterWheels
    --switchToFilter = epSwitchToFilter
    hasMotorizedStage = epHasMotorizedStage
    motorizedStageName _ = StageName "stage"
    supportedStageAxes = epSupportedStageAxes
    getStagePosition = epGetStagePositionFunc
    setStagePosition = epSetStagePositionFunc

type InitFunc = Ptr () -> IO CInt
type ShutdownFunc = IO ()
type IdentifierFunc = CString -> CUInt -> IO CInt
type StringListFunc = Ptr CString -> CInt -> CInt -> Ptr CInt -> IO CInt
type SingleIntPtrFunc = Ptr CInt -> IO CInt
type AvailableLightSourcesFunc = StringListFunc
type AvailableChannelsFunc      = CString -> Ptr CString -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
type ActivateLightSourceFunc    = CString -> Ptr CString -> Ptr CDouble -> CInt -> IO CInt
type DeactivateLightSourceFunc  = IO CInt
type AvailableFilterWheelsFunc = StringListFunc
type AvailableFiltersFunc = CString -> Ptr CString -> CInt -> CInt -> Ptr CInt -> IO CInt
type SwitchToFilterFunc = CString -> CString -> IO CInt
type SupportedStageAxesFunc = Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
type GetStagePositionFunc = Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
type SetStagePositionFunc = CDouble -> CDouble -> CDouble -> CInt -> CInt -> IO CInt

foreign import ccall "dynamic" mkInitFunc :: FunPtr InitFunc -> InitFunc
foreign import ccall "dynamic" mkShutdownFunc :: FunPtr ShutdownFunc -> ShutdownFunc
foreign import ccall "dynamic" mkIdentifierFunc :: FunPtr IdentifierFunc -> IdentifierFunc
foreign import ccall "dynamic" mkSingleIntPtrFunc :: FunPtr SingleIntPtrFunc -> SingleIntPtrFunc
foreign import ccall "dynamic" mkSupportedStageAxesFunc :: FunPtr SupportedStageAxesFunc -> SupportedStageAxesFunc
foreign import ccall "dynamic" mkGetStagePositionFunc :: FunPtr GetStagePositionFunc -> GetStagePositionFunc
foreign import ccall "dynamic" mkSetStagePositionFunc :: FunPtr SetStagePositionFunc -> SetStagePositionFunc
foreign import ccall "dynamic" mkAvailableLightSourcesFunc :: FunPtr StringListFunc -> AvailableLightSourcesFunc
foreign import ccall "dynamic" mkAvailableChannelsFunc :: FunPtr AvailableChannelsFunc -> AvailableChannelsFunc
foreign import ccall "dynamic" mkActivateLightSourceFunc :: FunPtr ActivateLightSourceFunc -> ActivateLightSourceFunc
foreign import ccall "dynamic" mkDeactivateLightSourceFunc :: FunPtr DeactivateLightSourceFunc -> DeactivateLightSourceFunc
foreign import ccall "dynamic" mkAvailableFilterWheelsFunc :: FunPtr StringListFunc -> AvailableFilterWheelsFunc
foreign import ccall "dynamic" mkAvailableFiltersFunc :: FunPtr AvailableFiltersFunc -> AvailableFiltersFunc
foreign import ccall "dynamic" mkSetFilterFunc :: FunPtr SwitchToFilterFunc -> SwitchToFilterFunc

loadPlugin :: Text -> IO EquipmentW
loadPlugin libName =
    loadModule libName >>= \modu ->
    loadFunc modu "InitImagerPlugin" mkInitFunc >>= \initF ->
    loadFunc modu "ShutdownImagerPlugin" mkShutdownFunc >>= \shutdownF ->
    loadFunc modu "ImagerPluginAPIVersion" mkSingleIntPtrFunc >>= \apiVersionF ->
    loadFunc modu "EquipmentName" mkIdentifierFunc >>= \eqNameF ->
    loadFunc modu "ListAvailableLightSources" mkAvailableLightSourcesFunc >>= \listLightSourcesF ->
    loadFunc modu "ListAvailableChannels" mkAvailableChannelsFunc >>= \listChannelsF ->
    loadFunc modu "ActivateLightSource" mkActivateLightSourceFunc >>= \activateLightSourceF ->
    loadFunc modu "DeactivateLightSource" mkDeactivateLightSourceFunc >>= \deactivateLightSourceF ->
    loadFunc modu "ListAvailableFilterWheels" mkAvailableFilterWheelsFunc >>= \listFilterWheelsF ->
    loadFunc modu "ListAvailableFilters" mkAvailableFiltersFunc >>= \listFiltersF ->
    loadFunc modu "SetFilter" mkSetFilterFunc >>= \setFilterF ->
    loadFunc modu "HasMotorizedStage" mkSingleIntPtrFunc >>= \hasStageF ->
    loadFunc modu "MotorizedStageName" mkIdentifierFunc >>= \stageNameF ->
    loadFunc modu "SupportedStageAxes" mkSupportedStageAxesFunc >>= \suppAxesF ->
    loadFunc modu "GetStagePosition" mkGetStagePositionFunc >>= \getStagePosF ->
    loadFunc modu "SetStagePosition" mkSetStagePositionFunc >>= \setStagePosF ->

    castFunPtrToPtr <$> mkCStringCallback (pluginPrinter libName) >>= \printFunc ->
    initF printFunc >>= \initResult ->
    when (initResult /= 0) (error "couldn't init plugin") >>
    verifyPluginVersion apiVersionF >>
    EqName <$> readIdentifier eqNameF >>= \eqName ->
    readAvailableLightSources (listLightSourcesF, listChannelsF) >>= \lightSources ->
    readAvailableFilterWheels (listFilterWheelsF, listFiltersF) >>= \filterWheels ->
    ((/=) 0) <$> readSingleIntPtr hasStageF >>= \hasStage ->
    StageName <$> readIdentifier stageNameF >>= \stageName ->
    readSupportedAxesFunc suppAxesF >>= \supportedAxes ->
    EquipmentW <$> pure (EquipmentPlugin eqName shutdownF lightSources (handleActivateLightSource activateLightSourceF)
                                         (handleDeactivateLightSource deactivateLightSourceF)
                                         filterWheels (handleSwitchToFilter setFilterF)
                                         hasStage stageName supportedAxes
                                         (handleGetStagePosition getStagePosF) (handleSetStagePosition setStagePosF))
    where
        verifyPluginVersion :: SingleIntPtrFunc -> IO ()
        verifyPluginVersion f = alloca $ \versionPtr ->
                                checkError (f versionPtr) >>
                                peek versionPtr >>= \version ->
                                when (version /= 0) (error "incorrect api version")
        readIdentifier :: IdentifierFunc -> IO Text
        readIdentifier f = allocaArray0 128 $ \nameArray ->
                           checkError (f nameArray 128) >>
                           T.pack <$> peekCString nameArray
        readSingleIntPtr :: SingleIntPtrFunc -> IO Int
        readSingleIntPtr f = alloca $ \intPtr ->
                             checkError (f intPtr) >>
                             fromIntegral <$> peek intPtr
        readAvailableLightSources :: (AvailableLightSourcesFunc, AvailableChannelsFunc) -> IO [LightSourceDescription]
        readAvailableLightSources (fLights, fChannels) =
            handleStringListFunc fLights >>= \arrayOfcStr ->
            sequence (map (readLightSourceDescription fChannels) arrayOfcStr)
        readLightSourceDescription :: AvailableChannelsFunc -> CString -> IO LightSourceDescription
        readLightSourceDescription fChannels cSourceName =
            withCStringArray' 16 128 $ \namesPtr ->
            alloca $ \nNamesReturnedPtr ->
            alloca $ \canControlPowerPtr ->
            alloca $ \channelsAreExclusivePtr ->
            checkError (fChannels cSourceName namesPtr 16 128 nNamesReturnedPtr canControlPowerPtr channelsAreExclusivePtr) >>
            LSName <$> T.pack <$> peekCString cSourceName >>= \sourceName ->
            ((/=) 0) <$> peek canControlPowerPtr >>= \canControlPower ->
            ((/=) 0) <$> peek channelsAreExclusivePtr >>= \channelsAreExclusive ->
            fromIntegral <$> peek nNamesReturnedPtr >>= \nNamesReturned ->
            map LSChannelName <$> peekArrayString nNamesReturned namesPtr >>= \channels ->
            pure (LightSourceDescription sourceName canControlPower channelsAreExclusive channels)
        handleActivateLightSource :: ActivateLightSourceFunc -> (LSName -> [(LSChannelName, LSIlluminationPower)] -> IO ())
        handleActivateLightSource f (LSName name) ps =
            withCString (T.unpack name) $ \cName ->
            withCStringArray (map (T.unpack . fromLSChannelName . fst) ps) $ \cChannels ->
            withArray (map (CDouble . fromLSIlluminationPower . snd) ps) $ \cPowers ->
            checkError (f cName cChannels cPowers (fromIntegral (length ps)))
        handleDeactivateLightSource :: DeactivateLightSourceFunc -> IO ()
        handleDeactivateLightSource f = checkError f
        readAvailableFilterWheels :: (AvailableFilterWheelsFunc, AvailableFiltersFunc) -> IO [FilterWheelDescription]
        readAvailableFilterWheels (fwListF, filterListF) =
            handleStringListFunc fwListF >>= \filterWheelNames ->
            forM filterWheelNames (\fwName ->
                handleStringListFunc (filterListF fwName)) >>= \filterNames ->
            sequence (zipWith zipF filterWheelNames filterNames)
            where
                zipF :: CString -> [CString] -> IO FilterWheelDescription
                zipF fwName filters = do
                    asFW <- FWName . T.pack <$> peekCString fwName
                    asFNames <- mapM (\n -> FName . T.pack <$> peekCString n) filters
                    pure (FilterWheelDescription asFW asFNames)
        handleSwitchToFilter :: SwitchToFilterFunc -> (FWName -> FName -> IO ())
        handleSwitchToFilter f (FWName fwName) (FName fName) =
            withCString (T.unpack fwName) $ \cFwName ->
            withCString (T.unpack fName) $ \cFName ->
            checkError (f cFwName cFName)
        readSupportedAxesFunc :: SupportedStageAxesFunc -> IO [StageAxis]
        readSupportedAxesFunc f =
            alloca $ \xPtr ->
            alloca $ \yPtr ->
            alloca $ \zPtr ->
            checkError (f xPtr yPtr zPtr) >>
            ((/=) 0) <$> peek xPtr >>= \x ->
            ((/=) 0) <$> peek yPtr >>= \y ->
            ((/=) 0) <$> peek zPtr >>= \z ->
            pure ([] ++ (if x then [XAxis] else []) ++ (if y then [YAxis] else []) ++ (if z then [ZAxis] else []))
        handleGetStagePosition :: GetStagePositionFunc -> IO StagePosition
        handleGetStagePosition f =
            alloca $ \xPtr ->
            alloca $ \yPtr ->
            alloca $ \zPtr ->
            alloca $ \useAFPtr ->
            alloca $ \afOffsetPtr ->
            checkError (f xPtr yPtr zPtr useAFPtr afOffsetPtr) >>
            fromCDouble <$> peek xPtr >>= \x ->
            fromCDouble <$> peek yPtr >>= \y ->
            fromCDouble <$> peek zPtr >>= \z ->
            (/= 0) <$> peek useAFPtr >>= \useAF ->
            fromIntegral <$> peek afOffsetPtr >>= \afOffset ->
            pure (StagePosition x y z useAF afOffset)
        handleSetStagePosition :: SetStagePositionFunc -> (StagePosition -> IO ())
        handleSetStagePosition f =
            \(StagePosition x y z useAF afOffset) ->
                checkError (f (CDouble x) (CDouble y) (CDouble z) (if (useAF) then 1 else 0) (fromIntegral afOffset))

pluginPrinter :: Text -> CString -> IO ()
pluginPrinter pluginName str = T.putStr pluginName >> T.putStr ": " >>
                               B.packCString str >>= B.putStrLn


withCStringArray :: [String] -> (Ptr CString -> IO b) -> IO b
withCStringArray ss f = aux ss []
    where
        aux [] cl = withArray cl f
        aux (s:ls) cl = withCString s (\cs -> aux ls (cs:cl))

withCStringArray' :: Int -> Int -> (Ptr CString -> IO b) -> IO b
withCStringArray' len strLen = withCStringArray (replicate len (replicate strLen '\0'))

peekArrayString :: Int -> Ptr CString -> IO [Text]
peekArrayString n ptr = map T.pack <$> join (sequence <$> map peekCString <$> peekArray n ptr)

handleStringListFunc :: StringListFunc -> IO [CString]
handleStringListFunc f =
    withCStringArray' 16 128 $ \namesPtr ->
    alloca $ \nNamesReturnedPtr ->
    checkError (f namesPtr 16 128 nNamesReturnedPtr) >>
    fromIntegral <$> peek nNamesReturnedPtr >>= \nNamesReturned ->
    peekArray nNamesReturned namesPtr

readObjects :: StringListFunc -> (CString -> IO b) -> IO [b]
readObjects fList fDetail =
    handleStringListFunc fList >>= \arrayOfcStr ->
    sequence (map fDetail arrayOfcStr)

checkError :: IO CInt -> IO ()
checkError f = f >>= \result ->
               when (result /= 0) (error "error executing plugin")
