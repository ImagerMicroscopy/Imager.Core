{-# LANGUAGE BangPatterns, InstanceSigs #-}

module Equipment.EquipmentPluginsInternal (
    EquipmentPlugin
  , PluginDetector(..)
  , loadPlugin
  , addDirectoryToLoaderPath
) where

import Control.Concurrent.Chan
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Aeson hiding (withArray)
import Data.ByteString(ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Unsafe as B
import Data.Maybe
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Foreign as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import Data.Int
import Data.List
import qualified Data.Vector.Storable as V
import Data.Word
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import System.Clock
import System.FilePath

import Camera.SCCameraTypes
import Detectors.Detector
import Equipment.Equipment
import Equipment.EquipmentTypes
import Utils.DLLUtils
import Utils.MiscUtils

pluginAPIVersion :: CInt
pluginAPIVersion = 2

data EquipmentPlugin = EquipmentPlugin {
                           epEquipmentName :: !EqName
                         , epCloseDevice :: IO ()

                         , epAvailableLightSources :: ![LightSourceDescription]
                         , epActivateLightSource :: LSName -> [(LSChannelName, LSIlluminationPower)] -> IO ()
                         , epDeactivateLightSource :: IO ()

                         , epAvailableMovableComponents :: [MovableComponentDescription]
                         , epMoveComponent :: [MovableComponentSetting] -> IO ()

                         , epHasMotorizedStage :: !Bool
                         , epStageName :: !StageName
                         , epSupportedStageAxes :: ![StageAxis]
                         , epGetStagePositionFunc :: IO StagePosition
                         , epSetStagePositionFunc :: StagePosition -> IO ()
                         
                         , epListRobots :: [RobotDescription]
                         , epExecuteRobotProgram :: RobotName -> RobotProgramCallParams -> IO ()
                         , epStopRobot :: IO ()

                         , epListConnectedCameras :: [DetectorName]
                         , epGetCameraOptions :: DetectorName -> IO [DetectorProperty]
                         , epSetCameraOption :: DetectorName -> DetectorProperty -> IO ()
                         , epGetFrameRate :: DetectorName -> IO Double
                         , epIsConfiguredForHardwareTriggering :: DetectorName -> IO Bool
                         , epAcquireSingleImage :: DetectorName -> IO MeasuredImage
                         , epStartAsyncAcquisition :: DetectorName -> IO ()
                         , epStartBoundedAsyncAcquisition :: DetectorName -> Word64 -> IO ()
                         , epGetOldestImageAsyncAcquired :: DetectorName -> Word -> IO (Maybe MeasuredImage)
                         , epAbortAsyncAcquisition :: DetectorName -> IO ()
                       }

instance Equipment EquipmentPlugin where
    equipmentName = epEquipmentName
    flushSerialPorts _ = return ()
    closeDevice = epCloseDevice
    availableLightSources = epAvailableLightSources
    activateLightSource = epActivateLightSource
    deactivateLightSource = epDeactivateLightSource
    availableMovableComponents = epAvailableMovableComponents
    moveComponent = epMoveComponent
    hasMotorizedStage = epHasMotorizedStage
    motorizedStageName _ = StageName "stage"
    supportedStageAxes = epSupportedStageAxes
    getStagePosition = epGetStagePositionFunc
    setStagePosition = epSetStagePositionFunc
    availableRobots = epListRobots
    executeRobotProgram = epExecuteRobotProgram
    stopRobot = epStopRobot

data PluginDetector = PluginDetector {
                          pdCamName :: !DetectorName
                        , pdEquipmentPlugin :: EquipmentPlugin
                      }

instance Detector PluginDetector where
    detectorName = pdCamName

    acquireData :: PluginDetector -> IO MeasuredImage
    acquireData (PluginDetector camName eP@(EquipmentPlugin{})) =
        eP.epAcquireSingleImage camName

    acquireStreamingData :: PluginDetector -> NMeasurementsToPerform -> Signal -> Chan AsyncData -> IO ()
    acquireStreamingData (PluginDetector camName ep@(EquipmentPlugin{})) nMeasurements hasStarted chan =
        performAcq `onException` (ep.epAbortAsyncAcquisition camName >> writeChan chan AsyncError)
        where
            performAcq = ep.epStartBoundedAsyncAcquisition camName (fromIntegral nMeasurements) >>
                         raiseSignal hasStarted >>
                         fetchImages nMeasurements chan >>
                         writeChan chan AsyncFinished
            fetchImages :: Int -> Chan AsyncData -> IO ()
            fetchImages nImagesRemaining chan
                | nImagesRemaining == 0 = return ()
                | otherwise =
                    fetchNextImage >>= \measuredImage ->
                    writeChan chan (AsyncData (camName, measuredImage)) >>
                    fetchImages (nImagesRemaining - 1) chan
            fetchNextImage = ep.epGetOldestImageAsyncAcquired camName 500 >>= \maybeImg ->
                                if (isJust maybeImg)
                                    then return (fromJust maybeImg)
                                    else fetchNextImage

    getDetectorProperties :: PluginDetector -> IO [DetectorProperty]
    getDetectorProperties (PluginDetector camName ep@(EquipmentPlugin{})) = ep.epGetCameraOptions camName
    setDetectorOption :: PluginDetector -> DetectorProperty -> IO ()
    setDetectorOption (PluginDetector camName ep@(EquipmentPlugin{})) opt = ep.epSetCameraOption camName opt
    getDetectorFrameRate :: PluginDetector -> IO Double
    getDetectorFrameRate (PluginDetector camName ep@(EquipmentPlugin{})) = ep.epGetFrameRate camName
    isConfiguredForHardwareTriggering (PluginDetector camName ep@(EquipmentPlugin{})) = ep.epIsConfiguredForHardwareTriggering camName


type InitFunc = CString -> Ptr () -> IO CInt
type ShutdownFunc = IO ()
type IdentifierFunc = CString -> CUInt -> IO CInt
type StringListFunc = Ptr CString -> CInt -> CInt -> Ptr CInt -> IO CInt
type StringListFuncWithName = CString -> Ptr CString -> CInt -> CInt -> Ptr CInt -> IO CInt
type SingleIntPtrFunc = Ptr CInt -> IO CInt
type GetLastErrorFunc = CString -> CSize -> IO ()

type AvailableLightSourcesFunc = StringListFunc
type AvailableChannelsFunc      = CString -> Ptr CString -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
type ActivateLightSourceFunc    = CString -> Ptr CString -> Ptr CDouble -> CInt -> IO CInt
type DeactivateLightSourceFunc  = IO CInt

type ListDiscreteMovableComponentsFunc = StringListFunc
type ListContinuouslyMovableComponentsFunc = StringListFunc
type ListDiscreteMovableComponentSettingsFunc = StringListFuncWithName
type ListContinuouslyMovableComponentRangeFunc = CString -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
type SetMovableComponentsFunc = CInt -> Ptr CString -> Ptr CString -> CInt -> Ptr CString -> Ptr CDouble -> IO CInt

type SupportedStageAxesFunc = Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
type GetStagePositionFunc = Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
type SetStagePositionFunc = CDouble -> CDouble -> CDouble -> CInt -> CInt -> IO CInt

type ListRobotsFunc = StringListFunc
type ListRobotProgramsFunc = CString -> Ptr CString -> IO CInt
type ReleaseRobotProgramsInfoFunc = FunPtr (CString -> IO ())
type ExecuteRobotProgramFunc = CString -> CString -> IO CInt
type StopRobotFunc = IO CInt

type ListConnectedCameraNamesFunc = StringListFunc
type GetCameraOptionsFunc = CString -> Ptr CString -> IO CInt
type ReleaseOptionsDataFunc = FunPtr (CString -> IO ())
type SetCameraOptionFunc = CString -> CString -> IO CInt
type GetFrameRateFunc = CString -> Ptr CDouble -> IO CInt
type IsConfiguredForHardwareTriggeringFunc = CString -> Ptr CInt -> IO CInt
type AcquireSingleImageFunc = CString -> Ptr (Ptr Word16) -> Ptr CInt -> Ptr CInt -> IO CInt
type StartAsyncAcquisitionFunc = CString -> IO CInt
type StartBoundedAsyncAcquisitionFunc = CString -> Word64 -> IO CInt
type GetOldestImageAsyncAcquiredFunc = CString -> Word32 -> Ptr (Ptr Word16) -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
type ReleaseImageDataFunc = FunPtr (Ptr Word16 -> IO ())
type AbortAsyncAcquisitionFunc = CString -> IO CInt
type GetLastSCCamErrorFunc = GetLastErrorFunc

foreign import ccall "dynamic" mkInitFunc :: FunPtr InitFunc -> InitFunc
foreign import ccall "dynamic" mkShutdownFunc :: FunPtr ShutdownFunc -> ShutdownFunc
foreign import ccall "dynamic" mkIdentifierFunc :: FunPtr IdentifierFunc -> IdentifierFunc
foreign import ccall "dynamic" mkSingleIntPtrFunc :: FunPtr SingleIntPtrFunc -> SingleIntPtrFunc

foreign import ccall "dynamic" mkAvailableLightSourcesFunc :: FunPtr StringListFunc -> AvailableLightSourcesFunc
foreign import ccall "dynamic" mkAvailableChannelsFunc :: FunPtr AvailableChannelsFunc -> AvailableChannelsFunc
foreign import ccall "dynamic" mkActivateLightSourceFunc :: FunPtr ActivateLightSourceFunc -> ActivateLightSourceFunc
foreign import ccall "dynamic" mkDeactivateLightSourceFunc :: FunPtr DeactivateLightSourceFunc -> DeactivateLightSourceFunc

foreign import ccall "dynamic" mkListDiscreteMovableComponentsFunc :: FunPtr ListDiscreteMovableComponentsFunc -> ListDiscreteMovableComponentsFunc
foreign import ccall "dynamic" mkListContinuouslyMovableComponentsFunc :: FunPtr ListContinuouslyMovableComponentsFunc -> ListContinuouslyMovableComponentsFunc
foreign import ccall "dynamic" mkListDiscreteMovableComponentSettingsFunc :: FunPtr ListDiscreteMovableComponentSettingsFunc -> ListDiscreteMovableComponentSettingsFunc
foreign import ccall "dynamic" mkListContinuouslyMovableComponentRangeFunc :: FunPtr ListContinuouslyMovableComponentRangeFunc -> ListContinuouslyMovableComponentRangeFunc
foreign import ccall "dynamic" mkSetMovableComponentsFunc :: FunPtr SetMovableComponentsFunc -> SetMovableComponentsFunc

foreign import ccall "dynamic" mkSupportedStageAxesFunc :: FunPtr SupportedStageAxesFunc -> SupportedStageAxesFunc
foreign import ccall "dynamic" mkGetStagePositionFunc :: FunPtr GetStagePositionFunc -> GetStagePositionFunc
foreign import ccall "dynamic" mkSetStagePositionFunc :: FunPtr SetStagePositionFunc -> SetStagePositionFunc

foreign import ccall "dynamic" mkListRobotsFunc :: FunPtr ListRobotsFunc -> ListRobotsFunc
foreign import ccall "dynamic" mkListRobotProgramsFunc :: FunPtr ListRobotProgramsFunc -> ListRobotProgramsFunc
foreign import ccall "dynamic" mkReleaseRobotProgramsInfoFunc :: FunPtr ReleaseRobotProgramsInfoFunc -> ReleaseRobotProgramsInfoFunc
foreign import ccall "dynamic" mkExecuteRobotProgramFunc :: FunPtr ExecuteRobotProgramFunc -> ExecuteRobotProgramFunc
foreign import ccall "dynamic" mkStopRobotFunc :: FunPtr StopRobotFunc -> StopRobotFunc

foreign import ccall "dynamic" mkListConnectedCameraNamesFunc :: FunPtr StringListFunc -> ListConnectedCameraNamesFunc
foreign import ccall "dynamic" mkGetCameraOptionsFunc :: FunPtr GetCameraOptionsFunc -> GetCameraOptionsFunc
foreign import ccall "dynamic" mkSetCameraOptionFunc :: FunPtr SetCameraOptionFunc -> SetCameraOptionFunc
foreign import ccall "dynamic" mkGetFrameRateFunc :: FunPtr GetFrameRateFunc -> GetFrameRateFunc
foreign import ccall "dynamic" mkIsConfiguredForHardwareTriggeringFunc :: FunPtr IsConfiguredForHardwareTriggeringFunc -> IsConfiguredForHardwareTriggeringFunc
foreign import ccall "dynamic" mkAcquireSingleImageFunc :: FunPtr AcquireSingleImageFunc -> AcquireSingleImageFunc
foreign import ccall "dynamic" mkStartAsyncAcquisitionFunc :: FunPtr StartAsyncAcquisitionFunc -> StartAsyncAcquisitionFunc
foreign import ccall "dynamic" mkStartBoundedAsyncAcquisitionFunc :: FunPtr StartBoundedAsyncAcquisitionFunc -> StartBoundedAsyncAcquisitionFunc
foreign import ccall "dynamic" mkGetOldestImageAsyncAcquiredFunc :: FunPtr GetOldestImageAsyncAcquiredFunc -> GetOldestImageAsyncAcquiredFunc
foreign import ccall "dynamic" mkAbortAsyncAcquisitionFunc :: FunPtr AbortAsyncAcquisitionFunc -> AbortAsyncAcquisitionFunc
foreign import ccall "dynamic" mkGetLastSCCamErrorFunc :: FunPtr GetLastSCCamErrorFunc -> GetLastSCCamErrorFunc

loadPlugin :: FilePath -> Text -> IO (EquipmentW, [PluginDetector])
loadPlugin pluginConfigDir libName =
    loadModule libName >>= \modu ->
    loadFunc modu "InitImagerPlugin" mkInitFunc >>= \initF ->
    loadFunc modu "ShutdownImagerPlugin" mkShutdownFunc >>= \shutdownF ->
    loadFunc modu "ImagerPluginAPIVersion" mkSingleIntPtrFunc >>= \apiVersionF ->
    
    loadFunc modu "EquipmentName" mkIdentifierFunc >>= \eqNameF ->
    
    loadFunc modu "ListAvailableLightSources" mkAvailableLightSourcesFunc >>= \listLightSourcesF ->
    loadFunc modu "ListAvailableChannels" mkAvailableChannelsFunc >>= \listChannelsF ->
    loadFunc modu "ActivateLightSource" mkActivateLightSourceFunc >>= \activateLightSourceF ->
    loadFunc modu "DeactivateLightSource" mkDeactivateLightSourceFunc >>= \deactivateLightSourceF ->
    
    loadFunc modu "ListDiscreteMovableComponents" mkListDiscreteMovableComponentsFunc >>= \listDiscreteMovableComponentsF ->
    loadFunc modu "ListContinuouslyMovableComponents" mkListContinuouslyMovableComponentsFunc >>= \listContinouslyMovableComponentsF ->
    loadFunc modu "ListDiscreteMovableComponentSettings" mkListDiscreteMovableComponentSettingsFunc >>= \listContinouslyMovableComponentSettingsF ->
    loadFunc modu "ListContinuouslyMovableComponentRange" mkListContinuouslyMovableComponentRangeFunc >>= \listContinuouslyMovableComponentRangeF ->
    loadFunc modu "SetMovableComponents" mkSetMovableComponentsFunc >>= \setMovableComponentsF ->
    
    loadFunc modu "HasMotorizedStage" mkSingleIntPtrFunc >>= \hasStageF ->
    loadFunc modu "MotorizedStageName" mkIdentifierFunc >>= \stageNameF ->
    loadFunc modu "SupportedStageAxes" mkSupportedStageAxesFunc >>= \suppAxesF ->
    loadFunc modu "GetStagePosition" mkGetStagePositionFunc >>= \getStagePosF ->
    loadFunc modu "SetStagePosition" mkSetStagePositionFunc >>= \setStagePosF ->

    loadFunc modu "ListRobots" mkListRobotsFunc >>= \listRobotsF ->
    loadFunc modu "ListRobotPrograms" mkListRobotProgramsFunc >>= \listRobotProgramsF ->
    loadFuncPtr modu "ReleaseRobotProgramsInfo" >>= \releaseRobotProgramsInfoF ->
    loadFunc modu "ExecuteRobotProgram" mkExecuteRobotProgramFunc >>= \executeRobotProgramF ->
    loadFunc modu "StopRobot" mkStopRobotFunc >>= \stopRobotF ->

    loadFunc modu "ListConnectedCameraNames" mkListConnectedCameraNamesFunc >>= \listConnectedCameraNamesF ->
    loadFunc modu "GetCameraOptions" mkGetCameraOptionsFunc >>= \getCameraOptionsF ->
    loadFuncPtr modu "ReleaseOptionsData" >>= \releaseOptionsDataF ->
    loadFunc modu "SetCameraOption" mkSetCameraOptionFunc >>= \setCameraOptionF ->
    loadFunc modu "GetFrameRate" mkGetFrameRateFunc >>= \getFrameRateF ->
    loadFunc modu "IsConfiguredForHardwareTriggering" mkIsConfiguredForHardwareTriggeringFunc >>= \isConfiguredForHardwareTriggeringF ->
    loadFunc modu "AcquireSingleImage" mkAcquireSingleImageFunc >>= \acquireSingleImageF ->
    loadFunc modu "StartAsyncAcquisition" mkStartAsyncAcquisitionFunc >>= \startAsyncAcquisitionF ->
    loadFunc modu "StartBoundedAsyncAcquisition" mkStartBoundedAsyncAcquisitionFunc >>= \startBoundedAsyncAcquisitionF ->
    loadFunc modu "GetOldestImageAsyncAcquired" mkGetOldestImageAsyncAcquiredFunc >>= \getOldestImageAsyncAcquiredF ->
    loadFuncPtr modu "ReleaseImageData" >>= \releaseImageDataF ->
    loadFunc modu "AbortAsyncAcquisition" mkAbortAsyncAcquisitionFunc >>= \abortAsyncAcquisitionF ->
    loadFunc modu "GetLastSCCamError" mkGetLastSCCamErrorFunc >>= \getLastSCCamErrorF ->


    verifyPluginVersion apiVersionF >>  -- check if the plugin api matches
    castFunPtrToPtr <$> mkCStringCallback (pluginPrinter libName) >>= \printFunc ->
    B.useAsCString (T.encodeUtf8 . T.pack $ pluginConfigDir) $ \confDirPtr ->
    initF confDirPtr printFunc >>= \initResult ->
    when (initResult /= 0) (error "couldn't init plugin") >>
    EqName <$> readIdentifier eqNameF >>= \eqName ->
    when ((fromEqName eqName) /= baseLibName) (
        throwIO $ userError ("The name of the plugin (" ++ T.unpack baseLibName ++ ") must exactly match the name of the equipment it communicates to Imager (" ++ (T.unpack (fromEqName eqName)) ++ ")")) >>
    readAvailableLightSources (listLightSourcesF, listChannelsF) >>= \lightSources ->
    readAvailableMovableComponents (listDiscreteMovableComponentsF, listContinouslyMovableComponentsF, 
                                    listContinouslyMovableComponentSettingsF, listContinuouslyMovableComponentRangeF) >>= \movableComps ->
    ((/=) 0) <$> readSingleIntPtr hasStageF >>= \hasStage ->
    (if hasStage then (StageName <$> readIdentifier stageNameF) else (pure $ StageName "")) >>= \stageName ->
    (if hasStage then (readSupportedAxesFunc suppAxesF) else pure []) >>= \supportedAxes ->
    listRobots (listRobotsF, listRobotProgramsF, releaseRobotProgramsInfoF) >>= \robots ->
    listConnectedCameras listConnectedCameraNamesF >>= \connectedCameraNames ->
    
    let ep = EquipmentPlugin eqName shutdownF lightSources (handleActivateLightSource activateLightSourceF)
                                (handleDeactivateLightSource deactivateLightSourceF)
                                movableComps (handleSetMoveComponents setMovableComponentsF)
                                hasStage stageName supportedAxes
                                (handleGetStagePosition getStagePosF) (handleSetStagePosition setStagePosF)
                                robots
                                (executeRobotProgram executeRobotProgramF)
                                (stopRobot stopRobotF)
                                connectedCameraNames
                                (getCameraOptions getLastSCCamErrorF getCameraOptionsF releaseOptionsDataF)
                                (setCameraOption getLastSCCamErrorF setCameraOptionF)
                                (getFrameRate getLastSCCamErrorF getFrameRateF)
                                (isConfiguredForHardwareTriggering getLastSCCamErrorF isConfiguredForHardwareTriggeringF)
                                (acquireSingleImage getLastSCCamErrorF acquireSingleImageF releaseImageDataF)
                                (startAsyncAcquisition getLastSCCamErrorF startAsyncAcquisitionF)
                                (startBoundedAsyncAcquisition getLastSCCamErrorF startBoundedAsyncAcquisitionF)
                                (getOldestImageAsyncAcquired getLastSCCamErrorF getOldestImageAsyncAcquiredF releaseImageDataF)
                                (abortAsyncAcquisition getLastSCCamErrorF abortAsyncAcquisitionF)
    in  pure (EquipmentW ep, map (\camName -> PluginDetector camName ep) connectedCameraNames)
    where
        baseLibName :: Text
        baseLibName = T.pack . dropExtension . T.unpack $ libName

        verifyPluginVersion :: SingleIntPtrFunc -> IO ()
        verifyPluginVersion f = alloca $ \versionPtr ->
                                checkError (f versionPtr) >>
                                peek versionPtr >>= \version ->
                                when (version /= pluginAPIVersion) (
                                    throwIO $ userError $ "Incorrect plugin api version. A different version of this plugin (" ++ T.unpack baseLibName ++ ") is required for this system.")

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
            alloca $ \allowMultipleChannelsPtr ->
            checkError (fChannels cSourceName namesPtr 16 128 nNamesReturnedPtr canControlPowerPtr allowMultipleChannelsPtr) >>
            LSName <$> T.pack <$> peekCString cSourceName >>= \sourceName ->
            ((/=) 0) <$> peek canControlPowerPtr >>= \canControlPower ->
            ((/=) 0) <$> peek allowMultipleChannelsPtr >>= \allowMultipleChannels ->
            fromIntegral <$> peek nNamesReturnedPtr >>= \nNamesReturned ->
            map LSChannelName <$> peekArrayString nNamesReturned namesPtr >>= \channels ->
            pure (LightSourceDescription sourceName canControlPower allowMultipleChannels channels)
        
        handleActivateLightSource :: ActivateLightSourceFunc -> (LSName -> [(LSChannelName, LSIlluminationPower)] -> IO ())
        handleActivateLightSource f (LSName name) ps =
            T.withCString name $ \cName ->
            withCStringArray (map (T.unpack . fromLSChannelName . fst) ps) $ \cChannels ->
            withArray (map (CDouble . fromLSIlluminationPower . snd) ps) $ \cPowers ->
            checkError (f cName cChannels cPowers (fromIntegral (length ps)))
        
        handleDeactivateLightSource :: DeactivateLightSourceFunc -> IO ()
        handleDeactivateLightSource f = checkError f

        readAvailableMovableComponents :: (ListDiscreteMovableComponentsFunc, ListContinuouslyMovableComponentsFunc, ListDiscreteMovableComponentSettingsFunc, ListContinuouslyMovableComponentRangeFunc) -> IO [MovableComponentDescription]
        readAvailableMovableComponents (listDiscreteCompsF, listContinuousCompsF, listDiscreteMovableComponentSettingsF, listContinouslyMovableComponentSettingsF) =
            handleStringListFuncT listDiscreteCompsF >>= \discreteCompNames ->
            handleStringListFuncT listContinuousCompsF >>= \continuousCompNames ->
            forM discreteCompNames (\dCompName ->
                withCStringArray' 16 128 $ \namesPtr ->
                alloca $ \nNamesReturnedPtr ->
                T.withCString dCompName $ \cStrName ->
                checkError (listDiscreteMovableComponentSettingsF cStrName namesPtr 16 128 nNamesReturnedPtr) >>
                fromIntegral <$> peek nNamesReturnedPtr >>= \nNamesReturned ->
                map T.decodeUtf8 <$> (peekArray nNamesReturned namesPtr >>= mapM B.packCString) >>= \settings ->
                pure (DiscreteMovableComponent dCompName settings)
            ) >>= \discreteCompDescs ->
            forM continuousCompNames (\cCompName ->
                alloca $ \minValPtr ->
                alloca $ \maxValPtr ->
                alloca $ \incrementPtr ->
                T.withCString cCompName $ \cStrName ->
                checkError (listContinouslyMovableComponentSettingsF cStrName minValPtr maxValPtr incrementPtr) >>
                ContinuouslyMovableComponent cCompName <$> (fromCDouble <$> peek minValPtr) 
                                                       <*> (fromCDouble <$> peek maxValPtr)
                                                       <*> (fromCDouble <$> peek incrementPtr)) >>= \continuousCompDescs ->
            pure (discreteCompDescs ++ continuousCompDescs)

        handleSetMoveComponents :: SetMovableComponentsFunc -> [MovableComponentSetting] -> IO ()
        handleSetMoveComponents setMovableComponentsF compSettings =
            let (discComps, contComps) = partition isDiscreteF compSettings
                nDisc = fromIntegral $ length discComps
                nCont = fromIntegral $ length contComps
                dCompNames = map mcsComponentName discComps
                cCompNames = map mcsComponentName contComps
                dCompSettings = map mcsDesiredStrSetting discComps
                cCompSettings = map (realToFrac . mcsDesiredNumSetting) contComps   -- realToFrac converts the Double to a CDouble
            in  withCStringArrayT dCompNames $ \dCompNamesArr ->
                withCStringArrayT dCompSettings $ \dCompSettingsArr ->
                withCStringArrayT cCompNames $ \cCompNamesArr ->
                withArray cCompSettings $ \cCompSettingsArr ->
                checkError (setMovableComponentsF nDisc dCompNamesArr dCompSettingsArr nCont cCompNamesArr cCompSettingsArr)
            where
                isDiscreteF (DiscreteComponentSetting{}) = True
                isDiscreteF _ = False
        
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
        
        listRobots :: (ListRobotsFunc, ListRobotProgramsFunc, ReleaseRobotProgramsInfoFunc) -> IO [RobotDescription]
        listRobots (listRobotsF, listRobotProgramsF, releaseRobotProgramsInfoF) =
            map RobotName <$> (handleStringListFuncT listRobotsF) >>= \robotNames ->
            forM robotNames (\robotName ->
                (B.useAsCString . T.encodeUtf8 . fromRobotName) robotName $ \rName ->
                alloca $ \strPtr ->
                poke strPtr nullPtr >>
                checkError (listRobotProgramsF rName strPtr) >>
                peek strPtr >>= newForeignPtr releaseRobotProgramsInfoF >>= \fPtr ->
                withForeignPtr fPtr (\dataPtr ->
                        decodeStrict' <$> B.unsafePackCString dataPtr >>= \decoded ->
                        when ((not . isJust) decoded) (print decoded) >>
                        pure (fromJust decoded)) >>= \programInfo ->
                        pure (RobotDescription robotName programInfo))

        executeRobotProgram :: ExecuteRobotProgramFunc -> RobotName -> RobotProgramCallParams -> IO ()
        executeRobotProgram f rName callParams =
            B.useAsCString (T.encodeUtf8 (fromRobotName rName)) $ \rNamePtr ->
            B.useAsCString (LB.toStrict $ encode callParams) $ \encodedCallParamsPtr ->
            checkError (f rNamePtr encodedCallParamsPtr)
        
        stopRobot :: StopRobotFunc -> IO ()
        stopRobot f = checkError f
        
        listConnectedCameras ::  ListConnectedCameraNamesFunc -> IO [DetectorName]
        listConnectedCameras f = map DetectorName <$> (handleStringListFuncT f)

        getCameraOptions :: GetLastErrorFunc -> GetCameraOptionsFunc -> ReleaseOptionsDataFunc -> DetectorName -> IO [DetectorProperty]
        getCameraOptions errF getOptionsF releaseOptionsF (DetectorName camName) =
            withCString (T.unpack camName) $ \nameStr ->
            alloca $ \strPtr ->
            poke strPtr nullPtr >>
            checkErrorWithCallback errF (getOptionsF nameStr strPtr) >>
            peek strPtr >>= newForeignPtr releaseOptionsF >>= \fPtr ->
            withForeignPtr fPtr (\dataPtr ->
                decodeStrict' <$> B.unsafePackCString dataPtr >>= \decoded ->
                when ((not . isJust) decoded) (print decoded) >>
                (pure . fromCPList . fromJust) decoded)
        
        setCameraOption :: GetLastErrorFunc -> SetCameraOptionFunc -> DetectorName -> DetectorProperty -> IO ()
        setCameraOption errF f (DetectorName camName) option =
            withCString (T.unpack camName) $ \nameStr ->
            B.useAsCString (LB.toStrict $ encode option) $ \optStr ->
            checkErrorWithCallback errF (f nameStr optStr)
        
        getFrameRate :: GetLastErrorFunc -> GetFrameRateFunc -> DetectorName -> IO Double
        getFrameRate errF f (DetectorName camName) =
            withCString (T.unpack camName) $ \nameStr ->
            alloca $ \frPtr ->
            checkErrorWithCallback errF (f nameStr frPtr) >>
            fromCDouble <$> peek frPtr
        
        isConfiguredForHardwareTriggering :: GetLastErrorFunc -> IsConfiguredForHardwareTriggeringFunc -> DetectorName -> IO Bool
        isConfiguredForHardwareTriggering errF f (DetectorName camName) =
            withCString (T.unpack camName) $ \nameStr ->
            alloca $ \isConfPtr ->
            checkErrorWithCallback errF (f nameStr isConfPtr) >>
            peek isConfPtr >>= \isConf ->
            if (isConf == 0)
                then (pure False)
                else (pure True)
        
        acquireSingleImage :: GetLastErrorFunc -> AcquireSingleImageFunc -> ReleaseImageDataFunc -> DetectorName -> IO MeasuredImage
        acquireSingleImage errF acqF releaseF (DetectorName camName) =
            withCString (T.unpack camName) $ \nameStr ->
            alloca $ \nRowsPtr ->
            alloca $ \nColsPtr ->
            alloca $ \imagePtrPtr ->
            poke imagePtrPtr nullPtr >>
            checkErrorWithCallback errF (acqF nameStr imagePtrPtr nRowsPtr nColsPtr) >>
            peek imagePtrPtr >>= newForeignPtr releaseF >>= \fPtr ->
            fromIntegral <$> peek nRowsPtr >>= \nRows ->
            fromIntegral <$> peek nColsPtr >>= \nCols ->
            pure (MeasuredImage nRows nCols (SecondsSinceStartOfDetection 0.0) (V.unsafeFromForeignPtr0 fPtr (nRows * nCols)))

        startAsyncAcquisition :: GetLastErrorFunc -> StartAsyncAcquisitionFunc -> DetectorName -> IO ()
        startAsyncAcquisition errF f (DetectorName camName) =
            withCString (T.unpack camName) $ \nameStr ->
            checkErrorWithCallback errF (f nameStr)
        
        startBoundedAsyncAcquisition :: GetLastErrorFunc -> StartBoundedAsyncAcquisitionFunc -> DetectorName -> Word64 -> IO ()
        startBoundedAsyncAcquisition errF f (DetectorName camName) nImages =
            withCString (T.unpack camName) $ \nameStr ->
            checkErrorWithCallback errF (f nameStr nImages)

        getOldestImageAsyncAcquired :: GetLastErrorFunc -> GetOldestImageAsyncAcquiredFunc -> ReleaseImageDataFunc -> DetectorName -> Word -> IO (Maybe MeasuredImage)
        getOldestImageAsyncAcquired errF getImageF releaseImageF (DetectorName camName) timeoutMillis =
            withCString (T.unpack camName) $ \nameStr ->
            alloca $ \imagePtrPtr ->
            alloca $ \nRowsPtr ->
            alloca $ \nColsPtr ->
            alloca $ \timeStampPtr ->
            poke imagePtrPtr nullPtr >>
            checkErrorWithCallback errF (getImageF nameStr (fromIntegral timeoutMillis) imagePtrPtr nRowsPtr nColsPtr timeStampPtr) >>
            peek imagePtrPtr >>= \imgPtr ->
            if (imgPtr == nullPtr)
                then return Nothing
                else
                    newForeignPtr releaseImageF imgPtr >>= \fPtr ->
                    fromIntegral <$> peek nRowsPtr >>= \nRows ->
                    fromIntegral <$> peek nColsPtr >>= \nCols ->
                    fromCDouble <$> peek timeStampPtr >>= \timeStamp ->
                    pure (Just (MeasuredImage nRows nCols (SecondsSinceStartOfDetection timeStamp) (V.unsafeFromForeignPtr0 fPtr (nRows * nCols))))
    
        abortAsyncAcquisition :: GetLastErrorFunc -> AbortAsyncAcquisitionFunc -> DetectorName -> IO ()
        abortAsyncAcquisition errF f (DetectorName camName) =
            withCString (T.unpack camName) $ \nameStr ->
            checkErrorWithCallback errF (f nameStr)


pluginPrinter :: Text -> CString -> IO ()
pluginPrinter pluginName str = T.putStr pluginName >> T.putStr ": " >>
                               B.packCString str >>= B.putStrLn


withCStringArray :: [String] -> (Ptr CString -> IO b) -> IO b
withCStringArray ss f = aux ss []
    where
        aux [] cl = withArray cl f
        aux (s:ls) cl = withCString s (\cs -> aux ls (cs:cl))

withCStringArrayT :: [Text] -> (Ptr CString -> IO b) -> IO b
withCStringArrayT ss f = aux ss []
    where
        aux [] cl = withArray cl f
        aux (t : ts) cl = T.withCString t (\cs -> aux ts (cs : cl))

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

handleStringListFuncT :: StringListFunc -> IO [Text]
handleStringListFuncT f =
    handleStringListFunc f >>= mapM B.packCString >>= pure . map T.decodeUtf8

handleStringListFuncWithName :: StringListFuncWithName -> ByteString -> IO [ByteString]
handleStringListFuncWithName f name =
    B.useAsCString name $ \nameStr ->
    withCStringArray' 16 128 $ \namesPtr ->
    alloca $ \nNamesReturnedPtr ->
    checkError (f nameStr namesPtr 16 128 nNamesReturnedPtr) >>
    fromIntegral <$> peek nNamesReturnedPtr >>= \nNamesReturned ->
    peekArray nNamesReturned namesPtr >>= mapM B.packCString

handleStringListFuncWithNameT :: StringListFuncWithName -> Text -> IO [Text]
handleStringListFuncWithNameT f name =
    let nameBS = T.encodeUtf8 name
    in  map T.decodeUtf8 <$> handleStringListFuncWithName f nameBS

readObjects :: StringListFunc -> (CString -> IO b) -> IO [b]
readObjects fList fDetail =
    handleStringListFunc fList >>= \arrayOfcStr ->
    sequence (map fDetail arrayOfcStr)

checkError :: IO CInt -> IO ()
checkError f = f >>= \result ->
               when (result /= 0) (error "error executing plugin")

checkErrorWithCallback :: GetLastSCCamErrorFunc -> IO CInt -> IO ()
checkErrorWithCallback errFunc f =
    f >>= \result ->
    when (result /= 0) (
        allocaArray bufSize (\strPtr ->
            errFunc strPtr (fromIntegral bufSize) >>
            T.unpack . T.decodeUtf8 <$> B.packCString strPtr) >>=
            throwIO . userError
    )
    where
        bufSize = 512