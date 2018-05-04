{-# LANGUAGE ScopedTypeVariables #-}
module MicroscopeController where

import Control.Exception
import Control.Monad
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Foreign.Marshal
import Foreign.Marshal.Array
import Foreign.C.String
import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr

import Equipment
import EquipmentTypes
import MiscUtils

data MicroscopeController = MicroscopeController {
                                mcEqName :: !EqName
                              , mcLSs :: ![LightSourceDescription]
                              , mcFWs :: ![FilterWheelDescription]
                              , mcHasStage :: !Bool
                              , mcStageAxes :: ![StageAxis]
                          }

initializeMicroscopeController :: EquipmentDescription -> IO EquipmentW
initializeMicroscopeController (MicroscopeControllerDesc name) = do
    mcConnectToMicroscope
    lss <- mcListAvailableLightSources
    fws <- mcFilterWheelsAndFilters
    hasStage <- mcHasMotorizedStage
    axes <- mcSupportedStageAxes
    pure (EquipmentW (MicroscopeController (EqName name) lss fws hasStage axes))

instance Equipment MicroscopeController where
    equipmentName = mcEqName
    flushSerialPorts _ = pure ()
    closeDevice _ = c_MCCloseConnection

    availableLightSources = mcLSs


    availableFilterWheels = mcFWs
    switchToFilter _ fw ft = mcSwitchToFilter fw ft

    hasMotorizedStage = mcHasStage
    motorizedStageName _ = StageName "microscope stage"
    supportedStageAxes = mcStageAxes
    getStagePosition _ = mcGetStagePosition
    setStagePosition _ pos = mcSetStagePosition pos

mcConnectToMicroscope ::  IO ()
mcConnectToMicroscope = c_MCConnectToMicroscope >>= \result ->
                        when (result /= 0) (throwIO $ userError "unable to connect to microscope")

mcListAvailableLightSources :: IO [LightSourceDescription]
mcListAvailableLightSources =
    mcListLightSources >>= \lsNames ->
    forM lsNames (\lsName ->
        mcListChannels lsName >>= \(channels, canControlPower, channelsAreExclusive) ->
        pure (LightSourceDescription lsName canControlPower channelsAreExclusive channels))
    where
        mcListLightSources :: IO [LSName]
        mcListLightSources = map LSName <$> (mcGetNames c_MCListAvailableLightSources)
        mcListChannels :: LSName -> IO ([LSChannelName], Bool, Bool)
        mcListChannels lsName =
            B.useAsCString (T.encodeUtf8 (fromLSName lsName)) $ \cLSName ->
            alloca $ \canControlPowerPtr ->
            alloca $ \channelsAreExclusivePtr ->
            mcGetNames (withLastTwoArgs (c_MCListAvailableChannels cLSName) canControlPowerPtr channelsAreExclusivePtr) >>= \chNames ->
            peek canControlPowerPtr >>= \canControlPower ->
            peek channelsAreExclusivePtr >>= \channelsAreExclusive ->
            pure (map LSChannelName chNames, (canControlPower /= 0), (channelsAreExclusive /= 0))
        withLastTwoArgs :: (a -> b -> c -> d -> e -> f -> g) -> e -> f -> (a -> b -> c -> d -> g)
        withLastTwoArgs f p1 p2 = \a1 a2 a3 a4 -> f a1 a2 a3 a4 p1 p2

mcFilterWheelsAndFilters :: IO [FilterWheelDescription]
mcFilterWheelsAndFilters =
    mcListFilterWheels >>= \fwNames ->
    forM fwNames (\fw ->
        (,) fw <$> mcListFilters fw) >>= \fwsAndFilters ->
    pure (map (\(fw, fs) -> FilterWheelDescription fw fs) fwsAndFilters)
    where
        mcListFilterWheels :: IO [FWName]
        mcListFilterWheels = map FWName <$> (mcGetNames c_MCListAvailableFilterWheels)
        mcListFilters :: FWName -> IO [FName]
        mcListFilters (FWName fwName) =
            B.useAsCString (T.encodeUtf8 fwName) (\cFWName ->
                map FName <$> mcGetNames (c_MCListAvailableFilters cFWName))

mcGetNames :: (Ptr CString -> CInt -> CInt -> Ptr CInt -> IO CInt) -> IO [Text]
mcGetNames f =
    let maxNNames = 20
        nBytesPerName = 128
        ptrSize = sizeOf (undefined :: Ptr CChar)
    in  allocaArray maxNNames (\namePtrs ->
        allocaArray (maxNNames * nBytesPerName) (\(namesStoragePtr :: Ptr CChar) ->
        forM_ [0 .. (maxNNames - 1)] (\idx ->
            let address = namesStoragePtr `plusPtr` (idx * nBytesPerName)
            in  poke (namePtrs `plusPtr` (idx * ptrSize)) address) >>
        alloca (\nReturnedPtr ->
        f namePtrs (fromIntegral maxNNames) (fromIntegral nBytesPerName) nReturnedPtr >>= \result ->
        when (result /= 0) (error "couldn't fetch filterwheel or filter names") >>
        fromIntegral <$> peek nReturnedPtr >>= \nReturned ->
        if (nReturned == 0)
        then return []
        else forM [0 .. (nReturned - 1)] (\idx ->
                 T.pack <$> (peekCString =<< peekElemOff namePtrs idx)))))

mcSwitchToFilter :: FWName -> FName -> IO ()
mcSwitchToFilter (FWName fw) (FName ft) =
    B.useAsCString (T.encodeUtf8 fw) (\cFWName ->
    B.useAsCString (T.encodeUtf8 ft) (\cFTName ->
    c_MCSetFilter cFWName cFTName >>= \result ->
    when (result /= 0) (throwIO $ userError "unable to set MC filter")))

mcHasMotorizedStage :: IO Bool
mcHasMotorizedStage = alloca (\hasItPtr ->
                          c_MCHasMotorizedStage hasItPtr >>= \result ->
                          when (result /= 0) (throwIO $ userError "unable to check MC stage") >>
                          (/=) 0 <$> peek hasItPtr)

mcSupportedStageAxes :: IO [StageAxis]
mcSupportedStageAxes =
    alloca (\infoPtr ->
    c_MCSupportedStageAxes infoPtr >>= \result ->
    when (result /= 0) (throwIO $ userError "unable to read MC stage axes") >>
    peek infoPtr) >>= \info ->
    let x = if ((info .&. 1) /= 0) then [XAxis] else []
        y = if ((info .&. 2) /= 0) then [YAxis] else []
        z = if ((info .&. 4) /= 0) then [ZAxis] else []
    in  pure (concat [x, y, z])

mcGetStagePosition :: IO StagePosition
mcGetStagePosition = alloca (\xPtr ->
                     alloca (\yPtr ->
                     alloca (\zPtr ->
                         c_MCGetStagePosition xPtr yPtr zPtr >>= \result ->
                         when (result /= 0) (throwIO $ userError "unable to read MC stage position") >>
                         (,,) <$> (fromCDouble <$> peek xPtr)
                              <*> (fromCDouble <$> peek yPtr)
                              <*> (fromCDouble <$> peek zPtr))))

mcSetStagePosition :: StagePosition -> IO ()
mcSetStagePosition (x, y, z) =
    c_MCSetStagePosition (CDouble x) (CDouble y) (CDouble z) >>= \result ->
    when (result /= 0) (throwIO $ userError "unable to set MC stage position")

foreign import ccall unsafe "MicroscopeControlDLL.h MCConnectToMicroscope"
    c_MCConnectToMicroscope :: IO CInt
foreign import ccall unsafe "MicroscopeControlDLL.h MCCloseConnection"
    c_MCCloseConnection :: IO ()

foreign import ccall unsafe "MicroscopeControlDLL.h MCListAvailableLightSources"
    c_MCListAvailableLightSources :: Ptr CString -> CInt -> CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "MicroscopeControlDLL.h MCListAvailableChannels"
    c_MCListAvailableChannels :: CString -> Ptr CString -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "MicroscopeControlDLL.h MCActivateLightSource"
    c_MCActivateLightSource :: CString -> Ptr CString -> Ptr CDouble -> CInt -> IO CInt
foreign import ccall unsafe "MicroscopeControlDLL.h MCDeactivateLightSource"
    c_MCDeactivateLightSource :: CString -> IO CInt

foreign import ccall unsafe "MicroscopeControlDLL.h MCListAvailableFilterWheels"
    c_MCListAvailableFilterWheels :: Ptr CString -> CInt -> CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "MicroscopeControlDLL.h MCListAvailableFilters"
    c_MCListAvailableFilters :: CString -> Ptr CString -> CInt -> CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "MicroscopeControlDLL.h MCSetFilter"
    c_MCSetFilter :: CString -> CString -> IO CInt

foreign import ccall unsafe "MicroscopeControlDLL.h MCHasMotorizedStage"
    c_MCHasMotorizedStage :: Ptr CInt -> IO CInt
foreign import ccall unsafe "MicroscopeControlDLL.h MCSupportedStageAxes"
    c_MCSupportedStageAxes :: Ptr CInt -> IO CInt
foreign import ccall unsafe "MicroscopeControlDLL.h MCGetStagePosition"
    c_MCGetStagePosition :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt
foreign import ccall unsafe "MicroscopeControlDLL.h MCSetStagePosition"
    c_MCSetStagePosition :: CDouble -> CDouble -> CDouble -> IO CInt
