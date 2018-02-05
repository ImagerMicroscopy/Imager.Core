module MicroscopeController where

import Data.ByteString
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Foreign.C.String
import Foreign.C.Types

import Equipment

data MicroscopeController = MicroscopeController {
                                mcHasMotorizedDichroic :: !Bool
                              , mcAvailableDichroicMirrors :: ![Text]
                              , mcHasFilterWheel :: !Bool
                              , mcAvailableFilters :: ![Text]
                              , mcHasMotorizedStage :: !Bool
}

initializeMicroscopeController :: EquipmentDescription -> IO EquipmentW
initializeMicroscopeController MicroscopeControllerDesc = do
    connStatus <- mcConnectToMicroscope
    when (connStatus /= 0) (error "unable to connect to microscope")
    MicroscopeController <$> hasAccessory c_MCHasMotorizedDichroic
                         <*> listDichroicsFilters c_MCListDichroicMirrors
                         <*> hasAccessory c_MCHasFilterWheel
                         <*> listDichroicsFilters c_MCListAvailableFilters
                         <*> hasAccessory c_MCHasMotorizedStage >>= \mc ->
    pure (EquipmentW mc)

instance Equipment MicroscopeController where
    equipmentName _ = "microscope controller"
    closeDevice _ = mcCloseConnection
    hasFilterWheel  = mcHasFilterWheel
    filterWheelName :: e -> Text
    filterWheelChannels :: e -> [Text]
    switchToFilter :: e -> Text -> IO ()

hasAccessory :: (Ptr CInt -> IO CInt) -> IO Bool
hasAccessory f = alloca bPtr >>= \bPtr ->
                 f bPtr >>= \result ->
                 when (result /= 0) (error "hasAccessory failed") >>
                 (0 /=) <$> peek bPtr

listDichroicsFilters :: (Ptr CString -> CInt -> CInt -> Ptr CInt -> IO CInt) -> IO [Text]
listDichroicsFilters f =
    let maxNNames = 20
        nBytesPerName = 100
    in  allocaArray maxNNames >>= \namePtrs ->
        allocaArray (maxNNames * nBytesPerName) >>= \names ->
        forM_ [0 .. (maxNNames - 1)] (\idx ->
            let address = names `plusPtr` (idx * nBytesPerName)
            in  poke (namePtrs `plusPtr` idx) address) >>
        alloca >>= \nReturnedPtr ->
        f names nNames nBytesPerName nReturnedPtr >>= \result ->
        when (result /= 0) (error "couldn't fetch filter or dichroic names") >>
        fromIntegral <$> peek nReturnedPtr >>= \nReturned ->
        if (nReturned == 0)
        then return []
        else forM [0 .. (nReturned - 1)] (\idx->
                 T.decodeUTF8 <$> packCString <*> peek (namePtrs `plusPtr` idx))


foreign import ccall unsafe "MicroscopeControlDLL.h MCConnectToMicroscope"
    c_MCConnectToMicroscope :: IO CInt
foreign import ccall unsafe "MicroscopeControlDLL.h MCCloseConnection"
    c_MCCloseConnection :: IO ()

foreign import ccall unsafe "MicroscopeControlDLL.h MCHasMotorizedDichroic"
    c_MCHasMotorizedDichroic :: Ptr CInt -> IO CInt
foreign import ccall unsafe "MicroscopeControlDLL.h MCListDichroicMirrors"
    c_MCListDichroicMirrors :: Ptr CString -> CInt -> CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "MicroscopeControlDLL.h MCSetDichroicMirror"
    c_MCSetDichroicMirror :: CString -> IO CInt

foreign import ccall unsafe "MicroscopeControlDLL.h MCHasFilterWheel"
    c_MCHasFilterWheel :: Ptr CInt -> IO CInt
foreign import ccall unsafe "MicroscopeControlDLL.h MCListAvailableFilters"
    c_MCListAvailableFilters :: Ptr CString -> CInt -> CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "MicroscopeControlDLL.h MCSetFilter"
    c_MCSetFilter :: CString -> IO CInt

foreign import ccall unsafe "MicroscopeControlDLL.h MCHasMotorizedStage"
    c_MCHasMotorizedStage :: Ptr CInt -> IO CInt
foreign import ccall unsafe "MicroscopeControlDLL.h MCGetStagePosition"
    c_MCGetStagePosition :: Ptr CDouble -> Ptr CDouble -> Ptr Double -> IO CInt
foreign import ccall unsafe "MicroscopeControlDLL.h MCSetStagePosition"
    c_MCSetStagePosition :: CString -> IO CInt
