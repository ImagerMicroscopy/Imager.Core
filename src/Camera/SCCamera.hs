{-# LANGUAGE BangPatterns #-}

module Camera.SCCamera where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson hiding (withArray)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Unsafe as B
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as MV
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

import Camera.SCCameraTypes
import Utils.DLLUtils

initializeCameraDLL :: IO (Either String ())
initializeCameraDLL = mkCStringCallback scCamPrintFunc >>= \printF ->
                      cInitCameraDLL printF >>= \result ->
                      case result of
                          0 -> return (Right ())
                          e -> return (Left ("initialization failed with code " ++ show e))

shutdownCameraDLL :: IO ()
shutdownCameraDLL = cShutdownCameraDLL

listConnectedCameras :: IO [Text]
listConnectedCameras =
    allocaArray maxNCameras $ \strPtrArray ->
    bracket (forM [1 .. maxNCameras] (\_ -> (mallocArray0 maxCameraNameLength))) (mapM_ free) $ \cStringList ->
        pokeArray strPtrArray cStringList >>
        cListConnectedCameraNames strPtrArray >>= \nCameras ->
        mapM peekCString (take (fromIntegral nCameras) cStringList) >>= \cameraNames ->
        return (map T.pack cameraNames)

getCameraOptions :: Text -> IO [DetectorProperty]
getCameraOptions camName =
    withCString (T.unpack camName) $ \nameStr ->
    alloca $ \strPtr ->
    poke strPtr nullPtr >>
    cGetCameraOptions nameStr strPtr >>= \result ->
    when (result /= 0) (
        T.unpack <$> getLastSCCamError >>=
        throwIO . userError) >>
    peek strPtr >>= newForeignPtr cReleaseOptionsData >>= \fPtr ->
    withForeignPtr fPtr (\dataPtr ->
        decodeStrict' <$> B.unsafePackCString dataPtr >>= \decoded ->
        when ((not . isJust) decoded) (print decoded) >>
        (pure . fromCPList . fromJust) decoded)

setCameraOption :: Text -> DetectorProperty -> IO ()
setCameraOption camName option =
    withCString (T.unpack camName) $ \nameStr ->
    B.useAsCString (LB.toStrict $ encode option) $ \optStr ->
    cSetCameraOption nameStr optStr >>= \result ->
    when (result /= 0) (
        T.unpack <$> getLastSCCamError >>=
        throwIO . userError)

getFrameRate :: Text -> IO Double
getFrameRate camName =
  withCString (T.unpack camName) $ \nameStr ->
  alloca $ \frPtr ->
  cGetFrameRate nameStr frPtr >>= \result ->
  when (result /= 0) (
        T.unpack <$> getLastSCCamError >>=
        throwIO . userError) >>
  fromCDouble <$> peek frPtr

isConfiguredForHardwareTriggering :: Text -> IO Bool
isConfiguredForHardwareTriggering camName =
    withCString (T.unpack camName) $ \nameStr ->
    alloca $ \isConfPtr ->
    cIsConfiguredForHardwareTriggering nameStr isConfPtr >>= \result ->
    when (result /= 0) (
        T.unpack <$> getLastSCCamError >>=
        throwIO . userError) >>
    peek isConfPtr >>= \isConf ->
    if (isConf == 0)
        then (pure False)
        else (pure True)

setCameraOrientation :: Text -> [OrientationOp] -> IO ()
setCameraOrientation camName ops =
    withCString (T.unpack camName) $ \nameStr ->
    withArray (map encodedOp ops) $ \opsPtr ->
    cSetImageOrientation nameStr opsPtr (fromIntegral $ length ops) >>= \errorCode ->
    when (errorCode /= 0) (
        T.unpack <$> getLastSCCamError >>=
        throwIO . userError)
    where
        encodedOp :: OrientationOp -> CInt
        encodedOp RotateCWOp = 0
        encodedOp RotateCCWOp = 1
        encodedOp FlipHorizontalOp = 2
        encodedOp FlipVerticalOp = 3

acquireSingleImage :: Text -> IO MeasuredImage
acquireSingleImage camName =
    withCString (T.unpack camName) $ \nameStr ->
    alloca $ \nRowsPtr ->
    alloca $ \nColsPtr ->
    alloca $ \imagePtrPtr ->
    poke imagePtrPtr nullPtr >>
    cAcquireSingleImage nameStr imagePtrPtr nRowsPtr nColsPtr >>= \result ->
    when (result /= 0) (
        T.unpack <$> getLastSCCamError >>=
        throwIO . userError) >>
    peek imagePtrPtr >>= newForeignPtr cReleaseImageData >>= \fPtr ->
    fromIntegral <$> peek nRowsPtr >>= \nRows ->
    fromIntegral <$> peek nColsPtr >>= \nCols ->
    pure (MeasuredImage nRows nCols 0.0 (V.unsafeFromForeignPtr0 fPtr (nRows * nCols)))

startAsyncAcquisition :: Text -> IO ()
startAsyncAcquisition camName =
    withCString (T.unpack camName) $ \nameStr ->
    cStartAsyncAcquisition nameStr >>= \result ->
    when (result /= 0) (
        T.unpack <$> getLastSCCamError >>=
        throwIO . userError)

startBoundedAsyncAcquisition :: Text -> Word64 -> IO ()
startBoundedAsyncAcquisition camName nImages =
    withCString (T.unpack camName) $ \nameStr ->
    cStartBoundedAsyncAcquisition nameStr nImages >>= \result ->
    when (result /= 0) (
        T.unpack <$> getLastSCCamError >>=
        throwIO . userError)

getNextAcquiredImage :: Text -> Word -> IO (Maybe MeasuredImage)
getNextAcquiredImage camName timeoutMillis =
    withCString (T.unpack camName) $ \nameStr ->
    alloca $ \imagePtrPtr ->
    alloca $ \nRowsPtr ->
    alloca $ \nColsPtr ->
    alloca $ \timeStampPtr ->
    poke imagePtrPtr nullPtr >>
    cGetOldestImageAsyncAcquired nameStr (fromIntegral timeoutMillis) imagePtrPtr nRowsPtr nColsPtr timeStampPtr >>= \result ->
    when (result /= 0) (
        T.unpack <$> getLastSCCamError >>=
        throwIO . userError) >>
    peek imagePtrPtr >>= \imgPtr ->
    if (imgPtr == nullPtr)
        then return Nothing
        else
            newForeignPtr cReleaseImageData imgPtr >>= \fPtr ->
            fromIntegral <$> peek nRowsPtr >>= \nRows ->
            fromIntegral <$> peek nColsPtr >>= \nCols ->
            fromCDouble <$> peek timeStampPtr >>= \timeStamp ->
            pure (Just (MeasuredImage nRows nCols timeStamp (V.unsafeFromForeignPtr0 fPtr (nRows * nCols))))

abortAsyncAcquisition :: Text -> IO ()
abortAsyncAcquisition camName =
    withCString (T.unpack camName) $ \nameStr ->
    cAbortAsyncAcquisition nameStr >>= \result ->
    when (result /= 0) (
        T.unpack <$> getLastSCCamError >>=
        throwIO . userError)

fromCDouble :: CDouble -> Double
fromCDouble (CDouble d) = d

maxNCameras, maxCameraNameLength :: Int
maxNCameras = 10
maxCameraNameLength = 128

getLastSCCamError :: IO Text
getLastSCCamError =
    allocaArray bufSize (\strPtr ->
        cGetLastSCCamError strPtr (fromIntegral bufSize) >>
        T.decodeUtf8 <$> B.packCString strPtr)
    where
        bufSize = 512

scCamPrintFunc :: CString -> IO ()
scCamPrintFunc str = T.putStr "SCCam: " >> B.packCString str >>= B.putStrLn

foreign import ccall "SCCameraDLL.h InitCameraDLL"
    cInitCameraDLL :: FunPtr (CString -> IO ()) -> IO CInt

foreign import ccall "SCCameraDLL.h ShutdownCameraDLL"
    cShutdownCameraDLL :: IO ()

foreign import ccall unsafe "SCCameraDLL.h ListConnectedCameraNames"
    cListConnectedCameraNames :: Ptr CString -> IO CInt

foreign import ccall unsafe "SCCameraDLL.h GetCameraOptions"
    cGetCameraOptions :: CString -> Ptr CString -> IO CInt

foreign import ccall unsafe "SCCameraDLL.h &ReleaseOptionsData"
    cReleaseOptionsData :: FunPtr (CString -> IO ())

foreign import ccall "SCCameraDLL.h SetCameraOption"
    cSetCameraOption :: CString -> CString -> IO CInt

foreign import ccall unsafe "SCCameraDLL.h GetFrameRate"
    cGetFrameRate :: CString -> Ptr CDouble -> IO CInt

foreign import ccall unsafe "SCCameraDLL.h IsConfiguredForHardwareTriggering"
    cIsConfiguredForHardwareTriggering :: CString -> Ptr CInt -> IO CInt

foreign import ccall unsafe "SCCameraDLL.h SetImageOrientation"
    cSetImageOrientation :: CString -> Ptr CInt -> CInt -> IO CInt

foreign import ccall "SCCameraDLL.h AcquireSingleImage"
    cAcquireSingleImage :: CString -> Ptr (Ptr Word16) -> Ptr CInt -> Ptr CInt -> IO CInt

foreign import ccall "SCCameraDLL.h StartAsyncAcquisition"
    cStartAsyncAcquisition :: CString -> IO CInt

foreign import ccall "SCCameraDLL.h StartBoundedAsyncAcquisition"
    cStartBoundedAsyncAcquisition :: CString -> Word64 -> IO CInt

foreign import ccall "SCCameraDLL.h GetOldestImageAsyncAcquired"
    cGetOldestImageAsyncAcquired :: CString -> Word32 -> Ptr (Ptr Word16) -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

foreign import ccall unsafe "SCCameraDLL.h &ReleaseImageData"
    cReleaseImageData :: FunPtr (Ptr Word16 -> IO ())

foreign import ccall "SCCameraDLL.h AbortAsyncAcquisition"
    cAbortAsyncAcquisition :: CString -> IO CInt

foreign import ccall "SCCameraDLL.h GetLastSCCamError"
    cGetLastSCCamError :: CString -> CSize -> IO ()
