{-# LANGUAGE BangPatterns #-}

module SCCamera where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson hiding (withArray)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Unsafe as B
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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

import SCCameraTypes

initializeCameraDLL :: IO (Either String ())
initializeCameraDLL = cInitCameraDLL >>= \result ->
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
    peek strPtr >>= newForeignPtr cReleaseOptionsData >>= \fPtr ->
    when (result /= 0) (throwIO $ userError ("cGetCameraOptions returned error code " ++ show result)) >>
    withForeignPtr fPtr (\dataPtr ->
        decodeStrict' <$> B.unsafePackCString dataPtr >>= \decoded ->
        when ((not . isJust) decoded) (print decoded) >>
        (pure . fromCPList . fromJust) decoded)

setCameraOption :: Text -> DetectorProperty -> IO ()
setCameraOption camName option =
    withCString (T.unpack camName) $ \nameStr ->
    B.useAsCString (LB.toStrict $ encode option) $ \optStr ->
    cSetCameraOption nameStr optStr >>= \result ->
    when (result /= 0) (throwIO $ userError ("cSetCameraOption returned error code " ++ show result))

getFrameRate :: Text -> IO Double
getFrameRate camName =
  withCString (T.unpack camName) $ \nameStr ->
  alloca $ \frPtr ->
  cGetFrameRate nameStr frPtr >>= \result ->
  when (result /= 0) (throwIO $ userError ("cGetFrameRate returned error code " ++ show result)) >>
  fromCDouble <$> peek frPtr

setCameraOrientation :: Text -> [OrientationOp] -> IO ()
setCameraOrientation camName ops =
    withCString (T.unpack camName) $ \nameStr ->
    withArray (map encodedOp ops) $ \opsPtr ->
    cSetImageOrientation nameStr opsPtr (fromIntegral $ length ops) >>= \errorCode ->
    when (errorCode /= 0) (
         throwIO (userError ("SetImageOrientation returned error code " ++ show errorCode)))
    where
        encodedOp :: OrientationOp -> CInt
        encodedOp RotateCWOp = 0
        encodedOp RotateCCWOp = 1
        encodedOp FlipHorizontalOp = 2
        encodedOp FlipVerticalOp = 3

acquireSingleImage :: Text -> IO MeasuredImages
acquireSingleImage camName =
    withCString (T.unpack camName) $ \nameStr ->
    alloca $ \nRowsPtr ->
    alloca $ \nColsPtr ->
    alloca $ \imagePtrPtr ->
    poke imagePtrPtr nullPtr >>
    cAcquireSingleImage nameStr imagePtrPtr nRowsPtr nColsPtr >>= \result ->
    when (result /= 0) (throwIO (userError ("AcquireSingleImage returned error code " ++ show result))) >>
    peek imagePtrPtr >>= newForeignPtr cReleaseImageData >>= \fPtr ->
    fromIntegral <$> peek nRowsPtr >>= \nRows ->
    fromIntegral <$> peek nColsPtr >>= \nCols ->
    pure (MeasuredImages nRows nCols 0.0 (V.unsafeFromForeignPtr0 fPtr (nRows * nCols)))

startAsyncAcquisition :: Text -> IO ()
startAsyncAcquisition camName =
    withCString (T.unpack camName) $ \nameStr ->
    cStartAsyncAcquisition nameStr >>= \result ->
       case result of
           0 -> return ()
           e -> throwIO (userError ("startAsyncAcquisition returned error code " ++ show e))

getNextAcquiredImage :: Text -> IO MeasuredImages
getNextAcquiredImage camName =
    withCString (T.unpack camName) $ \nameStr ->
    alloca $ \imagePtrPtr ->
    alloca $ \nRowsPtr ->
    alloca $ \nColsPtr ->
    alloca $ \timeStampPtr ->
    poke imagePtrPtr nullPtr >>
    cGetOldestImageAsyncAcquired nameStr imagePtrPtr nRowsPtr nColsPtr timeStampPtr >>= \result ->
    when (result /= 0) (throwIO (userError ("GetLastImageAsyncAcquired returned error code " ++ show result))) >>
    peek imagePtrPtr >>= newForeignPtr cReleaseImageData >>= \fPtr ->
    fromIntegral <$> peek nRowsPtr >>= \nRows ->
    fromIntegral <$> peek nColsPtr >>= \nCols ->
    fromCDouble <$> peek timeStampPtr >>= \timeStamp ->
    pure (MeasuredImages nRows nCols timeStamp (V.unsafeFromForeignPtr0 fPtr (nRows * nCols)))

abortAsyncAcquisition :: Text -> IO ()
abortAsyncAcquisition camName =
    withCString (T.unpack camName) $ \nameStr ->
    cAbortAsyncAcquisition nameStr >>= \result ->
    case result of
        0 -> return ()
        e -> throwIO (userError ("abortAsyncAcquisition returned error code " ++ show e))

fromCDouble :: CDouble -> Double
fromCDouble (CDouble d) = d

maxNCameras, maxCameraNameLength :: Int
maxNCameras = 10
maxCameraNameLength = 128

foreign import ccall "SCCameraDLL.h InitCameraDLL"
    cInitCameraDLL :: IO CInt

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

foreign import ccall unsafe "SCCameraDLL.h SetImageOrientation"
    cSetImageOrientation :: CString -> Ptr CInt -> CInt -> IO CInt

foreign import ccall "SCCameraDLL.h AcquireSingleImage"
    cAcquireSingleImage :: CString -> Ptr (Ptr Word16) -> Ptr CInt -> Ptr CInt -> IO CInt

foreign import ccall "SCCameraDLL.h StartAsyncAcquisition"
    cStartAsyncAcquisition :: CString -> IO CInt

foreign import ccall "SCCameraDLL.h GetOldestImageAsyncAcquired"
    cGetOldestImageAsyncAcquired :: CString -> Ptr (Ptr Word16) -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

foreign import ccall unsafe "SCCameraDLL.h &ReleaseImageData"
    cReleaseImageData :: FunPtr (Ptr Word16 -> IO ())

foreign import ccall "SCCameraDLL.h AbortAsyncAcquisition"
    cAbortAsyncAcquisition :: CString -> IO CInt
