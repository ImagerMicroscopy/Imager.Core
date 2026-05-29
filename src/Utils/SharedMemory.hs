{-# LANGUAGE CPP #-}

module Utils.SharedMemory(
    SharedMemory
  , createSharedMemory
  , sharedMemoryName
  , sharedMemorySize
  , writeToSharedMemory
  , writeMultipleToSharedMemory
  , writeMultipleToSharedMemoryLBS
) where

import Control.Exception (throwIO)
import Control.Monad (when, replicateM)
import Data.Bits ((.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as LB
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr
import Foreign.C.String (CString, withCString)
import Foreign.C.Types
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr)
import System.Random (randomRIO)

#ifdef WINDOWS
import System.Win32
#endif

data SharedMemory = SharedMemory {
    smName :: Text
  , smSize :: Word64
  , smPtr :: ForeignPtr ()
} deriving (Show)

generateUniqueName :: IO Text
generateUniqueName = do
    chars <- replicateM 16 (randomRIO ('a', 'z'))
#ifdef WINDOWS
    pure $ T.pack $ "ImagerMem_" ++ chars
#else
    pure $ T.pack $ "/ImagerMem_" ++ chars
#endif

createSharedMemory :: Word64 -> IO SharedMemory
#ifdef WINDOWS
eRROR_ALREADY_EXISTS = 183 :: DWORD

createSharedMemory nBytes =
    generateUniqueName >>= \name ->
    createFileMapping Nothing pAGE_READWRITE (fromIntegral nBytes) (Just (T.unpack name)) >>= \hMap ->
    getLastError >>= \errCode ->
    when (errCode == eRROR_ALREADY_EXISTS) (throwIO $ userError "Shared memory with this name already exists") >>
    mapViewOfFile hMap fILE_MAP_ALL_ACCESS 0 (fromIntegral nBytes) >>= \shMemPtr ->
    when (shMemPtr == nullPtr) (throwIO $ userError "Failed to map shared memory") >>
    let unMap = unmapViewOfFile shMemPtr >> closeHandle hMap
    in  newForeignPtr shMemPtr unMap >>= \fptr ->
        pure (SharedMemory name nBytes fptr)

#else

foreign import ccall unsafe "shm_open" c_shm_open :: CString -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "ftruncate" c_ftruncate :: CInt -> CLong -> IO CInt
foreign import ccall unsafe "mmap" c_mmap :: Ptr () -> CSize -> CInt -> CInt -> CInt -> CLong -> IO (Ptr ())
foreign import ccall unsafe "munmap" c_munmap :: Ptr () -> CSize -> IO CInt
foreign import ccall unsafe "close" c_close :: CInt -> IO CInt
foreign import ccall unsafe "shm_unlink" c_shm_unlink :: CString -> IO CInt

createSharedMemory nBytes =
    generateUniqueName >>= \name ->
    withCString (T.unpack name) $ \c_name ->
    -- O_CREAT (0o100) | O_EXCL (0o200) | O_RDWR (0o2) = 0o302
    let flags = 0o302 
        mode = 0o666
    in  c_shm_open c_name flags mode >>= \fd ->
        when (fd < 0) (throwIO $ userError "Failed to create shared memory or it already exists (shm_open)") >>
        c_ftruncate fd (fromIntegral nBytes) >>= \res ->
        when (res /= 0) (c_close fd >> c_shm_unlink c_name >> throwIO (userError "Failed to set size of shared memory (ftruncate)")) >>
        -- PROT_READ (1) | PROT_WRITE (2) = 3
        -- MAP_SHARED = 1
        c_mmap nullPtr (fromIntegral nBytes) 3 1 fd 0 >>= \ptr ->
        when (ptr == intPtrToPtr (-1 :: IntPtr)) (c_close fd >> c_shm_unlink c_name >> throwIO (userError "mmap failed")) >>
        c_close fd >>
        let unMap = c_munmap ptr (fromIntegral nBytes) >> withCString (T.unpack name) (\c_n -> c_shm_unlink c_n >> pure ())
        in  newForeignPtr ptr unMap >>= \fptr ->
            pure (SharedMemory name nBytes fptr)
#endif

sharedMemoryName :: SharedMemory -> Text
sharedMemoryName = smName

sharedMemorySize :: SharedMemory -> Word64
sharedMemorySize = smSize

writeToSharedMemory :: ByteString -> SharedMemory -> IO ()
writeToSharedMemory bs shMem =
    writeToSharedMemoryAtOffset bs shMem 0

writeMultipleToSharedMemory :: [ByteString] -> SharedMemory -> IO ()
writeMultipleToSharedMemory bss shMem = f bss 0
    where
        f [] _ = return ()
        f (bs:bss) offset =
            writeToSharedMemoryAtOffset bs shMem offset >>
            f bss (offset + (fromIntegral (B.length bs)))

writeMultipleToSharedMemoryLBS :: [LB.ByteString] -> SharedMemory -> IO ()
writeMultipleToSharedMemoryLBS lbss shMem = writeMultipleToSharedMemory (map LB.toStrict lbss) shMem

writeToSharedMemoryAtOffset :: ByteString -> SharedMemory -> Word64 -> IO ()
writeToSharedMemoryAtOffset bs (SharedMemory _ size bPtr) offset =
    when ((fromIntegral $ B.length bs) + offset > size) (throwIO $ userError "Data size with offset exceeds shared memory size") >>
    withForeignPtr bPtr ( \ptr ->
        B.unsafeUseAsCStringLen bs $ \(cstr, len) ->
        copyBytes (castPtr ptr `plusPtr` fromIntegral offset) cstr len)
