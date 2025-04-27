{-# LANGUAGE CPP #-}

module Utils.DLLUtils (
    HMODULE
  , addDirectoryToLoaderPath
  , loadModule
  , loadFunc
  , mkCStringCallback
) where

import Control.Monad
import Data.ByteString(ByteString)
import qualified Data.ByteString as B
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Int
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

newtype HMODULE = HMODULE { fromHMODULE :: (Ptr ()) }
newtype FARPROC = FARPROC {fromFARPROC :: (Ptr ()) }

addDirectoryToLoaderPath :: Text -> IO ()
addDirectoryToLoaderPath dir =
#ifdef WINDOWS
    B.useAsCString (T.encodeUtf8 dir) (\str ->
    cSetDllDirectoryA str >>= \result ->
    when (result == 0) (error "couldn't add dll loader path"))
#else
    pure () --error "addDirectoryToLoaderPath not defined on Linux"
#endif

loadModule :: Text -> IO HMODULE
loadModule mName =
#ifdef WINDOWS
    B.useAsCString (T.encodeUtf8 mName) (\nameStr ->
    cLoadLibraryA nameStr >>= \modu ->
    if (fromHMODULE modu == nullPtr)
    then cGetLastError >>= putStrLn . show >> error ("couldn't load " ++ T.unpack mName)
    else pure modu)
#else
    B.useAsCString (T.encodeUtf8 mName) (\nameStr ->
    cdlopen nameStr >>= \modu ->
    if (fromHMODULE modu == nullPtr)
    then cdlerror >>= putStrLn . show >> error ("couldn't load " ++ T.unpack mName)
    else pure modu)
#endif

loadFunc :: HMODULE -> Text -> (FunPtr a -> a) -> IO a
loadFunc modu name mkFunc = (mkFunc . castFARPROC) <$> loadFunctionAddress modu name
    where
        loadFunctionAddress :: HMODULE -> Text -> IO FARPROC
        loadFunctionAddress modu fName =
#ifdef WINDOWS
            B.useAsCString (T.encodeUtf8 fName) $ \nameStr ->
            cGetProcAddress modu nameStr >>= \address ->
            if (fromFARPROC address == nullPtr)
            then cGetLastError >>= putStrLn . show >> error ("couldn't load " ++ T.unpack fName)
            else pure address
#else
            B.useAsCString (T.encodeUtf8 fName) $ \nameStr ->
            cdlsym modu nameStr >>= \address ->
            if (fromFARPROC address == nullPtr)
            then cdlerror >>= putStrLn . show >> error ("couldn't load " ++ T.unpack fName)
            else pure address
#endif
        castFARPROC :: FARPROC -> FunPtr a
        castFARPROC (FARPROC address) = castPtrToFunPtr address


foreign import ccall "wrapper" mkCStringCallback :: (CString -> IO ()) -> IO (FunPtr (CString -> IO ()))

#ifdef WINDOWS
foreign import ccall "Windows.h SetDllDirectoryA"
    cSetDllDirectoryA :: CString -> IO CInt

foreign import ccall "Windows.h LoadLibraryA"
    cLoadLibraryA  :: CString -> IO HMODULE

foreign import ccall "Windows.h GetProcAddress"
    cGetProcAddress :: HMODULE -> CString -> IO FARPROC

foreign import ccall "Windows.h GetLastError"
    cGetLastError :: IO Int32
#else
foreign import ccall "dlfcn.h dlopen"
    cdlopen  :: CString -> IO HMODULE

foreign import ccall "dlfcn.h dlsym"
    cdlsym :: HMODULE -> CString -> IO FARPROC

foreign import ccall "dlfcn.h dlerror"
    cdlerror :: IO CString
#endif
