{-# LANGUAGE OverloadedStrings #-}
module Equipment.EquipmentPluginsInternal (
    EquipmentPlugin
  , loadPlugin
  , addDirectoryToLoaderPath
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
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable

import Equipment.Equipment
import Equipment.EquipmentTypes
import Utils.MiscUtils

data EquipmentPlugin = EquipmentPlugin {
                           epEquipmentName :: !EqName
                         , epCloseDevice :: IO ()
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
    hasMotorizedStage = epHasMotorizedStage
    motorizedStageName _ = StageName "stage"
    supportedStageAxes = epSupportedStageAxes
    getStagePosition = epGetStagePositionFunc
    setStagePosition = epSetStagePositionFunc

newtype HMODULE = HMODULE { fromHMODULE :: (Ptr ()) }
newtype FARPROC = FARPROC {fromFARPROC :: (Ptr ()) }

type InitFunc = Ptr () -> IO CInt
type ShutdownFunc = IO ()
type IdentifierFunc = CString -> CUInt -> IO CInt
type SingleIntPtrFunc = Ptr CInt -> IO CInt
type SupportedStageAxesFunc = Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
type GetStagePositionFunc = Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> IO CInt
type SetStagePositionFunc = CDouble -> CDouble -> CDouble -> CInt -> CInt -> IO CInt

foreign import stdcall "dynamic" mkInitFunc :: FunPtr InitFunc -> InitFunc
foreign import stdcall "dynamic" mkShutdownFunc :: FunPtr ShutdownFunc -> ShutdownFunc
foreign import stdcall "dynamic" mkIdentifierFunc :: FunPtr IdentifierFunc -> IdentifierFunc
foreign import stdcall "dynamic" mkSingleIntPtrFunc :: FunPtr SingleIntPtrFunc -> SingleIntPtrFunc
foreign import stdcall "dynamic" mkSupportedStageAxesFunc :: FunPtr SupportedStageAxesFunc -> SupportedStageAxesFunc
foreign import stdcall "dynamic" mkGetStagePositionFunc :: FunPtr GetStagePositionFunc -> GetStagePositionFunc
foreign import stdcall "dynamic" mkSetStagePositionFunc :: FunPtr SetStagePositionFunc -> SetStagePositionFunc


loadPlugin :: Text -> IO EquipmentW
loadPlugin libName =
    loadModule libName >>= \modu ->
    castFARPROC <$> loadFunctionAddress modu "InitImagerPlugin" >>= \initFA ->
    castFARPROC <$> loadFunctionAddress modu "ShutdownImagerPlugin" >>= \shutdownFA ->
    castFARPROC <$> loadFunctionAddress modu "ImagerPluginAPIVersion" >>= \apiVersionFA ->
    castFARPROC <$> loadFunctionAddress modu "EquipmentName" >>= \eqNameFA ->
    castFARPROC <$> loadFunctionAddress modu "HasMotorizedStage" >>= \hasStageFA ->
    castFARPROC <$> loadFunctionAddress modu "MotorizedStageName" >>= \stageNameFA ->
    castFARPROC <$> loadFunctionAddress modu "SupportedStageAxes" >>= \suppAxesFA ->
    castFARPROC <$> loadFunctionAddress modu "GetStagePosition" >>= \getStagePosFA ->
    castFARPROC <$> loadFunctionAddress modu "SetStagePosition" >>= \setStagePosFA ->

    let initF = mkInitFunc initFA
        shutdownF = mkShutdownFunc shutdownFA
        apiVersionF = mkSingleIntPtrFunc apiVersionFA
        eqNameF = mkIdentifierFunc eqNameFA
        hasStageF = mkSingleIntPtrFunc hasStageFA
        stageNameF = mkIdentifierFunc stageNameFA
        suppAxesF = mkSupportedStageAxesFunc suppAxesFA
        getStagePosF = mkGetStagePositionFunc getStagePosFA
        setStagePosF = mkSetStagePositionFunc setStagePosFA
    in  castFunPtrToPtr <$> mkCStringCallback (pluginPrinter libName) >>= \printFunc ->
        initF printFunc >>= \initResult ->
        when (initResult /= 0) (error "couldn't init plugin") >>
        verifyPluginVersion apiVersionF >>
        EqName <$> readIdentifier eqNameF >>= \eqName ->
        ((/=) 0) <$> readSingleIntPtr hasStageF >>= \hasStage ->
        StageName <$> readIdentifier stageNameF >>= \stageName ->
        readSupportedAxesFunc suppAxesF >>= \supportedAxes ->
        EquipmentW <$> pure (EquipmentPlugin eqName shutdownF hasStage stageName
                             supportedAxes (handleGetStagePosition getStagePosF) (handleSetStagePosition setStagePosF))
    where
        checkError :: IO CInt -> IO ()
        checkError f = f >>= \result ->
                       when (result /= 0) (error "error executing pluging")
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

addDirectoryToLoaderPath :: Text -> IO ()
addDirectoryToLoaderPath dir =
    B.useAsCString (T.encodeUtf8 dir) (\str ->
    cSetDllDirectoryA str >>= \result ->
    when (result == 0) (error "couldn't add dll loader path"))

loadModule :: Text -> IO HMODULE
loadModule mName =
    B.useAsCString (T.encodeUtf8 mName) (\nameStr ->
    cLoadLibraryA nameStr >>= \modu ->
    if (fromHMODULE modu == nullPtr)
    then cGetLastError >>= putStrLn . show >> error ("couldn't load " ++ T.unpack mName)
    else pure modu)

loadFunctionAddress :: HMODULE -> Text -> IO FARPROC
loadFunctionAddress modu fName =
    B.useAsCString (T.encodeUtf8 fName) $ \nameStr ->
    cGetProcAddress modu nameStr >>= \address ->
    if (fromFARPROC address == nullPtr)
    then error ("could't load " ++ T.unpack fName)
    else pure address

castFARPROC :: FARPROC -> FunPtr a
castFARPROC (FARPROC address) = castPtrToFunPtr address

pluginPrinter :: Text -> CString -> IO ()
pluginPrinter pluginName str = T.putStr pluginName >> T.putStr ": " >>
                               B.packCString str >>= B.putStr

foreign import stdcall "wrapper" mkCStringCallback :: (CString -> IO ()) -> IO (FunPtr (CString -> IO ()))

foreign import stdcall "Windows.h SetDllDirectoryA"
    cSetDllDirectoryA :: CString -> IO CInt

foreign import stdcall "Windows.h LoadLibraryA"
    cLoadLibraryA  :: CString -> IO HMODULE

foreign import stdcall "Windows.h GetProcAddress"
    cGetProcAddress :: HMODULE -> CString -> IO FARPROC

foreign import stdcall "Windows.h GetLastError"
    cGetLastError :: IO Int32
