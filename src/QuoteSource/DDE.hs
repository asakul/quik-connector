{-# LANGUAGE ForeignFunctionInterface #-}

module QuoteSource.DDE (
  initializeDde,
  DdeState,
  DdeCallback,
  ddeResultAck,
  ddeResultTrue,
  ddeResultFalse,
  ddeXtypConnect,
  ddeXtypPoke,
  ddeCpWinAnsi,
  queryString,
  nullDdeState
) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Bits
import Data.Typeable
import Data.ByteString hiding (map)
import System.Win32.DLL
import System.Win32.Types
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array

data DdeException = ApiError String
  deriving (Show, Typeable)

ddeResultAck :: HANDLE
ddeResultAck = wordPtrToPtr $ bit 15

ddeResultTrue :: HANDLE
ddeResultTrue = wordPtrToPtr $ bit 1

ddeResultFalse :: HANDLE
ddeResultFalse = wordPtrToPtr $ bit 0

ddeXtypConnect :: CUInt
ddeXtypConnect = 0x1062

ddeXtypPoke :: CUInt
ddeXtypPoke = 0x4090

ddeCpWinAnsi = 1004

instance Exception DdeException

foreign import ccall unsafe "windows.h DdeInitializeW"
  ddeInitialize :: LPDWORD -> FunPtr DdeCallback -> DWORD -> DWORD -> IO CUInt

foreign import ccall unsafe "windows.h DdeUninitialize"
  ddeUninitialize :: DWORD -> IO BOOL

foreign import ccall unsafe "windows.h DdeCreateStringHandleW"
  ddeCreateStringHandle :: DWORD -> LPSTR -> CInt -> IO HANDLE

foreign import ccall unsafe "windows.h DdeFreeStringHandleW"
  ddeFreeStringHandle :: DWORD -> LPSTR -> IO HANDLE

foreign import ccall unsafe "windows.h DdeNameService"
  ddeNameService :: DWORD -> HANDLE -> HANDLE -> CInt -> IO HANDLE

foreign import ccall unsafe "windows.h DdeCmpStringHandles"
  ddeCmpStringHandles :: HANDLE -> HANDLE -> IO CInt

foreign import ccall unsafe "windows.h DdeQueryStringW"
  ddeQueryString :: DWORD -> HANDLE -> CString -> DWORD -> CInt -> IO DWORD

foreign import ccall unsafe "windows.h DdeAccessData"
  ddeAccessData :: HANDLE -> LPDWORD -> IO (Ptr CUChar)

foreign import ccall unsafe "windows.h DdeUnaccessData"
  ddeUnaccessData :: HANDLE -> IO ()

foreign import ccall "wrapper"
  mkCallbackPtr :: DdeCallback -> IO (FunPtr DdeCallback)

data DdeState = DdeState {
  ddeInstance :: DWORD,
  appName :: HANDLE,
  topicName :: HANDLE,
  callback :: FunPtr DdeCallback
}

nullDdeState = DdeState { ddeInstance = 0, appName = nullPtr, topicName = nullPtr, callback = nullFunPtr }

type DdeCallback = CUInt -> CUInt -> HANDLE -> HANDLE -> HANDLE -> HANDLE -> LPDWORD -> LPDWORD -> IO HANDLE

initializeDde :: String -> String -> DdeCallback -> IO DdeState
initializeDde appName topicName callback = alloca (\instancePtr -> do
    cb <- mkCallbackPtr callback
    rc <- ddeInitialize instancePtr cb 0 0
    instanceRaw <- peek instancePtr
    when (rc /= CUInt 0) $ throw $ ApiError "Unable to initialize DDE"

    withCString appName (\appNameRaw -> withCString topicName (\topicNameRaw -> do
      appNameHandle <- ddeCreateStringHandle instanceRaw appNameRaw ddeCpWinAnsi
      topicNameHandle <- ddeCreateStringHandle instanceRaw topicNameRaw ddeCpWinAnsi
      when (appNameHandle == nullHANDLE || topicNameHandle == nullHANDLE) $ throw $ ApiError "Unable to create strings handles"

      rc2 <- ddeNameService instanceRaw appNameHandle nullPtr 1
      when (rc2 == nullHANDLE) $ throw $ ApiError $ "Unable to register application name: " ++ appName

      return DdeState { ddeInstance = instanceRaw, appName = appNameHandle, topicName = topicNameHandle, callback = cb } )))

destroyDde :: DdeState -> IO ()
destroyDde state = do
  freeHaskellFunPtr $ callback state
  ddeUninitialize $ ddeInstance state
  return ()

queryString :: DdeState -> Int -> HANDLE -> IO (Maybe String)
queryString state maxSize handle = allocaBytes maxSize (\x -> do
  rc <- ddeQueryString (ddeInstance state) handle x (toEnum maxSize) ddeCpWinAnsi
  if rc == 0
    then return Nothing
    else Just <$> peekCAString x)

accessData :: HANDLE -> IO ByteString
accessData handle = alloca (\dataSizePtr -> do
  dataPtr <- ddeAccessData handle dataSizePtr
  dataSize <- peek dataSizePtr
  pack . (map (toEnum . fromEnum)) <$> peekArray (fromEnum dataSize) dataPtr)


