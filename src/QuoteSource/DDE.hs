{-# LANGUAGE ForeignFunctionInterface, CPP #-}

module QuoteSource.DDE (
  initializeDde,
  destroyDde,
  DdeState,
  DdeCallback,
  ddeResultAck,
  ddeResultTrue,
  ddeResultFalse,
  ddeXtypConnect,
  ddeXtypPoke,
  ddeCpWinAnsi,
  queryString,
  accessData,
  unaccessData,
  withDdeData
) where

#if defined(i386_HOST_ARCH)
# define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
# define WINDOWS_CCONV ccall
#else
# error Unknown mingw32 arch
#endif

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Bits
import Data.Binary.Get
import Data.Typeable
import Data.ByteString hiding (map)
import Data.IORef
import QuoteSource.XlParser
import System.Win32.DLL
import System.Win32.Types
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array

import qualified Data.ByteString.Lazy as BL

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

foreign import WINDOWS_CCONV unsafe "windows.h DdeInitializeW"
  ddeInitialize :: LPDWORD -> FunPtr DdeCallback -> DWORD -> DWORD -> IO CUInt

foreign import WINDOWS_CCONV unsafe "windows.h DdeUninitialize"
  ddeUninitialize :: DWORD -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h DdeCreateStringHandleW"
  ddeCreateStringHandle :: DWORD -> LPSTR -> CInt -> IO HANDLE

foreign import WINDOWS_CCONV unsafe "windows.h DdeFreeStringHandleW"
  ddeFreeStringHandle :: DWORD -> LPSTR -> IO HANDLE

foreign import WINDOWS_CCONV unsafe "windows.h DdeNameService"
  ddeNameService :: DWORD -> HANDLE -> HANDLE -> CInt -> IO HANDLE

foreign import WINDOWS_CCONV unsafe "windows.h DdeCmpStringHandles"
  ddeCmpStringHandles :: HANDLE -> HANDLE -> IO CInt

foreign import WINDOWS_CCONV unsafe "windows.h DdeQueryStringW"
  ddeQueryString :: DWORD -> HANDLE -> CString -> DWORD -> CInt -> IO DWORD

foreign import WINDOWS_CCONV unsafe "windows.h DdeAccessData"
  ddeAccessData :: HANDLE -> LPDWORD -> IO (Ptr CUChar)

foreign import WINDOWS_CCONV unsafe "windows.h DdeUnaccessData"
  ddeUnaccessData :: HANDLE -> IO ()

foreign import WINDOWS_CCONV "wrapper"
  mkCallbackPtr :: DdeCallback -> IO (FunPtr DdeCallback)

data DdeState = DdeState {
  ddeInstance :: DWORD,
  appName :: String,
  topic :: String,
  appNameHandle :: HANDLE,
  topicHandle :: HANDLE,
  callback :: FunPtr DdeCallback,
  dataCallback :: DdeDataCallback
}

type DdeDataCallback = String -> (Int, Int, [XlData]) -> IO Bool
type DdeCallback = CUInt -> CUInt -> HANDLE -> HANDLE -> HANDLE -> HANDLE -> LPDWORD -> LPDWORD -> IO HANDLE

{-|
 - Callback for DDE messages
 - DdeState is wrapped in IORef, because this callback should be passed to ddeInitialize, which in turn returns DDE handle
 -}
ddeCallback :: IORef DdeState -> CUInt -> CUInt -> HANDLE -> HANDLE -> HANDLE -> HANDLE -> LPDWORD -> LPDWORD -> IO HANDLE
ddeCallback state msgType format hConv hsz1 hsz2 hData dwData1 dwData2
    | msgType == ddeXtypConnect = handleConnect state hsz1 hsz2
    | msgType == ddeXtypPoke = handlePoke state hsz1 hData
    | otherwise = return nullHANDLE -- Do not handle other messages, they are boring
  where
    handleConnect state hsz1 hsz2 = do
      myDdeState <- readIORef state
      maybeAppName <- queryString myDdeState 256 hsz2
      case maybeAppName of
        Just incomingAppName -> do
          return $ if incomingAppName == appName myDdeState
            then ddeResultTrue
            else ddeResultFalse
        Nothing -> return ddeResultFalse

    handlePoke state hsz1 hData = do
      myDdeState <- readIORef state
      maybeTopic <- queryString myDdeState 256 hsz1
      case maybeTopic of
        Nothing -> return ddeResultFalse
        Just topic -> withDdeData hData (\xlData -> case runGetOrFail xlParser $ BL.fromStrict xlData of
          Left (_,  _, errmsg) -> return ddeResultFalse
          Right (_, _, table) -> do
            rc <- (dataCallback myDdeState) topic table
            return $ if rc
              then ddeResultAck
              else ddeResultFalse )

initializeDde :: String -> String -> DdeDataCallback -> IO (IORef DdeState)
initializeDde appName topic callback = alloca (\instancePtr -> do
    ddeState <- newIORef $ DdeState {
      ddeInstance = 0,
      appName = appName,
      appNameHandle = nullHANDLE,
      topic = topic,
      topicHandle = nullHANDLE,
      callback = nullFunPtr,
      dataCallback = callback } 
    cb <- mkCallbackPtr (ddeCallback ddeState)
    rc <- ddeInitialize instancePtr cb 0 0
    instanceRaw <- peek instancePtr
    atomicModifyIORef' ddeState (\state -> (state { ddeInstance = instanceRaw, callback = cb }, ()))
    when (rc /= CUInt 0) $ throw $ ApiError "Unable to initialize DDE"

    withCString appName (\appNameRaw -> withCString topic (\topicRaw -> do
      myAppNameHandle <- ddeCreateStringHandle instanceRaw appNameRaw ddeCpWinAnsi
      myTopicHandle <- ddeCreateStringHandle instanceRaw topicRaw ddeCpWinAnsi
      when (myAppNameHandle == nullHANDLE || myTopicHandle == nullHANDLE) $ throw $ ApiError "Unable to create strings handles"

      atomicModifyIORef' ddeState (\state -> (state { appNameHandle = myAppNameHandle, topicHandle = myTopicHandle }, ()))
      rc2 <- ddeNameService instanceRaw myAppNameHandle nullPtr 1
      when (rc2 == nullHANDLE) $ throw $ ApiError $ "Unable to register application name: " ++ appName

      return ddeState)))

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
  pack . map (toEnum . fromEnum) <$> peekArray (fromEnum dataSize) dataPtr)

unaccessData :: HANDLE -> IO ()
unaccessData = ddeUnaccessData

withDdeData :: HANDLE -> (ByteString -> IO a) -> IO a
withDdeData handle = bracket (accessData handle) (\_ -> unaccessData handle)

