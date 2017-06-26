{-# LANGUAGE OverloadedStrings #-}

module Network.Telegram 
(
  mkTelegramContext,
  sendMessage,
  Manager
) where

import Control.Monad

import Network.Connection
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as BU8
import Data.Aeson
import Data.Aeson.Types

type TelegramApiToken = T.Text
type TelegramChatId = T.Text

data TelegramContext = TelegramContext {
  tgToken :: TelegramApiToken,
  httpMan :: Manager
}

mkTelegramContext :: Manager -> TelegramApiToken -> IO TelegramContext
mkTelegramContext man token = do
  return TelegramContext { httpMan = man, tgToken = token }


sendMessage :: TelegramContext -> TelegramChatId -> T.Text -> IO ()
sendMessage ctx chatId text = do
  req <- parseUrl $ "https://api.telegram.org/bot" ++ (T.unpack $ tgToken ctx) ++ "/sendMessage"
  void $ withResponse (req { method = "POST", requestHeaders = [("Content-Type", BU8.fromString "application/json")], requestBody = (RequestBodyLBS . encode) (object ["chat_id" .= chatId, "text" .= text]) }) (httpMan ctx) (\resp -> brConsume (responseBody resp))

