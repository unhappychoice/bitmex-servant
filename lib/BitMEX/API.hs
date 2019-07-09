{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -fcontext-stack=328 #-}

module BitMEX.API
  -- * Client and Server
  ( ServerConfig(..)
  , BitMEXBackend
  , createBitMEXClient
  , runBitMEXServer
  , runBitMEXClient
  , runBitMEXClientWithManager
  , BitMEXClient
  -- ** Servant
  , BitMEXAPI
  ) where

import BitMEX.Types

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class
import Data.Aeson (Value)
import Data.Coerce (coerce)
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (IsString(..))
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types.Method (methodOptions)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (ServantErr, serve)
import Servant.API
import Servant.API.Verbs (StdMethod(..), Verb)
import Servant.Client (Scheme(Http), ServantError, client)
import Servant.Common.BaseUrl (BaseUrl(..))
import Web.HttpApiData



data FormAPIKeyDisable = FormAPIKeyDisable
  { aPIKeyDisableApiKeyID :: Text
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormAPIKeyDisable where
  fromFormUrlEncoded inputs = FormAPIKeyDisable <$> lookupEither "apiKeyID" inputs

instance ToFormUrlEncoded FormAPIKeyDisable where
  toFormUrlEncoded value =
    [ ("apiKeyID", toQueryParam $ aPIKeyDisableApiKeyID value)
    ]
data FormAPIKeyEnable = FormAPIKeyEnable
  { aPIKeyEnableApiKeyID :: Text
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormAPIKeyEnable where
  fromFormUrlEncoded inputs = FormAPIKeyEnable <$> lookupEither "apiKeyID" inputs

instance ToFormUrlEncoded FormAPIKeyEnable where
  toFormUrlEncoded value =
    [ ("apiKeyID", toQueryParam $ aPIKeyEnableApiKeyID value)
    ]
data FormAPIKeyNew = FormAPIKeyNew
  { aPIKeyNewName :: Text
  , aPIKeyNewCidr :: Text
  , aPIKeyNewPermissions :: Text
  , aPIKeyNewEnabled :: Bool
  , aPIKeyNewToken :: Text
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormAPIKeyNew where
  fromFormUrlEncoded inputs = FormAPIKeyNew <$> lookupEither "name" inputs <*> lookupEither "cidr" inputs <*> lookupEither "permissions" inputs <*> lookupEither "enabled" inputs <*> lookupEither "token" inputs

instance ToFormUrlEncoded FormAPIKeyNew where
  toFormUrlEncoded value =
    [ ("name", toQueryParam $ aPIKeyNewName value)
    , ("cidr", toQueryParam $ aPIKeyNewCidr value)
    , ("permissions", toQueryParam $ aPIKeyNewPermissions value)
    , ("enabled", toQueryParam $ aPIKeyNewEnabled value)
    , ("token", toQueryParam $ aPIKeyNewToken value)
    ]
data FormAPIKeyRemove = FormAPIKeyRemove
  { aPIKeyRemoveApiKeyID :: Text
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormAPIKeyRemove where
  fromFormUrlEncoded inputs = FormAPIKeyRemove <$> lookupEither "apiKeyID" inputs

instance ToFormUrlEncoded FormAPIKeyRemove where
  toFormUrlEncoded value =
    [ ("apiKeyID", toQueryParam $ aPIKeyRemoveApiKeyID value)
    ]
data FormChatNew = FormChatNew
  { chatNewMessage :: Text
  , chatNewChannelID :: Double
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormChatNew where
  fromFormUrlEncoded inputs = FormChatNew <$> lookupEither "message" inputs <*> lookupEither "channelID" inputs

instance ToFormUrlEncoded FormChatNew where
  toFormUrlEncoded value =
    [ ("message", toQueryParam $ chatNewMessage value)
    , ("channelID", toQueryParam $ chatNewChannelID value)
    ]
data FormOrderAmend = FormOrderAmend
  { orderAmendOrderID :: Text
  , orderAmendOrigClOrdID :: Text
  , orderAmendClOrdID :: Text
  , orderAmendSimpleOrderQty :: Double
  , orderAmendOrderQty :: Double
  , orderAmendSimpleLeavesQty :: Double
  , orderAmendLeavesQty :: Double
  , orderAmendPrice :: Double
  , orderAmendStopPx :: Double
  , orderAmendPegOffsetValue :: Double
  , orderAmendText :: Text
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormOrderAmend where
  fromFormUrlEncoded inputs = FormOrderAmend <$> lookupEither "orderID" inputs <*> lookupEither "origClOrdID" inputs <*> lookupEither "clOrdID" inputs <*> lookupEither "simpleOrderQty" inputs <*> lookupEither "orderQty" inputs <*> lookupEither "simpleLeavesQty" inputs <*> lookupEither "leavesQty" inputs <*> lookupEither "price" inputs <*> lookupEither "stopPx" inputs <*> lookupEither "pegOffsetValue" inputs <*> lookupEither "text" inputs

instance ToFormUrlEncoded FormOrderAmend where
  toFormUrlEncoded value =
    [ ("orderID", toQueryParam $ orderAmendOrderID value)
    , ("origClOrdID", toQueryParam $ orderAmendOrigClOrdID value)
    , ("clOrdID", toQueryParam $ orderAmendClOrdID value)
    , ("simpleOrderQty", toQueryParam $ orderAmendSimpleOrderQty value)
    , ("orderQty", toQueryParam $ orderAmendOrderQty value)
    , ("simpleLeavesQty", toQueryParam $ orderAmendSimpleLeavesQty value)
    , ("leavesQty", toQueryParam $ orderAmendLeavesQty value)
    , ("price", toQueryParam $ orderAmendPrice value)
    , ("stopPx", toQueryParam $ orderAmendStopPx value)
    , ("pegOffsetValue", toQueryParam $ orderAmendPegOffsetValue value)
    , ("text", toQueryParam $ orderAmendText value)
    ]
data FormOrderAmendBulk = FormOrderAmendBulk
  { orderAmendBulkOrders :: Text
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormOrderAmendBulk where
  fromFormUrlEncoded inputs = FormOrderAmendBulk <$> lookupEither "orders" inputs

instance ToFormUrlEncoded FormOrderAmendBulk where
  toFormUrlEncoded value =
    [ ("orders", toQueryParam $ orderAmendBulkOrders value)
    ]
data FormOrderCancel = FormOrderCancel
  { orderCancelOrderID :: Text
  , orderCancelClOrdID :: Text
  , orderCancelText :: Text
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormOrderCancel where
  fromFormUrlEncoded inputs = FormOrderCancel <$> lookupEither "orderID" inputs <*> lookupEither "clOrdID" inputs <*> lookupEither "text" inputs

instance ToFormUrlEncoded FormOrderCancel where
  toFormUrlEncoded value =
    [ ("orderID", toQueryParam $ orderCancelOrderID value)
    , ("clOrdID", toQueryParam $ orderCancelClOrdID value)
    , ("text", toQueryParam $ orderCancelText value)
    ]
data FormOrderCancelAll = FormOrderCancelAll
  { orderCancelAllSymbol :: Text
  , orderCancelAllFilter :: Text
  , orderCancelAllText :: Text
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormOrderCancelAll where
  fromFormUrlEncoded inputs = FormOrderCancelAll <$> lookupEither "symbol" inputs <*> lookupEither "filter" inputs <*> lookupEither "text" inputs

instance ToFormUrlEncoded FormOrderCancelAll where
  toFormUrlEncoded value =
    [ ("symbol", toQueryParam $ orderCancelAllSymbol value)
    , ("filter", toQueryParam $ orderCancelAllFilter value)
    , ("text", toQueryParam $ orderCancelAllText value)
    ]
data FormOrderCancelAllAfter = FormOrderCancelAllAfter
  { orderCancelAllAfterTimeout :: Double
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormOrderCancelAllAfter where
  fromFormUrlEncoded inputs = FormOrderCancelAllAfter <$> lookupEither "timeout" inputs

instance ToFormUrlEncoded FormOrderCancelAllAfter where
  toFormUrlEncoded value =
    [ ("timeout", toQueryParam $ orderCancelAllAfterTimeout value)
    ]
data FormOrderClosePosition = FormOrderClosePosition
  { orderClosePositionSymbol :: Text
  , orderClosePositionPrice :: Double
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormOrderClosePosition where
  fromFormUrlEncoded inputs = FormOrderClosePosition <$> lookupEither "symbol" inputs <*> lookupEither "price" inputs

instance ToFormUrlEncoded FormOrderClosePosition where
  toFormUrlEncoded value =
    [ ("symbol", toQueryParam $ orderClosePositionSymbol value)
    , ("price", toQueryParam $ orderClosePositionPrice value)
    ]
data FormOrderNew = FormOrderNew
  { orderNewSymbol :: Text
  , orderNewSide :: Text
  , orderNewSimpleOrderQty :: Double
  , orderNewOrderQty :: Double
  , orderNewPrice :: Double
  , orderNewDisplayQty :: Double
  , orderNewStopPx :: Double
  , orderNewClOrdID :: Text
  , orderNewClOrdLinkID :: Text
  , orderNewPegOffsetValue :: Double
  , orderNewPegPriceType :: Text
  , orderNewOrdType :: Text
  , orderNewTimeInForce :: Text
  , orderNewExecInst :: Text
  , orderNewContingencyType :: Text
  , orderNewText :: Text
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormOrderNew where
  fromFormUrlEncoded inputs = FormOrderNew <$> lookupEither "symbol" inputs <*> lookupEither "side" inputs <*> lookupEither "simpleOrderQty" inputs <*> lookupEither "orderQty" inputs <*> lookupEither "price" inputs <*> lookupEither "displayQty" inputs <*> lookupEither "stopPx" inputs <*> lookupEither "clOrdID" inputs <*> lookupEither "clOrdLinkID" inputs <*> lookupEither "pegOffsetValue" inputs <*> lookupEither "pegPriceType" inputs <*> lookupEither "ordType" inputs <*> lookupEither "timeInForce" inputs <*> lookupEither "execInst" inputs <*> lookupEither "contingencyType" inputs <*> lookupEither "text" inputs

instance ToFormUrlEncoded FormOrderNew where
  toFormUrlEncoded value =
    [ ("symbol", toQueryParam $ orderNewSymbol value)
    , ("side", toQueryParam $ orderNewSide value)
    , ("simpleOrderQty", toQueryParam $ orderNewSimpleOrderQty value)
    , ("orderQty", toQueryParam $ orderNewOrderQty value)
    , ("price", toQueryParam $ orderNewPrice value)
    , ("displayQty", toQueryParam $ orderNewDisplayQty value)
    , ("stopPx", toQueryParam $ orderNewStopPx value)
    , ("clOrdID", toQueryParam $ orderNewClOrdID value)
    , ("clOrdLinkID", toQueryParam $ orderNewClOrdLinkID value)
    , ("pegOffsetValue", toQueryParam $ orderNewPegOffsetValue value)
    , ("pegPriceType", toQueryParam $ orderNewPegPriceType value)
    , ("ordType", toQueryParam $ orderNewOrdType value)
    , ("timeInForce", toQueryParam $ orderNewTimeInForce value)
    , ("execInst", toQueryParam $ orderNewExecInst value)
    , ("contingencyType", toQueryParam $ orderNewContingencyType value)
    , ("text", toQueryParam $ orderNewText value)
    ]
data FormOrderNewBulk = FormOrderNewBulk
  { orderNewBulkOrders :: Text
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormOrderNewBulk where
  fromFormUrlEncoded inputs = FormOrderNewBulk <$> lookupEither "orders" inputs

instance ToFormUrlEncoded FormOrderNewBulk where
  toFormUrlEncoded value =
    [ ("orders", toQueryParam $ orderNewBulkOrders value)
    ]
data FormPositionIsolateMargin = FormPositionIsolateMargin
  { positionIsolateMarginSymbol :: Text
  , positionIsolateMarginEnabled :: Bool
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormPositionIsolateMargin where
  fromFormUrlEncoded inputs = FormPositionIsolateMargin <$> lookupEither "symbol" inputs <*> lookupEither "enabled" inputs

instance ToFormUrlEncoded FormPositionIsolateMargin where
  toFormUrlEncoded value =
    [ ("symbol", toQueryParam $ positionIsolateMarginSymbol value)
    , ("enabled", toQueryParam $ positionIsolateMarginEnabled value)
    ]
data FormPositionTransferIsolatedMargin = FormPositionTransferIsolatedMargin
  { positionTransferIsolatedMarginSymbol :: Text
  , positionTransferIsolatedMarginAmount :: Double
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormPositionTransferIsolatedMargin where
  fromFormUrlEncoded inputs = FormPositionTransferIsolatedMargin <$> lookupEither "symbol" inputs <*> lookupEither "amount" inputs

instance ToFormUrlEncoded FormPositionTransferIsolatedMargin where
  toFormUrlEncoded value =
    [ ("symbol", toQueryParam $ positionTransferIsolatedMarginSymbol value)
    , ("amount", toQueryParam $ positionTransferIsolatedMarginAmount value)
    ]
data FormPositionUpdateLeverage = FormPositionUpdateLeverage
  { positionUpdateLeverageSymbol :: Text
  , positionUpdateLeverageLeverage :: Double
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormPositionUpdateLeverage where
  fromFormUrlEncoded inputs = FormPositionUpdateLeverage <$> lookupEither "symbol" inputs <*> lookupEither "leverage" inputs

instance ToFormUrlEncoded FormPositionUpdateLeverage where
  toFormUrlEncoded value =
    [ ("symbol", toQueryParam $ positionUpdateLeverageSymbol value)
    , ("leverage", toQueryParam $ positionUpdateLeverageLeverage value)
    ]
data FormPositionUpdateRiskLimit = FormPositionUpdateRiskLimit
  { positionUpdateRiskLimitSymbol :: Text
  , positionUpdateRiskLimitRiskLimit :: Double
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormPositionUpdateRiskLimit where
  fromFormUrlEncoded inputs = FormPositionUpdateRiskLimit <$> lookupEither "symbol" inputs <*> lookupEither "riskLimit" inputs

instance ToFormUrlEncoded FormPositionUpdateRiskLimit where
  toFormUrlEncoded value =
    [ ("symbol", toQueryParam $ positionUpdateRiskLimitSymbol value)
    , ("riskLimit", toQueryParam $ positionUpdateRiskLimitRiskLimit value)
    ]
data FormUserCancelWithdrawal = FormUserCancelWithdrawal
  { userCancelWithdrawalToken :: Text
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormUserCancelWithdrawal where
  fromFormUrlEncoded inputs = FormUserCancelWithdrawal <$> lookupEither "token" inputs

instance ToFormUrlEncoded FormUserCancelWithdrawal where
  toFormUrlEncoded value =
    [ ("token", toQueryParam $ userCancelWithdrawalToken value)
    ]
data FormUserCommunicationToken = FormUserCommunicationToken
  { userCommunicationTokenToken :: Text
  , userCommunicationTokenPlatformAgent :: Text
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormUserCommunicationToken where
  fromFormUrlEncoded inputs = FormUserCommunicationToken <$> lookupEither "token" inputs <*> lookupEither "platformAgent" inputs

instance ToFormUrlEncoded FormUserCommunicationToken where
  toFormUrlEncoded value =
    [ ("token", toQueryParam $ userCommunicationTokenToken value)
    , ("platformAgent", toQueryParam $ userCommunicationTokenPlatformAgent value)
    ]
data FormUserConfirm = FormUserConfirm
  { userConfirmToken :: Text
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormUserConfirm where
  fromFormUrlEncoded inputs = FormUserConfirm <$> lookupEither "token" inputs

instance ToFormUrlEncoded FormUserConfirm where
  toFormUrlEncoded value =
    [ ("token", toQueryParam $ userConfirmToken value)
    ]
data FormUserConfirmWithdrawal = FormUserConfirmWithdrawal
  { userConfirmWithdrawalToken :: Text
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormUserConfirmWithdrawal where
  fromFormUrlEncoded inputs = FormUserConfirmWithdrawal <$> lookupEither "token" inputs

instance ToFormUrlEncoded FormUserConfirmWithdrawal where
  toFormUrlEncoded value =
    [ ("token", toQueryParam $ userConfirmWithdrawalToken value)
    ]
data FormUserRequestWithdrawal = FormUserRequestWithdrawal
  { userRequestWithdrawalOtpToken :: Text
  , userRequestWithdrawalCurrency :: Text
  , userRequestWithdrawalAmount :: Double
  , userRequestWithdrawalAddress :: Text
  , userRequestWithdrawalFee :: Double
  , userRequestWithdrawalText :: Text
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormUserRequestWithdrawal where
  fromFormUrlEncoded inputs = FormUserRequestWithdrawal <$> lookupEither "otpToken" inputs <*> lookupEither "currency" inputs <*> lookupEither "amount" inputs <*> lookupEither "address" inputs <*> lookupEither "fee" inputs <*> lookupEither "text" inputs

instance ToFormUrlEncoded FormUserRequestWithdrawal where
  toFormUrlEncoded value =
    [ ("otpToken", toQueryParam $ userRequestWithdrawalOtpToken value)
    , ("currency", toQueryParam $ userRequestWithdrawalCurrency value)
    , ("amount", toQueryParam $ userRequestWithdrawalAmount value)
    , ("address", toQueryParam $ userRequestWithdrawalAddress value)
    , ("fee", toQueryParam $ userRequestWithdrawalFee value)
    , ("text", toQueryParam $ userRequestWithdrawalText value)
    ]
data FormUserSavePreferences = FormUserSavePreferences
  { userSavePreferencesPrefs :: Text
  , userSavePreferencesOverwrite :: Bool
  } deriving (Show, Eq, Generic)

instance FromFormUrlEncoded FormUserSavePreferences where
  fromFormUrlEncoded inputs = FormUserSavePreferences <$> lookupEither "prefs" inputs <*> lookupEither "overwrite" inputs

instance ToFormUrlEncoded FormUserSavePreferences where
  toFormUrlEncoded value =
    [ ("prefs", toQueryParam $ userSavePreferencesPrefs value)
    , ("overwrite", toQueryParam $ userSavePreferencesOverwrite value)
    ]

-- For the form data code generation.
lookupEither :: FromHttpApiData b => Text -> [(Text, Text)] -> Either String b
lookupEither key assocs =
  case lookup key assocs of
    Nothing -> Left $ "Could not find parameter " <> (T.unpack key) <> " in form data"
    Just value ->
      case parseQueryParam value of
        Left result -> Left $ T.unpack result
        Right result -> Right $ result

-- | Servant type-level API, generated from the Swagger spec for BitMEX.
type BitMEXAPI
    =    "apiKey" :> "disable" :> ReqBody '[FormUrlEncoded] FormAPIKeyDisable :> Verb 'POST 200 '[JSON] APIKey -- 'aPIKey.disable' route
    :<|> "apiKey" :> "enable" :> ReqBody '[FormUrlEncoded] FormAPIKeyEnable :> Verb 'POST 200 '[JSON] APIKey -- 'aPIKey.enable' route
    :<|> "apiKey" :> QueryParam "reverse" Bool :> Verb 'GET 200 '[JSON] [APIKey] -- 'aPIKey.get' route
    :<|> "apiKey" :> ReqBody '[FormUrlEncoded] FormAPIKeyNew :> Verb 'POST 200 '[JSON] APIKey -- 'aPIKey.new' route
    :<|> "apiKey" :> ReqBody '[FormUrlEncoded] FormAPIKeyRemove :> Verb 'DELETE 200 '[JSON] Inline_response_200 -- 'aPIKey.remove' route
    :<|> "announcement" :> QueryParam "columns" Text :> Verb 'GET 200 '[JSON] [Announcement] -- 'announcement.get' route
    :<|> "announcement" :> "urgent" :> Verb 'GET 200 '[JSON] [Announcement] -- 'announcement.getUrgent' route
    :<|> "chat" :> QueryParam "count" Double :> QueryParam "start" Double :> QueryParam "reverse" Bool :> QueryParam "channelID" Double :> Verb 'GET 200 '[JSON] [Chat] -- 'chat.get' route
    :<|> "chat" :> "channels" :> Verb 'GET 200 '[JSON] [ChatChannel] -- 'chat.getChannels' route
    :<|> "chat" :> "connected" :> Verb 'GET 200 '[JSON] ConnectedUsers -- 'chat.getConnected' route
    :<|> "chat" :> ReqBody '[FormUrlEncoded] FormChatNew :> Verb 'POST 200 '[JSON] Chat -- 'chat.new' route
    :<|> "execution" :> QueryParam "symbol" Text :> QueryParam "filter" Text :> QueryParam "columns" Text :> QueryParam "count" Double :> QueryParam "start" Double :> QueryParam "reverse" Bool :> QueryParam "startTime" Integer :> QueryParam "endTime" Integer :> Verb 'GET 200 '[JSON] [Execution] -- 'execution.get' route
    :<|> "execution" :> "tradeHistory" :> QueryParam "symbol" Text :> QueryParam "filter" Text :> QueryParam "columns" Text :> QueryParam "count" Double :> QueryParam "start" Double :> QueryParam "reverse" Bool :> QueryParam "startTime" Integer :> QueryParam "endTime" Integer :> Verb 'GET 200 '[JSON] [Execution] -- 'execution.getTradeHistory' route
    :<|> "funding" :> QueryParam "symbol" Text :> QueryParam "filter" Text :> QueryParam "columns" Text :> QueryParam "count" Double :> QueryParam "start" Double :> QueryParam "reverse" Bool :> QueryParam "startTime" Integer :> QueryParam "endTime" Integer :> Verb 'GET 200 '[JSON] [Funding] -- 'funding.get' route
    :<|> "globalNotification" :> Verb 'GET 200 '[JSON] [GlobalNotification] -- 'globalNotification.get' route
    :<|> "instrument" :> QueryParam "symbol" Text :> QueryParam "filter" Text :> QueryParam "columns" Text :> QueryParam "count" Double :> QueryParam "start" Double :> QueryParam "reverse" Bool :> QueryParam "startTime" Integer :> QueryParam "endTime" Integer :> Verb 'GET 200 '[JSON] [Instrument] -- 'instrument.get' route
    :<|> "instrument" :> "active" :> Verb 'GET 200 '[JSON] [Instrument] -- 'instrument.getActive' route
    :<|> "instrument" :> "activeAndIndices" :> Verb 'GET 200 '[JSON] [Instrument] -- 'instrument.getActiveAndIndices' route
    :<|> "instrument" :> "activeIntervals" :> Verb 'GET 200 '[JSON] InstrumentInterval -- 'instrument.getActiveIntervals' route
    :<|> "instrument" :> "compositeIndex" :> QueryParam "symbol" Text :> QueryParam "filter" Text :> QueryParam "columns" Text :> QueryParam "count" Double :> QueryParam "start" Double :> QueryParam "reverse" Bool :> QueryParam "startTime" Integer :> QueryParam "endTime" Integer :> Verb 'GET 200 '[JSON] [IndexComposite] -- 'instrument.getCompositeIndex' route
    :<|> "instrument" :> "indices" :> Verb 'GET 200 '[JSON] [Instrument] -- 'instrument.getIndices' route
    :<|> "insurance" :> QueryParam "symbol" Text :> QueryParam "filter" Text :> QueryParam "columns" Text :> QueryParam "count" Double :> QueryParam "start" Double :> QueryParam "reverse" Bool :> QueryParam "startTime" Integer :> QueryParam "endTime" Integer :> Verb 'GET 200 '[JSON] [Insurance] -- 'insurance.get' route
    :<|> "leaderboard" :> QueryParam "method" Text :> Verb 'GET 200 '[JSON] [Leaderboard] -- 'leaderboard.get' route
    :<|> "leaderboard" :> "name" :> Verb 'GET 200 '[JSON] Inline_response_200_1 -- 'leaderboard.getName' route
    :<|> "liquidation" :> QueryParam "symbol" Text :> QueryParam "filter" Text :> QueryParam "columns" Text :> QueryParam "count" Double :> QueryParam "start" Double :> QueryParam "reverse" Bool :> QueryParam "startTime" Integer :> QueryParam "endTime" Integer :> Verb 'GET 200 '[JSON] [Liquidation] -- 'liquidation.get' route
    :<|> "order" :> ReqBody '[FormUrlEncoded] FormOrderAmend :> Verb 'PUT 200 '[JSON] Order -- 'order.amend' route
    :<|> "order" :> "bulk" :> ReqBody '[FormUrlEncoded] FormOrderAmendBulk :> Verb 'PUT 200 '[JSON] [Order] -- 'order.amendBulk' route
    :<|> "order" :> ReqBody '[FormUrlEncoded] FormOrderCancel :> Verb 'DELETE 200 '[JSON] [Order] -- 'order.cancel' route
    :<|> "order" :> "all" :> ReqBody '[FormUrlEncoded] FormOrderCancelAll :> Verb 'DELETE 200 '[JSON] [Order] -- 'order.cancelAll' route
    :<|> "order" :> "cancelAllAfter" :> ReqBody '[FormUrlEncoded] FormOrderCancelAllAfter :> Verb 'POST 200 '[JSON] Value -- 'order.cancelAllAfter' route
    :<|> "order" :> "closePosition" :> ReqBody '[FormUrlEncoded] FormOrderClosePosition :> Verb 'POST 200 '[JSON] Order -- 'order.closePosition' route
    :<|> "order" :> QueryParam "symbol" Text :> QueryParam "filter" Text :> QueryParam "columns" Text :> QueryParam "count" Double :> QueryParam "start" Double :> QueryParam "reverse" Bool :> QueryParam "startTime" Integer :> QueryParam "endTime" Integer :> Verb 'GET 200 '[JSON] [Order] -- 'order.getOrders' route
    :<|> "order" :> ReqBody '[FormUrlEncoded] FormOrderNew :> Verb 'POST 200 '[JSON] Order -- 'order.new' route
    :<|> "order" :> "bulk" :> ReqBody '[FormUrlEncoded] FormOrderNewBulk :> Verb 'POST 200 '[JSON] [Order] -- 'order.newBulk' route
    :<|> "orderBook" :> "L2" :> QueryParam "symbol" Text :> QueryParam "depth" Double :> Verb 'GET 200 '[JSON] [OrderBookL2] -- 'orderBook.getL2' route
    :<|> "position" :> QueryParam "filter" Text :> QueryParam "columns" Text :> QueryParam "count" Double :> Verb 'GET 200 '[JSON] [Position] -- 'position.get' route
    :<|> "position" :> "isolate" :> ReqBody '[FormUrlEncoded] FormPositionIsolateMargin :> Verb 'POST 200 '[JSON] Position -- 'position.isolateMargin' route
    :<|> "position" :> "transferMargin" :> ReqBody '[FormUrlEncoded] FormPositionTransferIsolatedMargin :> Verb 'POST 200 '[JSON] Position -- 'position.transferIsolatedMargin' route
    :<|> "position" :> "leverage" :> ReqBody '[FormUrlEncoded] FormPositionUpdateLeverage :> Verb 'POST 200 '[JSON] Position -- 'position.updateLeverage' route
    :<|> "position" :> "riskLimit" :> ReqBody '[FormUrlEncoded] FormPositionUpdateRiskLimit :> Verb 'POST 200 '[JSON] Position -- 'position.updateRiskLimit' route
    :<|> "quote" :> QueryParam "symbol" Text :> QueryParam "filter" Text :> QueryParam "columns" Text :> QueryParam "count" Double :> QueryParam "start" Double :> QueryParam "reverse" Bool :> QueryParam "startTime" Integer :> QueryParam "endTime" Integer :> Verb 'GET 200 '[JSON] [Quote] -- 'quote.get' route
    :<|> "quote" :> "bucketed" :> QueryParam "binSize" Text :> QueryParam "partial" Bool :> QueryParam "symbol" Text :> QueryParam "filter" Text :> QueryParam "columns" Text :> QueryParam "count" Double :> QueryParam "start" Double :> QueryParam "reverse" Bool :> QueryParam "startTime" Integer :> QueryParam "endTime" Integer :> Verb 'GET 200 '[JSON] [Quote] -- 'quote.getBucketed' route
    :<|> "schema" :> QueryParam "model" Text :> Verb 'GET 200 '[JSON] Value -- 'schema.get' route
    :<|> "schema" :> "websocketHelp" :> Verb 'GET 200 '[JSON] Value -- 'schema.websocketHelp' route
    :<|> "settlement" :> QueryParam "symbol" Text :> QueryParam "filter" Text :> QueryParam "columns" Text :> QueryParam "count" Double :> QueryParam "start" Double :> QueryParam "reverse" Bool :> QueryParam "startTime" Integer :> QueryParam "endTime" Integer :> Verb 'GET 200 '[JSON] [Settlement] -- 'settlement.get' route
    :<|> "stats" :> Verb 'GET 200 '[JSON] [Stats] -- 'stats.get' route
    :<|> "stats" :> "history" :> Verb 'GET 200 '[JSON] [StatsHistory] -- 'stats.history' route
    :<|> "stats" :> "historyUSD" :> Verb 'GET 200 '[JSON] [StatsUSD] -- 'stats.historyUSD' route
    :<|> "trade" :> QueryParam "symbol" Text :> QueryParam "filter" Text :> QueryParam "columns" Text :> QueryParam "count" Double :> QueryParam "start" Double :> QueryParam "reverse" Bool :> QueryParam "startTime" Integer :> QueryParam "endTime" Integer :> Verb 'GET 200 '[JSON] [Trade] -- 'trade.get' route
    :<|> "trade" :> "bucketed" :> QueryParam "binSize" Text :> QueryParam "partial" Bool :> QueryParam "symbol" Text :> QueryParam "filter" Text :> QueryParam "columns" Text :> QueryParam "count" Double :> QueryParam "start" Double :> QueryParam "reverse" Bool :> QueryParam "startTime" Integer :> QueryParam "endTime" Integer :> Verb 'GET 200 '[JSON] [TradeBin] -- 'trade.getBucketed' route
    :<|> "user" :> "cancelWithdrawal" :> ReqBody '[FormUrlEncoded] FormUserCancelWithdrawal :> Verb 'POST 200 '[JSON] Transaction -- 'user.cancelWithdrawal' route
    :<|> "user" :> "checkReferralCode" :> QueryParam "referralCode" Text :> Verb 'GET 200 '[JSON] Double -- 'user.checkReferralCode' route
    :<|> "user" :> "communicationToken" :> ReqBody '[FormUrlEncoded] FormUserCommunicationToken :> Verb 'POST 200 '[JSON] [CommunicationToken] -- 'user.communicationToken' route
    :<|> "user" :> "confirmEmail" :> ReqBody '[FormUrlEncoded] FormUserConfirm :> Verb 'POST 200 '[JSON] AccessToken -- 'user.confirm' route
    :<|> "user" :> "confirmWithdrawal" :> ReqBody '[FormUrlEncoded] FormUserConfirmWithdrawal :> Verb 'POST 200 '[JSON] Transaction -- 'user.confirmWithdrawal' route
    :<|> "user" :> Verb 'GET 200 '[JSON] User -- 'user.get' route
    :<|> "user" :> "affiliateStatus" :> Verb 'GET 200 '[JSON] Affiliate -- 'user.getAffiliateStatus' route
    :<|> "user" :> "commission" :> Verb 'GET 200 '[JSON] UserCommissionsBySymbol -- 'user.getCommission' route
    :<|> "user" :> "depositAddress" :> QueryParam "currency" Text :> Verb 'GET 200 '[JSON] Text -- 'user.getDepositAddress' route
    :<|> "user" :> "executionHistory" :> QueryParam "symbol" Text :> QueryParam "timestamp" Integer :> Verb 'GET 200 '[JSON] Value -- 'user.getExecutionHistory' route
    :<|> "user" :> "margin" :> QueryParam "currency" Text :> Verb 'GET 200 '[JSON] Margin -- 'user.getMargin' route
    :<|> "user" :> "wallet" :> QueryParam "currency" Text :> Verb 'GET 200 '[JSON] Wallet -- 'user.getWallet' route
    :<|> "user" :> "walletHistory" :> QueryParam "currency" Text :> QueryParam "count" Double :> QueryParam "start" Double :> Verb 'GET 200 '[JSON] [Transaction] -- 'user.getWalletHistory' route
    :<|> "user" :> "walletSummary" :> QueryParam "currency" Text :> Verb 'GET 200 '[JSON] [Transaction] -- 'user.getWalletSummary' route
    :<|> "user" :> "logout" :> Verb 'POST 200 '[JSON] () -- 'user.logout' route
    :<|> "user" :> "minWithdrawalFee" :> QueryParam "currency" Text :> Verb 'GET 200 '[JSON] Value -- 'user.minWithdrawalFee' route
    :<|> "user" :> "requestWithdrawal" :> ReqBody '[FormUrlEncoded] FormUserRequestWithdrawal :> Verb 'POST 200 '[JSON] Transaction -- 'user.requestWithdrawal' route
    :<|> "user" :> "preferences" :> ReqBody '[FormUrlEncoded] FormUserSavePreferences :> Verb 'POST 200 '[JSON] User -- 'user.savePreferences' route
    :<|> "userEvent" :> QueryParam "count" Double :> QueryParam "startId" Double :> Verb 'GET 200 '[JSON] [UserEvent] -- 'userEvent.get' route

-- | Server or client configuration, specifying the host and port to query or serve on.
data ServerConfig = ServerConfig
  { configHost :: String  -- ^ Hostname to serve on, e.g. "127.0.0.1"
  , configPort :: Int      -- ^ Port to serve on, e.g. 8080
  } deriving (Eq, Ord, Show, Read)

-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
  parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
  parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
  parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
  parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
  parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList


-- | Backend for BitMEX.
-- The backend can be used both for the client and the server. The client generated from the BitMEX Swagger spec
-- is a backend that executes actions by sending HTTP requests (see @createBitMEXClient@). Alternatively, provided
-- a backend, the API can be served using @runBitMEXServer@.
data BitMEXBackend m = BitMEXBackend
  { aPIKeyDisable :: FormAPIKeyDisable -> m APIKey{- ^  -}
  , aPIKeyEnable :: FormAPIKeyEnable -> m APIKey{- ^  -}
  , aPIKeyGet :: Maybe Bool -> m [APIKey]{- ^  -}
  , aPIKeyNew :: FormAPIKeyNew -> m APIKey{- ^ API Keys can only be created via the frontend. -}
  , aPIKeyRemove :: FormAPIKeyRemove -> m Inline_response_200{- ^  -}
  , announcementGet :: Maybe Text -> m [Announcement]{- ^  -}
  , announcementGetUrgent :: m [Announcement]{- ^  -}
  , chatGet :: Maybe Double -> Maybe Double -> Maybe Bool -> Maybe Double -> m [Chat]{- ^  -}
  , chatGetChannels :: m [ChatChannel]{- ^  -}
  , chatGetConnected :: m ConnectedUsers{- ^ Returns an array with browser users in the first position and API users (bots) in the second position. -}
  , chatNew :: FormChatNew -> m Chat{- ^  -}
  , executionGet :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Double -> Maybe Double -> Maybe Bool -> Maybe Integer -> Maybe Integer -> m [Execution]{- ^ This returns all raw transactions, which includes order opening and cancelation, and order status changes. It can be quite noisy. More focused information is available at `/execution/tradeHistory`.  You may also use the `filter` param to target your query. Specify an array as a filter value, such as `{\"execType\": [\"Settlement\", \"Trade\"]}` to filter on multiple values.  See [the FIX Spec](http://www.onixs.biz/fix-dictionary/5.0.SP2/msgType_8_8.html) for explanations of these fields.  -}
  , executionGetTradeHistory :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Double -> Maybe Double -> Maybe Bool -> Maybe Integer -> Maybe Integer -> m [Execution]{- ^  -}
  , fundingGet :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Double -> Maybe Double -> Maybe Bool -> Maybe Integer -> Maybe Integer -> m [Funding]{- ^  -}
  , globalNotificationGet :: m [GlobalNotification]{- ^ This is an upcoming feature and currently does not return data. -}
  , instrumentGet :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Double -> Maybe Double -> Maybe Bool -> Maybe Integer -> Maybe Integer -> m [Instrument]{- ^ This returns all instruments and indices, including those that have settled or are unlisted. Use this endpoint if you want to query for individual instruments or use a complex filter. Use `/instrument/active` to return active instruments, or use a filter like `{\"state\": \"Open\"}`. -}
  , instrumentGetActive :: m [Instrument]{- ^  -}
  , instrumentGetActiveAndIndices :: m [Instrument]{- ^  -}
  , instrumentGetActiveIntervals :: m InstrumentInterval{- ^ This endpoint is useful for determining which pairs are live. It returns two arrays of   strings. The first is intervals, such as `[\"XBT:perpetual\", \"XBT:monthly\", \"XBT:quarterly\", \"ETH:monthly\", ...]`. These identifiers are usable in any query's `symbol` param. The second array is the current resolution of these intervals. Results are mapped at the same index. -}
  , instrumentGetCompositeIndex :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Double -> Maybe Double -> Maybe Bool -> Maybe Integer -> Maybe Integer -> m [IndexComposite]{- ^ Composite indices are built from multiple external price sources.  Use this endpoint to get the underlying prices of an index. For example, send a `symbol` of `.XBT` to get the ticks and weights of the constituent exchanges that build the \".XBT\" index.  A tick with reference `\"BMI\"` and weight `null` is the composite index tick.  -}
  , instrumentGetIndices :: m [Instrument]{- ^  -}
  , insuranceGet :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Double -> Maybe Double -> Maybe Bool -> Maybe Integer -> Maybe Integer -> m [Insurance]{- ^  -}
  , leaderboardGet :: Maybe Text -> m [Leaderboard]{- ^  -}
  , leaderboardGetName :: m Inline_response_200_1{- ^  -}
  , liquidationGet :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Double -> Maybe Double -> Maybe Bool -> Maybe Integer -> Maybe Integer -> m [Liquidation]{- ^  -}
  , orderAmend :: FormOrderAmend -> m Order{- ^ Send an `orderID` or `origClOrdID` to identify the order you wish to amend.  Both order quantity and price can be amended. Only one `qty` field can be used to amend.  Use the `leavesQty` field to specify how much of the order you wish to remain open. This can be useful if you want to adjust your position's delta by a certain amount, regardless of how much of the order has already filled.  > A `leavesQty` can be used to make a \"Filled\" order live again, if it is received within 60 seconds of the fill.  Like order placement, amending can be done in bulk. Simply send a request to `PUT /api/v1/order/bulk` with a JSON body of the shape: `{\"orders\": [{...}, {...}]}`, each object containing the fields used in this endpoint.  -}
  , orderAmendBulk :: FormOrderAmendBulk -> m [Order]{- ^ Similar to POST /amend, but with multiple orders. `application/json` only. Ratelimited at 10%. -}
  , orderCancel :: FormOrderCancel -> m [Order]{- ^ Either an orderID or a clOrdID must be provided. -}
  , orderCancelAll :: FormOrderCancelAll -> m [Order]{- ^  -}
  , orderCancelAllAfter :: FormOrderCancelAllAfter -> m Value{- ^ Useful as a dead-man's switch to ensure your orders are canceled in case of an outage. If called repeatedly, the existing offset will be canceled and a new one will be inserted in its place.  Example usage: call this route at 15s intervals with an offset of 60000 (60s). If this route is not called within 60 seconds, all your orders will be automatically canceled.  This is also available via [WebSocket](https://www.bitmex.com/app/wsAPI#Dead-Mans-Switch-Auto-Cancel).  -}
  , orderClosePosition :: FormOrderClosePosition -> m Order{- ^ If no `price` is specified, a market order will be submitted to close the whole of your position. This will also close all other open orders in this symbol. -}
  , orderGetOrders :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Double -> Maybe Double -> Maybe Bool -> Maybe Integer -> Maybe Integer -> m [Order]{- ^ To get open orders only, send {\"open\": true} in the filter param.  See <a href=\"http://www.onixs.biz/fix-dictionary/5.0.SP2/msgType_D_68.html\">the FIX Spec</a> for explanations of these fields. -}
  , orderNew :: FormOrderNew -> m Order{- ^ ## Placing Orders  This endpoint is used for placing orders. See individual fields below for more details on their use.  #### Order Types  All orders require a `symbol`. All other fields are optional except when otherwise specified.  These are the valid `ordType`s:  * **Limit**: The default order type. Specify an `orderQty` and `price`. * **Market**: A traditional Market order. A Market order will execute until filled or your bankruptcy price is reached, at   which point it will cancel. * **Stop**: A Stop Market order. Specify an `orderQty` and `stopPx`. When the `stopPx` is reached, the order will be entered   into the book.   * On sell orders, the order will trigger if the triggering price is lower than the `stopPx`. On buys, higher.   * Note: Stop orders do not consume margin until triggered. Be sure that the required margin is available in your     account so that it may trigger fully.   * `Close` Stops don't require an `orderQty`. See Execution Instructions below. * **StopLimit**: Like a Stop Market, but enters a Limit order instead of a Market order. Specify an `orderQty`, `stopPx`,   and `price`. * **MarketIfTouched**: Similar to a Stop, but triggers are done in the opposite direction. Useful for Take Profit orders. * **LimitIfTouched**: As above; use for Take Profit Limit orders.  #### Execution Instructions  The following `execInst`s are supported. If using multiple, separate with a comma (e.g. `LastPrice,Close`).  * **ParticipateDoNotInitiate**: Also known as a Post-Only order. If this order would have executed on placement,   it will cancel instead. * **MarkPrice, LastPrice, IndexPrice**: Used by stop and if-touched orders to determine the triggering price.   Use only one. By default, `'MarkPrice'` is used. Also used for Pegged orders to define the value of `'LastPeg'`. * **ReduceOnly**: A `'ReduceOnly'` order can only reduce your position, not increase it. If you have a `'ReduceOnly'`   limit order that rests in the order book while the position is reduced by other orders, then its order quantity will   be amended down or canceled. If there are multiple `'ReduceOnly'` orders the least aggressive will be amended first. * **Close**: `'Close'` implies `'ReduceOnly'`. A `'Close'` order will cancel other active limit orders with the same side   and symbol if the open quantity exceeds the current position. This is useful for stops: by canceling these orders, a   `'Close'` Stop is ensured to have the margin required to execute, and can only execute up to the full size of your   position. If `orderQty` is not specified, a `'Close'` order has an `orderQty` equal to your current position's size.   * Note that a `Close` order without an `orderQty` requires a `side`, so that BitMEX knows if it should trigger   above or below the `stopPx`.  #### Linked Orders  [Linked Orders are deprecated as of 2018/11/10](https://blog.bitmex.com/api_announcement/deprecation-of-contingent-orders/)  #### Trailing Stops  You may use `pegPriceType` of `'TrailingStopPeg'` to create Trailing Stops. The pegged `stopPx` will move as the market moves away from the peg, and freeze as the market moves toward it.  To use, combine with `pegOffsetValue` to set the `stopPx` of your order. The peg is set to the triggering price specified in the `execInst` (default `'MarkPrice'`). Use a negative offset for stop-sell and buy-if-touched orders.  Requires `ordType`: `'Stop', 'StopLimit', 'MarketIfTouched', 'LimitIfTouched'`.  #### Simple Quantities  [Simple Quantities are deprecated as of 2018/10/26](https://blog.bitmex.com/api_announcement/deprecation-of-simpleorderqty-functionality/)  #### Rate Limits  See the [Bulk Order Documentation](#!/Order/Order_newBulk) if you need to place multiple orders at the same time. Bulk orders require fewer risk checks in the trading engine and thus are ratelimited at **1/10** the normal rate.  You can also improve your reactivity to market movements while staying under your ratelimit by using the [Amend](#!/Order/Order_amend) and [Amend Bulk](#!/Order/Order_amendBulk) endpoints. This allows you to stay in the market and avoids the cancel/replace cycle.  #### Tracking Your Orders  If you want to keep track of order IDs yourself, set a unique `clOrdID` per order. This `clOrdID` will come back as a property on the order and any related executions (including on the WebSocket), and can be used to get or cancel the order. Max length is 36 characters.  You can also change the `clOrdID` by amending an order, supplying an `origClOrdID`, and your desired new ID as the `clOrdID` param, like so:  ``` # Amends an order's leavesQty, and updates its clOrdID to \"def-456\" PUT /api/v1/order {\"origClOrdID\": \"abc-123\", \"clOrdID\": \"def-456\", \"leavesQty\": 1000} ```  -}
  , orderNewBulk :: FormOrderNewBulk -> m [Order]{- ^ This endpoint is used for placing bulk orders. Valid order types are Market, Limit, Stop, StopLimit, MarketIfTouched, LimitIfTouched, and Pegged.  Each individual order object in the array should have the same properties as an individual POST /order call.  This endpoint is much faster for getting many orders into the book at once. Because it reduces load on BitMEX systems, this endpoint is ratelimited at `ceil(0.1 * orders)`. Submitting 10 orders via a bulk order call will only count as 1 request, 15 as 2, 32 as 4, and so on.  For now, only `application/json` is supported on this endpoint.  -}
  , orderBookGetL2 :: Maybe Text -> Maybe Double -> m [OrderBookL2]{- ^  -}
  , positionGet :: Maybe Text -> Maybe Text -> Maybe Double -> m [Position]{- ^ This endpoint is used for retrieving position information. The fields largely follow the [FIX spec](http://www.onixs.biz/fix-dictionary/5.0.SP2/msgType_AP_6580.html) definitions. Some selected fields are explained in more detail below.  The fields _account_, _symbol_, _currency_ are unique to each position and form its key.  * **account**: Your unique account ID. * **symbol**: The contract for this position. * **currency**: The margin currency for this position. * **underlying**: Meta data of the _symbol_. * **quoteCurrency**: Meta data of the _symbol_,  All prices are in the _quoteCurrency_ * **commission**: The maximum of the maker, taker, and settlement fee. * **initMarginReq**: The initial margin requirement.  This will be at least the symbol's default initial maintenance margin, but can be higher if you choose lower leverage. * **maintMarginReq**: The maintenance margin requirement.  This will be at least the symbol's default maintenance maintenance margin, but can be higher if you choose a higher risk limit. * **riskLimit**: This is a function of your _maintMarginReq_. * **leverage**: 1 / initMarginReq. * **crossMargin**: True/false depending on whether you set cross margin on this position. * **deleveragePercentile**: Indicates where your position is in the ADL queue. * **rebalancedPnl**: The value of realised PNL that has transferred to your wallet for this position. * **prevRealisedPnl**: The value of realised PNL that has transferred to your wallet for this position since the position was closed. * **currentQty**: The current position amount in contracts. * **currentCost**: The current cost of the position in the settlement currency of the symbol (_currency_). * **currentComm**: The current commission of the position in the settlement currency of the symbol (_currency_). * **realisedCost**: The realised cost of this position calculated with regard to average cost accounting. * **unrealisedCost**: _currentCost_ - _realisedCost_. * **grossOpenCost**: The absolute value of your open orders for this symbol. * **grossOpenPremium**: The amount your bidding above the mark price in the settlement currency of the symbol (_currency_). * **markPrice**: The mark price of the symbol in _quoteCurrency_. * **markValue**: The _currentQty_ at the mark price in the settlement currency of the symbol (_currency_). * **homeNotional**: Value of position in units of _underlying_. * **foreignNotional**: Value of position in units of _quoteCurrency_. * **realisedPnl**: The negative of _realisedCost_. * **unrealisedGrossPnl**: _markValue_ - _unrealisedCost_. * **unrealisedPnl**: _unrealisedGrossPnl_. * **liquidationPrice**: Once markPrice reaches this price, this position will be liquidated. * **bankruptPrice**: Once markPrice reaches this price, this position will have no equity.  -}
  , positionIsolateMargin :: FormPositionIsolateMargin -> m Position{- ^  -}
  , positionTransferIsolatedMargin :: FormPositionTransferIsolatedMargin -> m Position{- ^  -}
  , positionUpdateLeverage :: FormPositionUpdateLeverage -> m Position{- ^  -}
  , positionUpdateRiskLimit :: FormPositionUpdateRiskLimit -> m Position{- ^  -}
  , quoteGet :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Double -> Maybe Double -> Maybe Bool -> Maybe Integer -> Maybe Integer -> m [Quote]{- ^  -}
  , quoteGetBucketed :: Maybe Text -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Double -> Maybe Double -> Maybe Bool -> Maybe Integer -> Maybe Integer -> m [Quote]{- ^ Timestamps returned by our bucketed endpoints are the **end** of the period, indicating when the bucket was written to disk. Some other common systems use the timestamp as the beginning of the period. Please be aware of this when using this endpoint. -}
  , schemaGet :: Maybe Text -> m Value{- ^  -}
  , schemaWebsocketHelp :: m Value{- ^  -}
  , settlementGet :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Double -> Maybe Double -> Maybe Bool -> Maybe Integer -> Maybe Integer -> m [Settlement]{- ^  -}
  , statsGet :: m [Stats]{- ^  -}
  , statsHistory :: m [StatsHistory]{- ^  -}
  , statsHistoryUSD :: m [StatsUSD]{- ^  -}
  , tradeGet :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Double -> Maybe Double -> Maybe Bool -> Maybe Integer -> Maybe Integer -> m [Trade]{- ^ Please note that indices (symbols starting with `.`) post trades at intervals to the trade feed. These have a `size` of 0 and are used only to indicate a changing price.  See [the FIX Spec](http://www.onixs.biz/fix-dictionary/5.0.SP2/msgType_AE_6569.html) for explanations of these fields. -}
  , tradeGetBucketed :: Maybe Text -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Double -> Maybe Double -> Maybe Bool -> Maybe Integer -> Maybe Integer -> m [TradeBin]{- ^ Timestamps returned by our bucketed endpoints are the **end** of the period, indicating when the bucket was written to disk. Some other common systems use the timestamp as the beginning of the period. Please be aware of this when using this endpoint.  Also note the `open` price is equal to the `close` price of the previous timeframe bucket. -}
  , userCancelWithdrawal :: FormUserCancelWithdrawal -> m Transaction{- ^  -}
  , userCheckReferralCode :: Maybe Text -> m Double{- ^ If the code is valid, responds with the referral code's discount (e.g. `0.1` for 10%). Otherwise, will return a 404 or 451 if invalid. -}
  , userCommunicationToken :: FormUserCommunicationToken -> m [CommunicationToken]{- ^  -}
  , userConfirm :: FormUserConfirm -> m AccessToken{- ^  -}
  , userConfirmWithdrawal :: FormUserConfirmWithdrawal -> m Transaction{- ^  -}
  , userGet :: m User{- ^  -}
  , userGetAffiliateStatus :: m Affiliate{- ^  -}
  , userGetCommission :: m UserCommissionsBySymbol{- ^  -}
  , userGetDepositAddress :: Maybe Text -> m Text{- ^  -}
  , userGetExecutionHistory :: Maybe Text -> Maybe Integer -> m Value{- ^  -}
  , userGetMargin :: Maybe Text -> m Margin{- ^  -}
  , userGetWallet :: Maybe Text -> m Wallet{- ^  -}
  , userGetWalletHistory :: Maybe Text -> Maybe Double -> Maybe Double -> m [Transaction]{- ^  -}
  , userGetWalletSummary :: Maybe Text -> m [Transaction]{- ^  -}
  , userLogout :: m (){- ^  -}
  , userMinWithdrawalFee :: Maybe Text -> m Value{- ^ This is changed based on network conditions to ensure timely withdrawals. During network congestion, this may be high. The fee is returned in the same currency. -}
  , userRequestWithdrawal :: FormUserRequestWithdrawal -> m Transaction{- ^ This will send a confirmation email to the email address on record. -}
  , userSavePreferences :: FormUserSavePreferences -> m User{- ^  -}
  , userEventGet :: Maybe Double -> Maybe Double -> m [UserEvent]{- ^  -}
  }

newtype BitMEXClient a = BitMEXClient
  { runClient :: Manager -> BaseUrl -> ExceptT ServantError IO a
  } deriving Functor

instance Applicative BitMEXClient where
  pure x = BitMEXClient (\_ _ -> pure x)
  (BitMEXClient f) <*> (BitMEXClient x) =
    BitMEXClient (\manager url -> f manager url <*> x manager url)

instance Monad BitMEXClient where
  (BitMEXClient a) >>= f =
    BitMEXClient (\manager url -> do
      value <- a manager url
      runClient (f value) manager url)

instance MonadIO BitMEXClient where
  liftIO io = BitMEXClient (\_ _ -> liftIO io)

createBitMEXClient :: BitMEXBackend BitMEXClient
createBitMEXClient = BitMEXBackend{..}
  where
    ((coerce -> aPIKeyDisable) :<|>
     (coerce -> aPIKeyEnable) :<|>
     (coerce -> aPIKeyGet) :<|>
     (coerce -> aPIKeyNew) :<|>
     (coerce -> aPIKeyRemove) :<|>
     (coerce -> announcementGet) :<|>
     (coerce -> announcementGetUrgent) :<|>
     (coerce -> chatGet) :<|>
     (coerce -> chatGetChannels) :<|>
     (coerce -> chatGetConnected) :<|>
     (coerce -> chatNew) :<|>
     (coerce -> executionGet) :<|>
     (coerce -> executionGetTradeHistory) :<|>
     (coerce -> fundingGet) :<|>
     (coerce -> globalNotificationGet) :<|>
     (coerce -> instrumentGet) :<|>
     (coerce -> instrumentGetActive) :<|>
     (coerce -> instrumentGetActiveAndIndices) :<|>
     (coerce -> instrumentGetActiveIntervals) :<|>
     (coerce -> instrumentGetCompositeIndex) :<|>
     (coerce -> instrumentGetIndices) :<|>
     (coerce -> insuranceGet) :<|>
     (coerce -> leaderboardGet) :<|>
     (coerce -> leaderboardGetName) :<|>
     (coerce -> liquidationGet) :<|>
     (coerce -> orderAmend) :<|>
     (coerce -> orderAmendBulk) :<|>
     (coerce -> orderCancel) :<|>
     (coerce -> orderCancelAll) :<|>
     (coerce -> orderCancelAllAfter) :<|>
     (coerce -> orderClosePosition) :<|>
     (coerce -> orderGetOrders) :<|>
     (coerce -> orderNew) :<|>
     (coerce -> orderNewBulk) :<|>
     (coerce -> orderBookGetL2) :<|>
     (coerce -> positionGet) :<|>
     (coerce -> positionIsolateMargin) :<|>
     (coerce -> positionTransferIsolatedMargin) :<|>
     (coerce -> positionUpdateLeverage) :<|>
     (coerce -> positionUpdateRiskLimit) :<|>
     (coerce -> quoteGet) :<|>
     (coerce -> quoteGetBucketed) :<|>
     (coerce -> schemaGet) :<|>
     (coerce -> schemaWebsocketHelp) :<|>
     (coerce -> settlementGet) :<|>
     (coerce -> statsGet) :<|>
     (coerce -> statsHistory) :<|>
     (coerce -> statsHistoryUSD) :<|>
     (coerce -> tradeGet) :<|>
     (coerce -> tradeGetBucketed) :<|>
     (coerce -> userCancelWithdrawal) :<|>
     (coerce -> userCheckReferralCode) :<|>
     (coerce -> userCommunicationToken) :<|>
     (coerce -> userConfirm) :<|>
     (coerce -> userConfirmWithdrawal) :<|>
     (coerce -> userGet) :<|>
     (coerce -> userGetAffiliateStatus) :<|>
     (coerce -> userGetCommission) :<|>
     (coerce -> userGetDepositAddress) :<|>
     (coerce -> userGetExecutionHistory) :<|>
     (coerce -> userGetMargin) :<|>
     (coerce -> userGetWallet) :<|>
     (coerce -> userGetWalletHistory) :<|>
     (coerce -> userGetWalletSummary) :<|>
     (coerce -> userLogout) :<|>
     (coerce -> userMinWithdrawalFee) :<|>
     (coerce -> userRequestWithdrawal) :<|>
     (coerce -> userSavePreferences) :<|>
     (coerce -> userEventGet)) = client (Proxy :: Proxy BitMEXAPI)

-- | Run requests in the BitMEXClient monad.
runBitMEXClient :: ServerConfig -> BitMEXClient a -> ExceptT ServantError IO a
runBitMEXClient clientConfig cl = do
  manager <- liftIO $ newManager defaultManagerSettings
  runBitMEXClientWithManager manager clientConfig cl

-- | Run requests in the BitMEXClient monad using a custom manager.
runBitMEXClientWithManager :: Manager -> ServerConfig -> BitMEXClient a -> ExceptT ServantError IO a
runBitMEXClientWithManager manager clientConfig cl =
  runClient cl manager $ BaseUrl Http (configHost clientConfig) (configPort clientConfig) ""

-- | Run the BitMEX server at the provided host and port.
runBitMEXServer :: MonadIO m => ServerConfig -> BitMEXBackend (ExceptT ServantErr IO)  -> m ()
runBitMEXServer ServerConfig{..} backend =
  liftIO $ Warp.runSettings warpSettings $ serve (Proxy :: Proxy BitMEXAPI) (serverFromBackend backend)
  where
    warpSettings = Warp.defaultSettings & Warp.setPort configPort & Warp.setHost (fromString configHost)
    serverFromBackend BitMEXBackend{..} =
      (coerce aPIKeyDisable :<|>
       coerce aPIKeyEnable :<|>
       coerce aPIKeyGet :<|>
       coerce aPIKeyNew :<|>
       coerce aPIKeyRemove :<|>
       coerce announcementGet :<|>
       coerce announcementGetUrgent :<|>
       coerce chatGet :<|>
       coerce chatGetChannels :<|>
       coerce chatGetConnected :<|>
       coerce chatNew :<|>
       coerce executionGet :<|>
       coerce executionGetTradeHistory :<|>
       coerce fundingGet :<|>
       coerce globalNotificationGet :<|>
       coerce instrumentGet :<|>
       coerce instrumentGetActive :<|>
       coerce instrumentGetActiveAndIndices :<|>
       coerce instrumentGetActiveIntervals :<|>
       coerce instrumentGetCompositeIndex :<|>
       coerce instrumentGetIndices :<|>
       coerce insuranceGet :<|>
       coerce leaderboardGet :<|>
       coerce leaderboardGetName :<|>
       coerce liquidationGet :<|>
       coerce orderAmend :<|>
       coerce orderAmendBulk :<|>
       coerce orderCancel :<|>
       coerce orderCancelAll :<|>
       coerce orderCancelAllAfter :<|>
       coerce orderClosePosition :<|>
       coerce orderGetOrders :<|>
       coerce orderNew :<|>
       coerce orderNewBulk :<|>
       coerce orderBookGetL2 :<|>
       coerce positionGet :<|>
       coerce positionIsolateMargin :<|>
       coerce positionTransferIsolatedMargin :<|>
       coerce positionUpdateLeverage :<|>
       coerce positionUpdateRiskLimit :<|>
       coerce quoteGet :<|>
       coerce quoteGetBucketed :<|>
       coerce schemaGet :<|>
       coerce schemaWebsocketHelp :<|>
       coerce settlementGet :<|>
       coerce statsGet :<|>
       coerce statsHistory :<|>
       coerce statsHistoryUSD :<|>
       coerce tradeGet :<|>
       coerce tradeGetBucketed :<|>
       coerce userCancelWithdrawal :<|>
       coerce userCheckReferralCode :<|>
       coerce userCommunicationToken :<|>
       coerce userConfirm :<|>
       coerce userConfirmWithdrawal :<|>
       coerce userGet :<|>
       coerce userGetAffiliateStatus :<|>
       coerce userGetCommission :<|>
       coerce userGetDepositAddress :<|>
       coerce userGetExecutionHistory :<|>
       coerce userGetMargin :<|>
       coerce userGetWallet :<|>
       coerce userGetWalletHistory :<|>
       coerce userGetWalletSummary :<|>
       coerce userLogout :<|>
       coerce userMinWithdrawalFee :<|>
       coerce userRequestWithdrawal :<|>
       coerce userSavePreferences :<|>
       coerce userEventGet)
