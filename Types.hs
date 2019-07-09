{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module BitMEX.Types (
  APIKey (..),
  AccessToken (..),
  Affiliate (..),
  Announcement (..),
  Chat (..),
  ChatChannel (..),
  CommunicationToken (..),
  ConnectedUsers (..),
  Error (..),
  Error_error (..),
  Execution (..),
  Funding (..),
  GlobalNotification (..),
  IndexComposite (..),
  Inline_response_200 (..),
  Inline_response_200_1 (..),
  Instrument (..),
  InstrumentInterval (..),
  Insurance (..),
  Leaderboard (..),
  Liquidation (..),
  Margin (..),
  Order (..),
  OrderBookL2 (..),
  Position (..),
  Quote (..),
  Settlement (..),
  Stats (..),
  StatsHistory (..),
  StatsUSD (..),
  Trade (..),
  TradeBin (..),
  Transaction (..),
  User (..),
  UserCommission (..),
  UserCommissionsBySymbol (..),
  UserEvent (..),
  UserPreferences (..),
  Wallet (..),
  Xany (..),
  ) where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Function ((&))


-- | Persistent API Keys for Developers
data APIKey = APIKey
  { aPIKeyId :: Text -- ^ 
  , aPIKeySecret :: Text -- ^ 
  , aPIKeyName :: Text -- ^ 
  , aPIKeyNonce :: Double -- ^ 
  , aPIKeyCidr :: Text -- ^ 
  , aPIKeyPermissions :: [Xany] -- ^ 
  , aPIKeyEnabled :: Bool -- ^ 
  , aPIKeyUserId :: Double -- ^ 
  , aPIKeyCreated :: Integer -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON APIKey where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "aPIKey")
instance ToJSON APIKey where
  toJSON = genericToJSON (removeFieldLabelPrefix False "aPIKey")

-- | 
data AccessToken = AccessToken
  { accessTokenId :: Text -- ^ 
  , accessTokenTtl :: Double -- ^ time to live in seconds (2 weeks by default)
  , accessTokenCreated :: Integer -- ^ 
  , accessTokenUserId :: Double -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON AccessToken where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "accessToken")
instance ToJSON AccessToken where
  toJSON = genericToJSON (removeFieldLabelPrefix False "accessToken")

-- | 
data Affiliate = Affiliate
  { affiliateAccount :: Double -- ^ 
  , affiliateCurrency :: Text -- ^ 
  , affiliatePrevPayout :: Double -- ^ 
  , affiliatePrevTurnover :: Double -- ^ 
  , affiliatePrevComm :: Double -- ^ 
  , affiliatePrevTimestamp :: Integer -- ^ 
  , affiliateExecTurnover :: Double -- ^ 
  , affiliateExecComm :: Double -- ^ 
  , affiliateTotalReferrals :: Double -- ^ 
  , affiliateTotalTurnover :: Double -- ^ 
  , affiliateTotalComm :: Double -- ^ 
  , affiliatePayoutPcnt :: Double -- ^ 
  , affiliatePendingPayout :: Double -- ^ 
  , affiliateTimestamp :: Integer -- ^ 
  , affiliateReferrerAccount :: Double -- ^ 
  , affiliateReferralDiscount :: Double -- ^ 
  , affiliateAffiliatePayout :: Double -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Affiliate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "affiliate")
instance ToJSON Affiliate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "affiliate")

-- | Public Announcements
data Announcement = Announcement
  { announcementId :: Double -- ^ 
  , announcementLink :: Text -- ^ 
  , announcementTitle :: Text -- ^ 
  , announcementContent :: Text -- ^ 
  , announcementDate :: Integer -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Announcement where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "announcement")
instance ToJSON Announcement where
  toJSON = genericToJSON (removeFieldLabelPrefix False "announcement")

-- | Trollbox Data
data Chat = Chat
  { chatId :: Double -- ^ 
  , chatDate :: Integer -- ^ 
  , chatUser :: Text -- ^ 
  , chatMessage :: Text -- ^ 
  , chatHtml :: Text -- ^ 
  , chatFromBot :: Bool -- ^ 
  , chatChannelID :: Double -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Chat where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "chat")
instance ToJSON Chat where
  toJSON = genericToJSON (removeFieldLabelPrefix False "chat")

-- | 
data ChatChannel = ChatChannel
  { chatChannelId :: Double -- ^ 
  , chatChannelName :: Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON ChatChannel where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "chatChannel")
instance ToJSON ChatChannel where
  toJSON = genericToJSON (removeFieldLabelPrefix False "chatChannel")

-- | User communication SNS token
data CommunicationToken = CommunicationToken
  { communicationTokenId :: Text -- ^ 
  , communicationTokenUserId :: Double -- ^ 
  , communicationTokenDeviceToken :: Text -- ^ 
  , communicationTokenChannel :: Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON CommunicationToken where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "communicationToken")
instance ToJSON CommunicationToken where
  toJSON = genericToJSON (removeFieldLabelPrefix False "communicationToken")

-- | 
data ConnectedUsers = ConnectedUsers
  { connectedUsersUsers :: Double -- ^ 
  , connectedUsersBots :: Double -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON ConnectedUsers where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "connectedUsers")
instance ToJSON ConnectedUsers where
  toJSON = genericToJSON (removeFieldLabelPrefix False "connectedUsers")

-- | 
data Error = Error
  { errorError :: Error_error -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Error where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "error")
instance ToJSON Error where
  toJSON = genericToJSON (removeFieldLabelPrefix False "error")

-- | 
data Error_error = Error_error
  { errorErrorMessage :: Text -- ^ 
  , errorErrorName :: Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Error_error where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "errorError")
instance ToJSON Error_error where
  toJSON = genericToJSON (removeFieldLabelPrefix False "errorError")

-- | Raw Order and Balance Data
data Execution = Execution
  { executionExecID :: Text -- ^ 
  , executionOrderID :: Text -- ^ 
  , executionClOrdID :: Text -- ^ 
  , executionClOrdLinkID :: Text -- ^ 
  , executionAccount :: Double -- ^ 
  , executionSymbol :: Text -- ^ 
  , executionSide :: Text -- ^ 
  , executionLastQty :: Double -- ^ 
  , executionLastPx :: Double -- ^ 
  , executionUnderlyingLastPx :: Double -- ^ 
  , executionLastMkt :: Text -- ^ 
  , executionLastLiquidityInd :: Text -- ^ 
  , executionSimpleOrderQty :: Double -- ^ 
  , executionOrderQty :: Double -- ^ 
  , executionPrice :: Double -- ^ 
  , executionDisplayQty :: Double -- ^ 
  , executionStopPx :: Double -- ^ 
  , executionPegOffsetValue :: Double -- ^ 
  , executionPegPriceType :: Text -- ^ 
  , executionCurrency :: Text -- ^ 
  , executionSettlCurrency :: Text -- ^ 
  , executionExecType :: Text -- ^ 
  , executionOrdType :: Text -- ^ 
  , executionTimeInForce :: Text -- ^ 
  , executionExecInst :: Text -- ^ 
  , executionContingencyType :: Text -- ^ 
  , executionExDestination :: Text -- ^ 
  , executionOrdStatus :: Text -- ^ 
  , executionTriggered :: Text -- ^ 
  , executionWorkingIndicator :: Bool -- ^ 
  , executionOrdRejReason :: Text -- ^ 
  , executionSimpleLeavesQty :: Double -- ^ 
  , executionLeavesQty :: Double -- ^ 
  , executionSimpleCumQty :: Double -- ^ 
  , executionCumQty :: Double -- ^ 
  , executionAvgPx :: Double -- ^ 
  , executionCommission :: Double -- ^ 
  , executionTradePublishIndicator :: Text -- ^ 
  , executionMultiLegReportingType :: Text -- ^ 
  , executionText :: Text -- ^ 
  , executionTrdMatchID :: Text -- ^ 
  , executionExecCost :: Double -- ^ 
  , executionExecComm :: Double -- ^ 
  , executionHomeNotional :: Double -- ^ 
  , executionForeignNotional :: Double -- ^ 
  , executionTransactTime :: Integer -- ^ 
  , executionTimestamp :: Integer -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Execution where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "execution")
instance ToJSON Execution where
  toJSON = genericToJSON (removeFieldLabelPrefix False "execution")

-- | Swap Funding History
data Funding = Funding
  { fundingTimestamp :: Integer -- ^ 
  , fundingSymbol :: Text -- ^ 
  , fundingFundingInterval :: Integer -- ^ 
  , fundingFundingRate :: Double -- ^ 
  , fundingFundingRateDaily :: Double -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Funding where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "funding")
instance ToJSON Funding where
  toJSON = genericToJSON (removeFieldLabelPrefix False "funding")

-- | Account Notifications
data GlobalNotification = GlobalNotification
  { globalNotificationId :: Double -- ^ 
  , globalNotificationDate :: Integer -- ^ 
  , globalNotificationTitle :: Text -- ^ 
  , globalNotificationBody :: Text -- ^ 
  , globalNotificationTtl :: Double -- ^ 
  , globalNotificationType :: Text -- ^ 
  , globalNotificationClosable :: Bool -- ^ 
  , globalNotificationPersist :: Bool -- ^ 
  , globalNotificationWaitForVisibility :: Bool -- ^ 
  , globalNotificationSound :: Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON GlobalNotification where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "globalNotification")
instance ToJSON GlobalNotification where
  toJSON = genericToJSON (removeFieldLabelPrefix False "globalNotification")

-- | 
data IndexComposite = IndexComposite
  { indexCompositeTimestamp :: Integer -- ^ 
  , indexCompositeSymbol :: Text -- ^ 
  , indexCompositeIndexSymbol :: Text -- ^ 
  , indexCompositeReference :: Text -- ^ 
  , indexCompositeLastPrice :: Double -- ^ 
  , indexCompositeWeight :: Double -- ^ 
  , indexCompositeLogged :: Integer -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON IndexComposite where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "indexComposite")
instance ToJSON IndexComposite where
  toJSON = genericToJSON (removeFieldLabelPrefix False "indexComposite")

-- | 
data Inline_response_200 = Inline_response_200
  { inlineResponse200Success :: Bool -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_200 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse200")
instance ToJSON Inline_response_200 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse200")

-- | 
data Inline_response_200_1 = Inline_response_200_1
  { inlineResponse2001Name :: Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_200_1 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponse2001")
instance ToJSON Inline_response_200_1 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponse2001")

-- | Tradeable Contracts, Indices, and History
data Instrument = Instrument
  { instrumentSymbol :: Text -- ^ 
  , instrumentRootSymbol :: Text -- ^ 
  , instrumentState :: Text -- ^ 
  , instrumentTyp :: Text -- ^ 
  , instrumentListing :: Integer -- ^ 
  , instrumentFront :: Integer -- ^ 
  , instrumentExpiry :: Integer -- ^ 
  , instrumentSettle :: Integer -- ^ 
  , instrumentRelistInterval :: Integer -- ^ 
  , instrumentInverseLeg :: Text -- ^ 
  , instrumentSellLeg :: Text -- ^ 
  , instrumentBuyLeg :: Text -- ^ 
  , instrumentOptionStrikePcnt :: Double -- ^ 
  , instrumentOptionStrikeRound :: Double -- ^ 
  , instrumentOptionStrikePrice :: Double -- ^ 
  , instrumentOptionMultiplier :: Double -- ^ 
  , instrumentPositionCurrency :: Text -- ^ 
  , instrumentUnderlying :: Text -- ^ 
  , instrumentQuoteCurrency :: Text -- ^ 
  , instrumentUnderlyingSymbol :: Text -- ^ 
  , instrumentReference :: Text -- ^ 
  , instrumentReferenceSymbol :: Text -- ^ 
  , instrumentCalcInterval :: Integer -- ^ 
  , instrumentPublishInterval :: Integer -- ^ 
  , instrumentPublishTime :: Integer -- ^ 
  , instrumentMaxOrderQty :: Double -- ^ 
  , instrumentMaxPrice :: Double -- ^ 
  , instrumentLotSize :: Double -- ^ 
  , instrumentTickSize :: Double -- ^ 
  , instrumentMultiplier :: Double -- ^ 
  , instrumentSettlCurrency :: Text -- ^ 
  , instrumentUnderlyingToPositionMultiplier :: Double -- ^ 
  , instrumentUnderlyingToSettleMultiplier :: Double -- ^ 
  , instrumentQuoteToSettleMultiplier :: Double -- ^ 
  , instrumentIsQuanto :: Bool -- ^ 
  , instrumentIsInverse :: Bool -- ^ 
  , instrumentInitMargin :: Double -- ^ 
  , instrumentMaintMargin :: Double -- ^ 
  , instrumentRiskLimit :: Double -- ^ 
  , instrumentRiskStep :: Double -- ^ 
  , instrumentLimit :: Double -- ^ 
  , instrumentCapped :: Bool -- ^ 
  , instrumentTaxed :: Bool -- ^ 
  , instrumentDeleverage :: Bool -- ^ 
  , instrumentMakerFee :: Double -- ^ 
  , instrumentTakerFee :: Double -- ^ 
  , instrumentSettlementFee :: Double -- ^ 
  , instrumentInsuranceFee :: Double -- ^ 
  , instrumentFundingBaseSymbol :: Text -- ^ 
  , instrumentFundingQuoteSymbol :: Text -- ^ 
  , instrumentFundingPremiumSymbol :: Text -- ^ 
  , instrumentFundingTimestamp :: Integer -- ^ 
  , instrumentFundingInterval :: Integer -- ^ 
  , instrumentFundingRate :: Double -- ^ 
  , instrumentIndicativeFundingRate :: Double -- ^ 
  , instrumentRebalanceTimestamp :: Integer -- ^ 
  , instrumentRebalanceInterval :: Integer -- ^ 
  , instrumentOpeningTimestamp :: Integer -- ^ 
  , instrumentClosingTimestamp :: Integer -- ^ 
  , instrumentSessionInterval :: Integer -- ^ 
  , instrumentPrevClosePrice :: Double -- ^ 
  , instrumentLimitDownPrice :: Double -- ^ 
  , instrumentLimitUpPrice :: Double -- ^ 
  , instrumentBankruptLimitDownPrice :: Double -- ^ 
  , instrumentBankruptLimitUpPrice :: Double -- ^ 
  , instrumentPrevTotalVolume :: Double -- ^ 
  , instrumentTotalVolume :: Double -- ^ 
  , instrumentVolume :: Double -- ^ 
  , instrumentVolume24h :: Double -- ^ 
  , instrumentPrevTotalTurnover :: Double -- ^ 
  , instrumentTotalTurnover :: Double -- ^ 
  , instrumentTurnover :: Double -- ^ 
  , instrumentTurnover24h :: Double -- ^ 
  , instrumentHomeNotional24h :: Double -- ^ 
  , instrumentForeignNotional24h :: Double -- ^ 
  , instrumentPrevPrice24h :: Double -- ^ 
  , instrumentVwap :: Double -- ^ 
  , instrumentHighPrice :: Double -- ^ 
  , instrumentLowPrice :: Double -- ^ 
  , instrumentLastPrice :: Double -- ^ 
  , instrumentLastPriceProtected :: Double -- ^ 
  , instrumentLastTickDirection :: Text -- ^ 
  , instrumentLastChangePcnt :: Double -- ^ 
  , instrumentBidPrice :: Double -- ^ 
  , instrumentMidPrice :: Double -- ^ 
  , instrumentAskPrice :: Double -- ^ 
  , instrumentImpactBidPrice :: Double -- ^ 
  , instrumentImpactMidPrice :: Double -- ^ 
  , instrumentImpactAskPrice :: Double -- ^ 
  , instrumentHasLiquidity :: Bool -- ^ 
  , instrumentOpenInterest :: Double -- ^ 
  , instrumentOpenValue :: Double -- ^ 
  , instrumentFairMethod :: Text -- ^ 
  , instrumentFairBasisRate :: Double -- ^ 
  , instrumentFairBasis :: Double -- ^ 
  , instrumentFairPrice :: Double -- ^ 
  , instrumentMarkMethod :: Text -- ^ 
  , instrumentMarkPrice :: Double -- ^ 
  , instrumentIndicativeTaxRate :: Double -- ^ 
  , instrumentIndicativeSettlePrice :: Double -- ^ 
  , instrumentOptionUnderlyingPrice :: Double -- ^ 
  , instrumentSettledPrice :: Double -- ^ 
  , instrumentTimestamp :: Integer -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Instrument where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "instrument")
instance ToJSON Instrument where
  toJSON = genericToJSON (removeFieldLabelPrefix False "instrument")

-- | 
data InstrumentInterval = InstrumentInterval
  { instrumentIntervalIntervals :: [Text] -- ^ 
  , instrumentIntervalSymbols :: [Text] -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON InstrumentInterval where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "instrumentInterval")
instance ToJSON InstrumentInterval where
  toJSON = genericToJSON (removeFieldLabelPrefix False "instrumentInterval")

-- | Insurance Fund Data
data Insurance = Insurance
  { insuranceCurrency :: Text -- ^ 
  , insuranceTimestamp :: Integer -- ^ 
  , insuranceWalletBalance :: Double -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Insurance where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "insurance")
instance ToJSON Insurance where
  toJSON = genericToJSON (removeFieldLabelPrefix False "insurance")

-- | Information on Top Users
data Leaderboard = Leaderboard
  { leaderboardName :: Text -- ^ 
  , leaderboardIsRealName :: Bool -- ^ 
  , leaderboardProfit :: Double -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Leaderboard where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "leaderboard")
instance ToJSON Leaderboard where
  toJSON = genericToJSON (removeFieldLabelPrefix False "leaderboard")

-- | Active Liquidations
data Liquidation = Liquidation
  { liquidationOrderID :: Text -- ^ 
  , liquidationSymbol :: Text -- ^ 
  , liquidationSide :: Text -- ^ 
  , liquidationPrice :: Double -- ^ 
  , liquidationLeavesQty :: Double -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Liquidation where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "liquidation")
instance ToJSON Liquidation where
  toJSON = genericToJSON (removeFieldLabelPrefix False "liquidation")

-- | 
data Margin = Margin
  { marginAccount :: Double -- ^ 
  , marginCurrency :: Text -- ^ 
  , marginRiskLimit :: Double -- ^ 
  , marginPrevState :: Text -- ^ 
  , marginState :: Text -- ^ 
  , marginAction :: Text -- ^ 
  , marginAmount :: Double -- ^ 
  , marginPendingCredit :: Double -- ^ 
  , marginPendingDebit :: Double -- ^ 
  , marginConfirmedDebit :: Double -- ^ 
  , marginPrevRealisedPnl :: Double -- ^ 
  , marginPrevUnrealisedPnl :: Double -- ^ 
  , marginGrossComm :: Double -- ^ 
  , marginGrossOpenCost :: Double -- ^ 
  , marginGrossOpenPremium :: Double -- ^ 
  , marginGrossExecCost :: Double -- ^ 
  , marginGrossMarkValue :: Double -- ^ 
  , marginRiskValue :: Double -- ^ 
  , marginTaxableMargin :: Double -- ^ 
  , marginInitMargin :: Double -- ^ 
  , marginMaintMargin :: Double -- ^ 
  , marginSessionMargin :: Double -- ^ 
  , marginTargetExcessMargin :: Double -- ^ 
  , marginVarMargin :: Double -- ^ 
  , marginRealisedPnl :: Double -- ^ 
  , marginUnrealisedPnl :: Double -- ^ 
  , marginIndicativeTax :: Double -- ^ 
  , marginUnrealisedProfit :: Double -- ^ 
  , marginSyntheticMargin :: Double -- ^ 
  , marginWalletBalance :: Double -- ^ 
  , marginMarginBalance :: Double -- ^ 
  , marginMarginBalancePcnt :: Double -- ^ 
  , marginMarginLeverage :: Double -- ^ 
  , marginMarginUsedPcnt :: Double -- ^ 
  , marginExcessMargin :: Double -- ^ 
  , marginExcessMarginPcnt :: Double -- ^ 
  , marginAvailableMargin :: Double -- ^ 
  , marginWithdrawableMargin :: Double -- ^ 
  , marginTimestamp :: Integer -- ^ 
  , marginGrossLastValue :: Double -- ^ 
  , marginCommission :: Double -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Margin where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "margin")
instance ToJSON Margin where
  toJSON = genericToJSON (removeFieldLabelPrefix False "margin")

-- | Placement, Cancellation, Amending, and History
data Order = Order
  { orderOrderID :: Text -- ^ 
  , orderClOrdID :: Text -- ^ 
  , orderClOrdLinkID :: Text -- ^ 
  , orderAccount :: Double -- ^ 
  , orderSymbol :: Text -- ^ 
  , orderSide :: Text -- ^ 
  , orderSimpleOrderQty :: Double -- ^ 
  , orderOrderQty :: Double -- ^ 
  , orderPrice :: Double -- ^ 
  , orderDisplayQty :: Double -- ^ 
  , orderStopPx :: Double -- ^ 
  , orderPegOffsetValue :: Double -- ^ 
  , orderPegPriceType :: Text -- ^ 
  , orderCurrency :: Text -- ^ 
  , orderSettlCurrency :: Text -- ^ 
  , orderOrdType :: Text -- ^ 
  , orderTimeInForce :: Text -- ^ 
  , orderExecInst :: Text -- ^ 
  , orderContingencyType :: Text -- ^ 
  , orderExDestination :: Text -- ^ 
  , orderOrdStatus :: Text -- ^ 
  , orderTriggered :: Text -- ^ 
  , orderWorkingIndicator :: Bool -- ^ 
  , orderOrdRejReason :: Text -- ^ 
  , orderSimpleLeavesQty :: Double -- ^ 
  , orderLeavesQty :: Double -- ^ 
  , orderSimpleCumQty :: Double -- ^ 
  , orderCumQty :: Double -- ^ 
  , orderAvgPx :: Double -- ^ 
  , orderMultiLegReportingType :: Text -- ^ 
  , orderText :: Text -- ^ 
  , orderTransactTime :: Integer -- ^ 
  , orderTimestamp :: Integer -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Order where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "order")
instance ToJSON Order where
  toJSON = genericToJSON (removeFieldLabelPrefix False "order")

-- | 
data OrderBookL2 = OrderBookL2
  { orderBookL2Symbol :: Text -- ^ 
  , orderBookL2Id :: Double -- ^ 
  , orderBookL2Side :: Text -- ^ 
  , orderBookL2Size :: Double -- ^ 
  , orderBookL2Price :: Double -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON OrderBookL2 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "orderBookL2")
instance ToJSON OrderBookL2 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "orderBookL2")

-- | Summary of Open and Closed Positions
data Position = Position
  { positionAccount :: Double -- ^ 
  , positionSymbol :: Text -- ^ 
  , positionCurrency :: Text -- ^ 
  , positionUnderlying :: Text -- ^ 
  , positionQuoteCurrency :: Text -- ^ 
  , positionCommission :: Double -- ^ 
  , positionInitMarginReq :: Double -- ^ 
  , positionMaintMarginReq :: Double -- ^ 
  , positionRiskLimit :: Double -- ^ 
  , positionLeverage :: Double -- ^ 
  , positionCrossMargin :: Bool -- ^ 
  , positionDeleveragePercentile :: Double -- ^ 
  , positionRebalancedPnl :: Double -- ^ 
  , positionPrevRealisedPnl :: Double -- ^ 
  , positionPrevUnrealisedPnl :: Double -- ^ 
  , positionPrevClosePrice :: Double -- ^ 
  , positionOpeningTimestamp :: Integer -- ^ 
  , positionOpeningQty :: Double -- ^ 
  , positionOpeningCost :: Double -- ^ 
  , positionOpeningComm :: Double -- ^ 
  , positionOpenOrderBuyQty :: Double -- ^ 
  , positionOpenOrderBuyCost :: Double -- ^ 
  , positionOpenOrderBuyPremium :: Double -- ^ 
  , positionOpenOrderSellQty :: Double -- ^ 
  , positionOpenOrderSellCost :: Double -- ^ 
  , positionOpenOrderSellPremium :: Double -- ^ 
  , positionExecBuyQty :: Double -- ^ 
  , positionExecBuyCost :: Double -- ^ 
  , positionExecSellQty :: Double -- ^ 
  , positionExecSellCost :: Double -- ^ 
  , positionExecQty :: Double -- ^ 
  , positionExecCost :: Double -- ^ 
  , positionExecComm :: Double -- ^ 
  , positionCurrentTimestamp :: Integer -- ^ 
  , positionCurrentQty :: Double -- ^ 
  , positionCurrentCost :: Double -- ^ 
  , positionCurrentComm :: Double -- ^ 
  , positionRealisedCost :: Double -- ^ 
  , positionUnrealisedCost :: Double -- ^ 
  , positionGrossOpenCost :: Double -- ^ 
  , positionGrossOpenPremium :: Double -- ^ 
  , positionGrossExecCost :: Double -- ^ 
  , positionIsOpen :: Bool -- ^ 
  , positionMarkPrice :: Double -- ^ 
  , positionMarkValue :: Double -- ^ 
  , positionRiskValue :: Double -- ^ 
  , positionHomeNotional :: Double -- ^ 
  , positionForeignNotional :: Double -- ^ 
  , positionPosState :: Text -- ^ 
  , positionPosCost :: Double -- ^ 
  , positionPosCost2 :: Double -- ^ 
  , positionPosCross :: Double -- ^ 
  , positionPosInit :: Double -- ^ 
  , positionPosComm :: Double -- ^ 
  , positionPosLoss :: Double -- ^ 
  , positionPosMargin :: Double -- ^ 
  , positionPosMaint :: Double -- ^ 
  , positionPosAllowance :: Double -- ^ 
  , positionTaxableMargin :: Double -- ^ 
  , positionInitMargin :: Double -- ^ 
  , positionMaintMargin :: Double -- ^ 
  , positionSessionMargin :: Double -- ^ 
  , positionTargetExcessMargin :: Double -- ^ 
  , positionVarMargin :: Double -- ^ 
  , positionRealisedGrossPnl :: Double -- ^ 
  , positionRealisedTax :: Double -- ^ 
  , positionRealisedPnl :: Double -- ^ 
  , positionUnrealisedGrossPnl :: Double -- ^ 
  , positionLongBankrupt :: Double -- ^ 
  , positionShortBankrupt :: Double -- ^ 
  , positionTaxBase :: Double -- ^ 
  , positionIndicativeTaxRate :: Double -- ^ 
  , positionIndicativeTax :: Double -- ^ 
  , positionUnrealisedTax :: Double -- ^ 
  , positionUnrealisedPnl :: Double -- ^ 
  , positionUnrealisedPnlPcnt :: Double -- ^ 
  , positionUnrealisedRoePcnt :: Double -- ^ 
  , positionSimpleQty :: Double -- ^ 
  , positionSimpleCost :: Double -- ^ 
  , positionSimpleValue :: Double -- ^ 
  , positionSimplePnl :: Double -- ^ 
  , positionSimplePnlPcnt :: Double -- ^ 
  , positionAvgCostPrice :: Double -- ^ 
  , positionAvgEntryPrice :: Double -- ^ 
  , positionBreakEvenPrice :: Double -- ^ 
  , positionMarginCallPrice :: Double -- ^ 
  , positionLiquidationPrice :: Double -- ^ 
  , positionBankruptPrice :: Double -- ^ 
  , positionTimestamp :: Integer -- ^ 
  , positionLastPrice :: Double -- ^ 
  , positionLastValue :: Double -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Position where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "position")
instance ToJSON Position where
  toJSON = genericToJSON (removeFieldLabelPrefix False "position")

-- | Best Bid/Offer Snapshots &amp; Historical Bins
data Quote = Quote
  { quoteTimestamp :: Integer -- ^ 
  , quoteSymbol :: Text -- ^ 
  , quoteBidSize :: Double -- ^ 
  , quoteBidPrice :: Double -- ^ 
  , quoteAskPrice :: Double -- ^ 
  , quoteAskSize :: Double -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Quote where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "quote")
instance ToJSON Quote where
  toJSON = genericToJSON (removeFieldLabelPrefix False "quote")

-- | Historical Settlement Data
data Settlement = Settlement
  { settlementTimestamp :: Integer -- ^ 
  , settlementSymbol :: Text -- ^ 
  , settlementSettlementType :: Text -- ^ 
  , settlementSettledPrice :: Double -- ^ 
  , settlementOptionStrikePrice :: Double -- ^ 
  , settlementOptionUnderlyingPrice :: Double -- ^ 
  , settlementBankrupt :: Double -- ^ 
  , settlementTaxBase :: Double -- ^ 
  , settlementTaxRate :: Double -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Settlement where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "settlement")
instance ToJSON Settlement where
  toJSON = genericToJSON (removeFieldLabelPrefix False "settlement")

-- | Exchange Statistics
data Stats = Stats
  { statsRootSymbol :: Text -- ^ 
  , statsCurrency :: Text -- ^ 
  , statsVolume24h :: Double -- ^ 
  , statsTurnover24h :: Double -- ^ 
  , statsOpenInterest :: Double -- ^ 
  , statsOpenValue :: Double -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Stats where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "stats")
instance ToJSON Stats where
  toJSON = genericToJSON (removeFieldLabelPrefix False "stats")

-- | 
data StatsHistory = StatsHistory
  { statsHistoryDate :: Integer -- ^ 
  , statsHistoryRootSymbol :: Text -- ^ 
  , statsHistoryCurrency :: Text -- ^ 
  , statsHistoryVolume :: Double -- ^ 
  , statsHistoryTurnover :: Double -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON StatsHistory where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "statsHistory")
instance ToJSON StatsHistory where
  toJSON = genericToJSON (removeFieldLabelPrefix False "statsHistory")

-- | 
data StatsUSD = StatsUSD
  { statsUSDRootSymbol :: Text -- ^ 
  , statsUSDCurrency :: Text -- ^ 
  , statsUSDTurnover24h :: Double -- ^ 
  , statsUSDTurnover30d :: Double -- ^ 
  , statsUSDTurnover365d :: Double -- ^ 
  , statsUSDTurnover :: Double -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON StatsUSD where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "statsUSD")
instance ToJSON StatsUSD where
  toJSON = genericToJSON (removeFieldLabelPrefix False "statsUSD")

-- | Individual &amp; Bucketed Trades
data Trade = Trade
  { tradeTimestamp :: Integer -- ^ 
  , tradeSymbol :: Text -- ^ 
  , tradeSide :: Text -- ^ 
  , tradeSize :: Double -- ^ 
  , tradePrice :: Double -- ^ 
  , tradeTickDirection :: Text -- ^ 
  , tradeTrdMatchID :: Text -- ^ 
  , tradeGrossValue :: Double -- ^ 
  , tradeHomeNotional :: Double -- ^ 
  , tradeForeignNotional :: Double -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Trade where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "trade")
instance ToJSON Trade where
  toJSON = genericToJSON (removeFieldLabelPrefix False "trade")

-- | 
data TradeBin = TradeBin
  { tradeBinTimestamp :: Integer -- ^ 
  , tradeBinSymbol :: Text -- ^ 
  , tradeBinOpen :: Double -- ^ 
  , tradeBinHigh :: Double -- ^ 
  , tradeBinLow :: Double -- ^ 
  , tradeBinClose :: Double -- ^ 
  , tradeBinTrades :: Double -- ^ 
  , tradeBinVolume :: Double -- ^ 
  , tradeBinVwap :: Double -- ^ 
  , tradeBinLastSize :: Double -- ^ 
  , tradeBinTurnover :: Double -- ^ 
  , tradeBinHomeNotional :: Double -- ^ 
  , tradeBinForeignNotional :: Double -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON TradeBin where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "tradeBin")
instance ToJSON TradeBin where
  toJSON = genericToJSON (removeFieldLabelPrefix False "tradeBin")

-- | 
data Transaction = Transaction
  { transactionTransactID :: Text -- ^ 
  , transactionAccount :: Double -- ^ 
  , transactionCurrency :: Text -- ^ 
  , transactionTransactType :: Text -- ^ 
  , transactionAmount :: Double -- ^ 
  , transactionFee :: Double -- ^ 
  , transactionTransactStatus :: Text -- ^ 
  , transactionAddress :: Text -- ^ 
  , transactionTx :: Text -- ^ 
  , transactionText :: Text -- ^ 
  , transactionTransactTime :: Integer -- ^ 
  , transactionTimestamp :: Integer -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Transaction where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "transaction")
instance ToJSON Transaction where
  toJSON = genericToJSON (removeFieldLabelPrefix False "transaction")

-- | Account Operations
data User = User
  { userId :: Double -- ^ 
  , userOwnerId :: Double -- ^ 
  , userFirstname :: Text -- ^ 
  , userLastname :: Text -- ^ 
  , userUsername :: Text -- ^ 
  , userEmail :: Text -- ^ 
  , userPhone :: Text -- ^ 
  , userCreated :: Integer -- ^ 
  , userLastUpdated :: Integer -- ^ 
  , userPreferences :: UserPreferences -- ^ 
  , userRestrictedEngineFields :: Value -- ^ 
  , userTFAEnabled :: Text -- ^ 
  , userAffiliateID :: Text -- ^ 
  , userPgpPubKey :: Text -- ^ 
  , userCountry :: Text -- ^ 
  , userGeoipCountry :: Text -- ^ 
  , userGeoipRegion :: Text -- ^ 
  , userTyp :: Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "user")
instance ToJSON User where
  toJSON = genericToJSON (removeFieldLabelPrefix False "user")

-- | 
data UserCommission = UserCommission
  { userCommissionMakerFee :: Double -- ^ 
  , userCommissionTakerFee :: Double -- ^ 
  , userCommissionSettlementFee :: Double -- ^ 
  , userCommissionMaxFee :: Double -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON UserCommission where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "userCommission")
instance ToJSON UserCommission where
  toJSON = genericToJSON (removeFieldLabelPrefix False "userCommission")

-- | 
newtype UserCommissionsBySymbol = UserCommissionsBySymbol { unUserCommissionsBySymbol :: (Map.Map Text UserCommission) }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- | User Events for auditing
data UserEvent = UserEvent
  { userEventId :: Double -- ^ 
  , userEventType :: Text -- ^ 
  , userEventStatus :: Text -- ^ 
  , userEventUserId :: Double -- ^ 
  , userEventCreatedById :: Double -- ^ 
  , userEventIp :: Text -- ^ 
  , userEventGeoipCountry :: Text -- ^ 
  , userEventGeoipRegion :: Text -- ^ 
  , userEventGeoipSubRegion :: Text -- ^ 
  , userEventEventMeta :: Value -- ^ 
  , userEventCreated :: Integer -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON UserEvent where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "userEvent")
instance ToJSON UserEvent where
  toJSON = genericToJSON (removeFieldLabelPrefix False "userEvent")

-- | 
data UserPreferences = UserPreferences
  { userPreferencesAlertOnLiquidations :: Bool -- ^ 
  , userPreferencesAnimationsEnabled :: Bool -- ^ 
  , userPreferencesAnnouncementsLastSeen :: Integer -- ^ 
  , userPreferencesChatChannelID :: Double -- ^ 
  , userPreferencesColorTheme :: Text -- ^ 
  , userPreferencesCurrency :: Text -- ^ 
  , userPreferencesDebug :: Bool -- ^ 
  , userPreferencesDisableEmails :: [Text] -- ^ 
  , userPreferencesDisablePush :: [Text] -- ^ 
  , userPreferencesHideConfirmDialogs :: [Text] -- ^ 
  , userPreferencesHideConnectionModal :: Bool -- ^ 
  , userPreferencesHideFromLeaderboard :: Bool -- ^ 
  , userPreferencesHideNameFromLeaderboard :: Bool -- ^ 
  , userPreferencesHideNotifications :: [Text] -- ^ 
  , userPreferencesLocale :: Text -- ^ 
  , userPreferencesMsgsSeen :: [Text] -- ^ 
  , userPreferencesOrderBookBinning :: Value -- ^ 
  , userPreferencesOrderBookType :: Text -- ^ 
  , userPreferencesOrderClearImmediate :: Bool -- ^ 
  , userPreferencesOrderControlsPlusMinus :: Bool -- ^ 
  , userPreferencesShowLocaleNumbers :: Bool -- ^ 
  , userPreferencesSounds :: [Text] -- ^ 
  , userPreferencesStrictIPCheck :: Bool -- ^ 
  , userPreferencesStrictTimeout :: Bool -- ^ 
  , userPreferencesTickerGroup :: Text -- ^ 
  , userPreferencesTickerPinned :: Bool -- ^ 
  , userPreferencesTradeLayout :: Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON UserPreferences where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "userPreferences")
instance ToJSON UserPreferences where
  toJSON = genericToJSON (removeFieldLabelPrefix False "userPreferences")

-- | 
data Wallet = Wallet
  { walletAccount :: Double -- ^ 
  , walletCurrency :: Text -- ^ 
  , walletPrevDeposited :: Double -- ^ 
  , walletPrevWithdrawn :: Double -- ^ 
  , walletPrevTransferIn :: Double -- ^ 
  , walletPrevTransferOut :: Double -- ^ 
  , walletPrevAmount :: Double -- ^ 
  , walletPrevTimestamp :: Integer -- ^ 
  , walletDeltaDeposited :: Double -- ^ 
  , walletDeltaWithdrawn :: Double -- ^ 
  , walletDeltaTransferIn :: Double -- ^ 
  , walletDeltaTransferOut :: Double -- ^ 
  , walletDeltaAmount :: Double -- ^ 
  , walletDeposited :: Double -- ^ 
  , walletWithdrawn :: Double -- ^ 
  , walletTransferIn :: Double -- ^ 
  , walletTransferOut :: Double -- ^ 
  , walletAmount :: Double -- ^ 
  , walletPendingCredit :: Double -- ^ 
  , walletPendingDebit :: Double -- ^ 
  , walletConfirmedDebit :: Double -- ^ 
  , walletTimestamp :: Integer -- ^ 
  , walletAddr :: Text -- ^ 
  , walletScript :: Text -- ^ 
  , walletWithdrawalLock :: [Text] -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Wallet where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "wallet")
instance ToJSON Wallet where
  toJSON = genericToJSON (removeFieldLabelPrefix False "wallet")

-- | 
data Xany = Xany
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON Xany where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "xany")
instance ToJSON Xany where
  toJSON = genericToJSON (removeFieldLabelPrefix False "xany")

-- Remove a field label prefix during JSON parsing.
-- Also perform any replacements for special characters.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
  {fieldLabelModifier = fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars}
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars =
      [ ("@", "'At")
      , ("\\", "'Back_Slash")
      , ("<=", "'Less_Than_Or_Equal_To")
      , ("\"", "'Double_Quote")
      , ("[", "'Left_Square_Bracket")
      , ("]", "'Right_Square_Bracket")
      , ("^", "'Caret")
      , ("_", "'Underscore")
      , ("`", "'Backtick")
      , ("!", "'Exclamation")
      , ("#", "'Hash")
      , ("$", "'Dollar")
      , ("%", "'Percent")
      , ("&", "'Ampersand")
      , ("'", "'Quote")
      , ("(", "'Left_Parenthesis")
      , (")", "'Right_Parenthesis")
      , ("*", "'Star")
      , ("+", "'Plus")
      , (",", "'Comma")
      , ("-", "'Dash")
      , (".", "'Period")
      , ("/", "'Slash")
      , (":", "'Colon")
      , ("{", "'Left_Curly_Bracket")
      , ("|", "'Pipe")
      , ("<", "'LessThan")
      , ("!=", "'Not_Equal")
      , ("=", "'Equal")
      , ("}", "'Right_Curly_Bracket")
      , (">", "'GreaterThan")
      , ("~", "'Tilde")
      , ("?", "'Question_Mark")
      , (">=", "'Greater_Than_Or_Equal_To")
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace
