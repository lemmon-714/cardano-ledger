{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains just the type of protocol parameters.
module Cardano.Ledger.Shelley.PParams
  ( ShelleyPParams,
    ShelleyPParamsHKD (..),
    PPUPState (..),
    emptyPParams,
    HKD,
    HKDFunctor (..),
    PPUpdateEnv (..),
    ProposedPPUpdates (..),
    emptyPPPUpdates,
    ShelleyPParamsUpdate,
    emptyPParamsUpdate,
    Update (..),
    updatePParams,
    pvCanFollow,

    -- * Deprecated
    PParams,
    PParams',
    PParamsUpdate,
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    decodeWord,
    encodeListLen,
    encodeMapLen,
    encodeWord,
  )
import Cardano.Ledger.BaseTypes
  ( NonNegativeInterval,
    Nonce (NeutralNonce),
    StrictMaybe (..),
    UnitInterval,
    fromSMaybe,
    invalidKey,
    strictMaybeToMaybe,
  )
import qualified Cardano.Ledger.BaseTypes as BT
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (Era (EraCrypto), EraPParams (applyPPUpdates))
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.HKD (HKD, HKDFunctor (..))
import Cardano.Ledger.Keys (GenDelegs, KeyHash, KeyRole (..))
import Cardano.Ledger.Serialization
  ( FromCBORGroup (..),
    ToCBORGroup (..),
    decodeMapContents,
    decodeRecordNamed,
    mapFromCBOR,
    mapToCBOR,
  )
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.Orphans ()
import Cardano.Ledger.Slot (EpochNo (..), SlotNo (..))
import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Data.Aeson (FromJSON (..), ToJSON (..), (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Coders
  ( Decode (From, RecD),
    decode,
    (<!),
  )
import Data.Default.Class (Default, def)
import Data.Foldable (fold)
import Data.Functor.Identity (Identity)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)
import Lens.Micro (lens)

-- ====================================================================

type PParams era = ShelleyPParams era

{-# DEPRECATED PParams "Use `ShelleyPParams` instead" #-}

type PParams' f era = ShelleyPParamsHKD f era

{-# DEPRECATED PParams' "Use `ShelleyPParamsHKD` instead" #-}

type PParamsUpdate era = ShelleyPParamsUpdate era

{-# DEPRECATED PParamsUpdate "Use `ShelleyPParamsUpdate` instead" #-}

-- | Protocol parameters.
--
-- We use the `HKD` type family so that the protocol parameters type and
-- the type for the updates to the protocol parameters can share records fields.
-- The protocol parameters will have type 'PParams'' 'Identity', and the updates
-- will have type 'PParams'' 'StrictMaybe', though 'Identity' will be hidden from use.
--
-- For example:
--
-- @
--   myParameters =
--     PParams
--       { _minfeeA = 0,
--         _minfeeB = 0,
--         ...
--       }
--
--   myUpdate =
--     PParamsUpdate
--       { _minfeeA = SNothing,
--         _minfeeB = SJust 42,
--         ...
--       }
-- @
data ShelleyPParamsHKD f era = ShelleyPParams
  { -- | The linear factor for the minimum fee calculation
    minfeeA :: !(HKD f Natural),
    -- | The constant factor for the minimum fee calculation
    minfeeB :: !(HKD f Natural),
    -- | Maximal block body size
    maxBBSize :: !(HKD f Natural),
    -- | Maximal transaction size
    maxTxSize :: !(HKD f Natural),
    -- | Maximal block header size
    maxBHSize :: !(HKD f Natural),
    -- | The amount of a key registration deposit
    keyDeposit :: !(HKD f Coin),
    -- | The amount of a pool registration deposit
    poolDeposit :: !(HKD f Coin),
    -- | epoch bound on pool retirement
    eMax :: !(HKD f EpochNo),
    -- | Desired number of pools
    nOpt :: !(HKD f Natural),
    -- | Pool influence
    a0 :: !(HKD f NonNegativeInterval),
    -- | Monetary expansion
    rho :: !(HKD f UnitInterval),
    -- | Treasury expansion
    tau :: !(HKD f UnitInterval),
    -- | Decentralization parameter
    d :: !(HKD f UnitInterval),
    -- | Extra entropy
    extraEntropy :: !(HKD f Nonce),
    -- | Protocol version
    protocolVersion :: !(HKD f BT.ProtVer),
    -- | Minimum UTxO value
    minUTxOValue :: !(HKD f Coin),
    -- | Minimum Stake Pool Cost
    minPoolCost :: !(HKD f Coin)
  }
  deriving (Generic)

type ShelleyPParams era = ShelleyPParamsHKD Identity era

type ShelleyPParamsUpdate era = ShelleyPParamsHKD StrictMaybe era

instance (CC.Crypto c) => EraPParams (ShelleyEra c) where
  type PParams (ShelleyEra c) = ShelleyPParams (ShelleyEra c)
  type PParamsUpdate (ShelleyEra c) = ShelleyPParamsUpdate (ShelleyEra c)

  emptyPParams = def
  emptyPParamsUpdate = def

  applyPPUpdates = updatePParams

  ppMinFeeAL = lens minfeeA $ \pp x -> pp{minfeeA = x}
  ppMinFeeBL = lens minfeeB $ \pp x -> pp{minfeeB = x}
  maxBBSizeL = lens maxBBSize $ \pp x -> pp{maxBBSize = x}
  maxTxSizeL = lens maxTxSize $ \pp x -> pp{maxTxSize = x}
  maxBHSizeL = lens maxBHSize $ \pp x -> pp{maxBHSize = x}
  keyDepositL = lens keyDeposit $ \pp x -> pp{keyDeposit = x}
  poolDepositL = lens poolDeposit $ \pp x -> pp{poolDeposit = x}
  eMaxL = lens eMax $ \pp x -> pp{eMax = x}
  nOptL = lens nOpt $ \pp x -> pp{nOpt = x}
  a0L = lens a0 $ \pp x -> pp{a0 = x}
  rhoL = lens rho $ \pp x -> pp{rho = x}
  tauL = lens tau $ \pp x -> pp{tau = x}
  dL = lens d $ \pp x -> pp{d = x}
  extraEntropyL = lens extraEntropy $ \pp x -> pp{extraEntropy = x}
  protocolVersionL = lens protocolVersion $ \pp x -> pp{protocolVersion = x}
  minUTxOValueL = lens minUTxOValue $ \pp x -> pp{minUTxOValue = x}
  minPoolCostL = lens minPoolCost $ \pp x -> pp{minPoolCost = x}

deriving instance Eq (PParams' Identity era)

deriving instance Show (PParams' Identity era)

deriving instance NFData (PParams' Identity era)

instance NoThunks (ShelleyPParams era)

instance (Era era) => ToCBOR (ShelleyPParams era) where
  toCBOR
    ShelleyPParams
      { minfeeA = minfeeA',
        minfeeB = minfeeB',
        maxBBSize = maxBBSize',
        maxTxSize = maxTxSize',
        maxBHSize = maxBHSize',
        keyDeposit = keyDeposit',
        poolDeposit = poolDeposit',
        eMax = eMax',
        nOpt = nOpt',
        a0 = a0',
        rho = rho',
        tau = tau',
        d = d',
        extraEntropy = extraEntropy',
        protocolVersion = protocolVersion',
        minUTxOValue = minUTxOValue',
        minPoolCost = minPoolCost'
      } =
      encodeListLen 18
        <> toCBOR minfeeA'
        <> toCBOR minfeeB'
        <> toCBOR maxBBSize'
        <> toCBOR maxTxSize'
        <> toCBOR maxBHSize'
        <> toCBOR keyDeposit'
        <> toCBOR poolDeposit'
        <> toCBOR eMax'
        <> toCBOR nOpt'
        <> toCBOR a0'
        <> toCBOR rho'
        <> toCBOR tau'
        <> toCBOR d'
        <> toCBOR extraEntropy'
        <> toCBORGroup protocolVersion'
        <> toCBOR minUTxOValue'
        <> toCBOR minPoolCost'

instance (Era era) => FromCBOR (ShelleyPParams era) where
  fromCBOR = do
    decodeRecordNamed "ShelleyPParams" (const 18) $
      ShelleyPParams
        <$> fromCBOR -- _minfeeA         :: Integer
        <*> fromCBOR -- _minfeeB         :: Natural
        <*> fromCBOR -- _maxBBSize       :: Natural
        <*> fromCBOR -- _maxTxSize       :: Natural
        <*> fromCBOR -- _maxBHSize       :: Natural
        <*> fromCBOR -- _keyDeposit      :: Coin
        <*> fromCBOR -- _poolDeposit     :: Coin
        <*> fromCBOR -- _eMax            :: EpochNo
        <*> fromCBOR -- _nOpt            :: Natural
        <*> fromCBOR -- _a0              :: NonNegativeInterval
        <*> fromCBOR -- _rho             :: UnitInterval
        <*> fromCBOR -- _tau             :: UnitInterval
        <*> fromCBOR -- _d               :: UnitInterval
        <*> fromCBOR -- _extraEntropy    :: Nonce
        <*> fromCBORGroup -- _protocolVersion :: ProtVer
        <*> fromCBOR -- _minUTxOValue    :: Natural
        <*> fromCBOR -- _minPoolCost     :: Natural

instance ToJSON (ShelleyPParams era) where
  toJSON pp =
    Aeson.object
      [ "minFeeA" .= minfeeA pp,
        "minFeeB" .= minfeeB pp,
        "maxBlockBodySize" .= maxBBSize pp,
        "maxTxSize" .= maxTxSize pp,
        "maxBlockHeaderSize" .= maxBHSize pp,
        "keyDeposit" .= keyDeposit pp,
        "poolDeposit" .= poolDeposit pp,
        "eMax" .= eMax pp,
        "nOpt" .= nOpt pp,
        "a0" .= a0 pp,
        "rho" .= rho pp,
        "tau" .= tau pp,
        "decentralisationParam" .= d pp,
        "extraEntropy" .= extraEntropy pp,
        "protocolVersion" .= protocolVersion pp,
        "minUTxOValue" .= minUTxOValue pp,
        "minPoolCost" .= minPoolCost pp
      ]

instance FromJSON (ShelleyPParams era) where
  parseJSON =
    Aeson.withObject "ShelleyPParams" $ \obj ->
      ShelleyPParams
        <$> obj .: "minFeeA"
        <*> obj .: "minFeeB"
        <*> obj .: "maxBlockBodySize"
        <*> obj .: "maxTxSize"
        <*> obj .: "maxBlockHeaderSize"
        <*> obj .: "keyDeposit"
        <*> obj .: "poolDeposit"
        <*> obj .: "eMax"
        <*> obj .: "nOpt"
        <*> obj .: "a0"
        <*> obj .: "rho"
        <*> obj .: "tau"
        <*> obj .: "decentralisationParam"
        <*> obj .: "extraEntropy"
        <*> obj .: "protocolVersion"
        <*> obj .:? "minUTxOValue" .!= mempty
        <*> obj .:? "minPoolCost" .!= mempty

instance Default (ShelleyPParams era) where
  def = emptyPParams

-- | Returns a basic "empty" `PParams` structure with all zero values.
emptyPParams :: ShelleyPParams era
emptyPParams =
  ShelleyPParams
    { minfeeA = 0,
      minfeeB = 0,
      maxBBSize = 0,
      maxTxSize = 2048,
      maxBHSize = 0,
      keyDeposit = Coin 0,
      poolDeposit = Coin 0,
      eMax = EpochNo 0,
      nOpt = 100,
      a0 = minBound,
      rho = minBound,
      tau = minBound,
      d = minBound,
      extraEntropy = NeutralNonce,
      protocolVersion = BT.ProtVer 0 0,
      minUTxOValue = mempty,
      minPoolCost = mempty
    }

-- | Update Proposal
data Update era
  = Update !(ProposedPPUpdates era) !EpochNo
  deriving (Generic)

deriving instance Eq (Core.PParamsUpdate era) => Eq (Update era)

deriving instance NFData (Core.PParamsUpdate era) => NFData (Update era)

deriving instance Show (Core.PParamsUpdate era) => Show (Update era)

instance NoThunks (Core.PParamsUpdate era) => NoThunks (Update era)

instance (Era era, ToCBOR (Core.PParamsUpdate era)) => ToCBOR (Update era) where
  toCBOR (Update ppUpdate e) =
    encodeListLen 2 <> toCBOR ppUpdate <> toCBOR e

instance
  (Era era, FromCBOR (Core.PParamsUpdate era)) =>
  FromCBOR (Update era)
  where
  fromCBOR = decode $ RecD Update <! From <! From

data PPUpdateEnv era = PPUpdateEnv SlotNo (GenDelegs era)
  deriving (Show, Eq, Generic)

instance NoThunks (PPUpdateEnv era)

deriving instance Eq (PParams' StrictMaybe era)

deriving instance Show (PParams' StrictMaybe era)

deriving instance Ord (PParams' StrictMaybe era)

deriving instance NFData (PParams' StrictMaybe era)

instance NoThunks (PParamsUpdate era)

instance (Era era) => ToCBOR (PParamsUpdate era) where
  toCBOR ppup =
    let l =
          mapMaybe
            strictMaybeToMaybe
            [ encodeMapElement 0 toCBOR =<< minfeeA ppup,
              encodeMapElement 1 toCBOR =<< minfeeB ppup,
              encodeMapElement 2 toCBOR =<< maxBBSize ppup,
              encodeMapElement 3 toCBOR =<< maxTxSize ppup,
              encodeMapElement 4 toCBOR =<< maxBHSize ppup,
              encodeMapElement 5 toCBOR =<< keyDeposit ppup,
              encodeMapElement 6 toCBOR =<< poolDeposit ppup,
              encodeMapElement 7 toCBOR =<< eMax ppup,
              encodeMapElement 8 toCBOR =<< nOpt ppup,
              encodeMapElement 9 toCBOR =<< a0 ppup,
              encodeMapElement 10 toCBOR =<< rho ppup,
              encodeMapElement 11 toCBOR =<< tau ppup,
              encodeMapElement 12 toCBOR =<< d ppup,
              encodeMapElement 13 toCBOR =<< extraEntropy ppup,
              encodeMapElement 14 toCBOR =<< protocolVersion ppup,
              encodeMapElement 15 toCBOR =<< minUTxOValue ppup,
              encodeMapElement 16 toCBOR =<< minPoolCost ppup
            ]
        n = fromIntegral $ length l
     in encodeMapLen n <> fold l
    where
      encodeMapElement ix encoder x = SJust (encodeWord ix <> encoder x)

instance Default (PParamsUpdate era) where
  def = emptyPParamsUpdate

emptyPParamsUpdate :: PParamsUpdate era
emptyPParamsUpdate =
  ShelleyPParams
    { minfeeA = SNothing,
      minfeeB = SNothing,
      maxBBSize = SNothing,
      maxTxSize = SNothing,
      maxBHSize = SNothing,
      keyDeposit = SNothing,
      poolDeposit = SNothing,
      eMax = SNothing,
      nOpt = SNothing,
      a0 = SNothing,
      rho = SNothing,
      tau = SNothing,
      d = SNothing,
      extraEntropy = SNothing,
      protocolVersion = SNothing,
      minUTxOValue = SNothing,
      minPoolCost = SNothing
    }

instance (Era era) => FromCBOR (PParamsUpdate era) where
  fromCBOR = do
    mapParts <-
      decodeMapContents $
        decodeWord >>= \case
          0 -> fromCBOR >>= \x -> pure (0, \up -> up {minfeeA = SJust x})
          1 -> fromCBOR >>= \x -> pure (1, \up -> up {minfeeB = SJust x})
          2 -> fromCBOR >>= \x -> pure (2, \up -> up {maxBBSize = SJust x})
          3 -> fromCBOR >>= \x -> pure (3, \up -> up {maxTxSize = SJust x})
          4 -> fromCBOR >>= \x -> pure (4, \up -> up {maxBHSize = SJust x})
          5 -> fromCBOR >>= \x -> pure (5, \up -> up {keyDeposit = SJust x})
          6 -> fromCBOR >>= \x -> pure (6, \up -> up {poolDeposit = SJust x})
          7 -> fromCBOR >>= \x -> pure (7, \up -> up {eMax = SJust x})
          8 -> fromCBOR >>= \x -> pure (8, \up -> up {nOpt = SJust x})
          9 -> fromCBOR >>= \x -> pure (9, \up -> up {a0 = SJust x})
          10 -> fromCBOR >>= \x -> pure (10, \up -> up {rho = SJust x})
          11 -> fromCBOR >>= \x -> pure (11, \up -> up {tau = SJust x})
          12 -> fromCBOR >>= \x -> pure (12, \up -> up {d = SJust x})
          13 -> fromCBOR >>= \x -> pure (13, \up -> up {extraEntropy = SJust x})
          14 -> fromCBOR >>= \x -> pure (14, \up -> up {protocolVersion = SJust x})
          15 -> fromCBOR >>= \x -> pure (15, \up -> up {minUTxOValue = SJust x})
          16 -> fromCBOR >>= \x -> pure (16, \up -> up {minPoolCost = SJust x})
          k -> invalidKey k
    let fields = fst <$> mapParts :: [Int]
    unless
      (nub fields == fields)
      (fail $ "duplicate keys: " <> show fields)
    pure $ foldr ($) emptyPParamsUpdate (snd <$> mapParts)

-- | Update operation for protocol parameters structure @PParams
newtype ProposedPPUpdates era
  = ProposedPPUpdates (Map (KeyHash 'Genesis (EraCrypto era)) (Core.PParamsUpdate era))
  deriving (Generic)

deriving instance Eq (Core.PParamsUpdate era) => Eq (ProposedPPUpdates era)

deriving instance NFData (Core.PParamsUpdate era) => NFData (ProposedPPUpdates era)

deriving instance Show (Core.PParamsUpdate era) => Show (ProposedPPUpdates era)

instance NoThunks (Core.PParamsUpdate era) => NoThunks (ProposedPPUpdates era)

instance
  (Era era, ToCBOR (Core.PParamsUpdate era)) =>
  ToCBOR (ProposedPPUpdates era)
  where
  toCBOR (ProposedPPUpdates m) = mapToCBOR m

instance
  (Era era, FromCBOR (Core.PParamsUpdate era)) =>
  FromCBOR (ProposedPPUpdates era)
  where
  fromCBOR = ProposedPPUpdates <$> mapFromCBOR

emptyPPPUpdates :: ProposedPPUpdates era
emptyPPPUpdates = ProposedPPUpdates Map.empty

updatePParams :: PParams era -> PParamsUpdate era -> PParams era
updatePParams pp ppup =
  ShelleyPParams
    { minfeeA = fromSMaybe (minfeeA pp) (minfeeA ppup),
      minfeeB = fromSMaybe (minfeeB pp) (minfeeB ppup),
      maxBBSize = fromSMaybe (maxBBSize pp) (maxBBSize ppup),
      maxTxSize = fromSMaybe (maxTxSize pp) (maxTxSize ppup),
      maxBHSize = fromSMaybe (maxBHSize pp) (maxBHSize ppup),
      keyDeposit = fromSMaybe (keyDeposit pp) (keyDeposit ppup),
      poolDeposit = fromSMaybe (poolDeposit pp) (poolDeposit ppup),
      eMax = fromSMaybe (eMax pp) (eMax ppup),
      nOpt = fromSMaybe (nOpt pp) (nOpt ppup),
      a0 = fromSMaybe (a0 pp) (a0 ppup),
      rho = fromSMaybe (rho pp) (rho ppup),
      tau = fromSMaybe (tau pp) (tau ppup),
      d = fromSMaybe (d pp) (d ppup),
      extraEntropy = fromSMaybe (extraEntropy pp) (extraEntropy ppup),
      protocolVersion = fromSMaybe (protocolVersion pp) (protocolVersion ppup),
      minUTxOValue = fromSMaybe (minUTxOValue pp) (minUTxOValue ppup),
      minPoolCost = fromSMaybe (minPoolCost pp) (minPoolCost ppup)
    }

data PPUPState era = PPUPState
  { proposals :: !(ProposedPPUpdates era),
    futureProposals :: !(ProposedPPUpdates era)
  }
  deriving (Generic)

deriving instance Show (Core.PParamsUpdate era) => Show (PPUPState era)

deriving instance Eq (Core.PParamsUpdate era) => Eq (PPUPState era)

deriving instance NFData (Core.PParamsUpdate era) => NFData (PPUPState era)

instance NoThunks (Core.PParamsUpdate era) => NoThunks (PPUPState era)

instance (Era era, ToCBOR (Core.PParamsUpdate era)) => ToCBOR (PPUPState era) where
  toCBOR (PPUPState ppup fppup) =
    encodeListLen 2 <> toCBOR ppup <> toCBOR fppup

instance
  (Era era, FromCBOR (Core.PParamsUpdate era)) =>
  FromCBOR (PPUPState era)
  where
  fromCBOR =
    decode $
      RecD PPUPState
        <! From
        <! From

instance Default (PPUPState era) where
  def = PPUPState emptyPPPUpdates emptyPPPUpdates

pvCanFollow :: BT.ProtVer -> StrictMaybe BT.ProtVer -> Bool
pvCanFollow _ SNothing = True
pvCanFollow (BT.ProtVer m n) (SJust (BT.ProtVer m' n')) =
  (m + 1, 0) == (m', n') || (m, n + 1) == (m', n')
