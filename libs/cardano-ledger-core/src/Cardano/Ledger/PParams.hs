{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.PParams
  ( EraPParams (..),
    PParams (..),
    PParamsUpdate (..),

    -- * PParams lens helpers
    ppMinFeeAL,
    ppMinFeeBL,
    ppMaxBBSizeL,
    ppMaxTxSizeL,
    ppMaxBHSizeL,
    ppKeyDepositL,
    ppPoolDepositL,
    ppEMaxL,
    ppNOptL,
    ppA0L,
    ppRhoL,
    ppTauL,
    ppDL,
    ppExtraEntropyL,
    ppProtocolVersionL,
    ppMinUTxOValueL,
    ppMinPoolCostL,

    -- * PParamsUpdate lens helpers
    ppuMinFeeAL,
    ppuMinFeeBL,
    ppuMaxBBSizeL,
    ppuMaxTxSizeL,
    ppuMaxBHSizeL,
    ppuKeyDepositL,
    ppuPoolDepositL,
    ppuEMaxL,
    ppuNOptL,
    ppuA0L,
    ppuRhoL,
    ppuTauL,
    ppuDL,
    ppuExtraEntropyL,
    ppuProtocolVersionL,
    ppuMinUTxOValueL,
    ppuMinPoolCostL,

    -- * Deprecated
    PParamsDelta,
  )
where

import Cardano.Binary (FromCBOR, ToCBOR)
import Cardano.Ledger.BaseTypes (NonNegativeInterval, Nonce (..), UnitInterval)
import qualified Cardano.Ledger.BaseTypes as BT
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Era.Class (Era (..))
import Cardano.Ledger.HKD (HKD)
import Cardano.Ledger.Slot (EpochNo (..))
import Control.DeepSeq (NFData)
import Control.Monad.Identity (Identity)
import Data.Default.Class (Default)
import Data.Kind (Type)
import Data.Maybe.Strict (StrictMaybe)
import GHC.Natural (Natural)
import Lens.Micro (Lens', lens)
import NoThunks.Class (NoThunks)

-- | Protocol parameters
newtype PParams era = PParams (PParamsHKD Identity era)

deriving newtype instance
  Eq (PParamsHKD Identity era) => Eq (PParams era)

deriving newtype instance 
  Default (PParamsHKD Identity era) => Default (PParams era)

deriving newtype instance 
  Show (PParamsHKD Identity era) => Show (PParams era)

deriving newtype instance 
  Ord (PParamsHKD Identity era) => Ord (PParams era)

deriving newtype instance 
  NFData (PParamsHKD Identity era) => NFData (PParams era)

deriving newtype instance 
  (Era era, FromCBOR (PParamsHKD Identity era)) => FromCBOR (PParams era)

deriving newtype instance 
  (Era era, ToCBOR (PParamsHKD Identity era)) => ToCBOR (PParams era)

deriving newtype instance 
  (Era era, NoThunks (PParamsHKD Identity era)) => NoThunks (PParams era)

-- | The type of updates to Protocol parameters
newtype PParamsUpdate era = PParamsUpdate (PParamsHKD StrictMaybe era)

deriving newtype instance 
  Eq (PParamsHKD StrictMaybe era) => Eq (PParamsUpdate era)

deriving newtype instance 
  Default (PParamsHKD StrictMaybe era) => Default (PParamsUpdate era)

deriving newtype instance 
  Show (PParamsHKD StrictMaybe era) => Show (PParamsUpdate era)

deriving newtype instance 
  Ord (PParamsHKD StrictMaybe era) => Ord (PParamsUpdate era)

deriving newtype instance 
  NFData (PParamsHKD StrictMaybe era) => NFData (PParamsUpdate era)

deriving newtype instance 
  (FromCBOR (PParamsHKD StrictMaybe era), Era era) => FromCBOR (PParamsUpdate era)

deriving newtype instance 
  (ToCBOR (PParamsHKD StrictMaybe era), Era era) => ToCBOR (PParamsUpdate era)

deriving newtype instance 
  (NoThunks (PParamsHKD StrictMaybe era), Era era) => NoThunks (PParamsUpdate era)

class
  ( Era era,
    Eq (PParams era),
    Show (PParams era),
    NFData (PParams era),
    ToCBOR (PParams era),
    FromCBOR (PParams era),
    NoThunks (PParams era),
    Ord (PParamsUpdate era),
    Show (PParamsUpdate era),
    NFData (PParamsUpdate era),
    ToCBOR (PParamsUpdate era),
    FromCBOR (PParamsUpdate era),
    NoThunks (PParamsUpdate era)
  ) =>
  EraPParams era
  where
  -- | Protocol parameters where the fields are represented with a HKD
  type PParamsHKD (f :: Type -> Type) era = (r :: Type) | r -> era

  -- | Applies a protocol parameters update
  applyPPUpdates :: PParams era -> PParamsUpdate era -> PParams era

  emptyPParams :: PParams era
  emptyPParamsUpdate :: PParamsUpdate era

  -- HKD Versions of lenses

  -- | The linear factor for the minimum fee calculation
  hkdMinFeeAL :: Lens' (PParamsHKD f era) (HKD f Natural)

  -- | The constant factor for the minimum fee calculation
  hkdMinFeeBL :: Lens' (PParamsHKD f era) (HKD f Natural)

  -- | Maximal block body size
  hkdMaxBBSizeL :: Lens' (PParamsHKD f era) (HKD f Natural)

  -- | Maximal transaction size
  hkdMaxTxSizeL :: Lens' (PParamsHKD f era) (HKD f Natural)

  -- | Maximal block header size
  hkdMaxBHSizeL :: Lens' (PParamsHKD f era) (HKD f Natural)

  -- | The amount of a key registration deposit
  hkdKeyDepositL :: Lens' (PParamsHKD f era) (HKD f Coin)

  -- | The amount of a pool registration deposit
  hkdPoolDepositL :: Lens' (PParamsHKD f era) (HKD f Coin)

  -- | epoch bound on pool retirement
  hkdEMaxL :: Lens' (PParamsHKD f era) (HKD f EpochNo)

  -- | Desired number of pools
  hkdNOptL :: Lens' (PParamsHKD f era) (HKD f Natural)

  -- | Pool influence
  hkdA0L :: Lens' (PParamsHKD f era) (HKD f NonNegativeInterval)

  -- | Monetary expansion
  hkdRhoL :: Lens' (PParamsHKD f era) (HKD f UnitInterval)

  -- | Treasury expansion
  hkdTauL :: Lens' (PParamsHKD f era) (HKD f UnitInterval)

  -- | Decentralization parameter
  hkdDL :: Lens' (PParamsHKD f era) (HKD f UnitInterval)

  -- | Extra entropy
  hkdExtraEntropyL :: Lens' (PParamsHKD f era) (HKD f Nonce)

  -- | Protocol version
  hkdProtocolVersionL :: Lens' (PParamsHKD f era) (HKD f BT.ProtVer)

  -- | Minimum UTxO value
  hkdMinUTxOValueL :: Lens' (PParamsHKD f era) (HKD f Coin)

  -- | Minimum Stake Pool Cost
  hkdMinPoolCostL :: Lens' (PParamsHKD f era) (HKD f Coin)

ppLens :: Lens' (PParams era) (PParamsHKD Identity era)
ppLens = lens (\(PParams x) -> x) (\_ pp -> PParams pp)

ppuLens :: Lens' (PParamsUpdate era) (PParamsHKD StrictMaybe era)
ppuLens = lens (\(PParamsUpdate x) -> x) (\_ pp -> PParamsUpdate pp)

-- PParams versions of lenses

-- | The linear factor for the minimum fee calculation
ppMinFeeAL :: forall era. EraPParams era => Lens' (PParams era) Natural
ppMinFeeAL = ppLens . hkdMinFeeAL @era @Identity

-- | The constant factor for the minimum fee calculation
ppMinFeeBL :: forall era. EraPParams era => Lens' (PParams era) Natural
ppMinFeeBL = ppLens . hkdMinFeeBL @era @Identity

-- | Maximal block body size
ppMaxBBSizeL :: forall era. EraPParams era => Lens' (PParams era) Natural
ppMaxBBSizeL = ppLens . hkdMaxBBSizeL @era @Identity

-- | Maximal transaction size
ppMaxTxSizeL :: forall era. EraPParams era => Lens' (PParams era) Natural
ppMaxTxSizeL = ppLens . hkdMaxTxSizeL @era @Identity

-- | Maximal block header size
ppMaxBHSizeL :: forall era. EraPParams era => Lens' (PParams era) Natural
ppMaxBHSizeL = ppLens . hkdMaxBHSizeL @era @Identity

-- | The amount of a key registration deposit
ppKeyDepositL :: forall era. EraPParams era => Lens' (PParams era) Coin
ppKeyDepositL = ppLens . hkdKeyDepositL @era @Identity

-- | The amount of a pool registration deposit
ppPoolDepositL :: forall era. EraPParams era => Lens' (PParams era) Coin
ppPoolDepositL = ppLens . hkdPoolDepositL @era @Identity

-- | epoch bound on pool retirement
ppEMaxL :: forall era. EraPParams era => Lens' (PParams era) EpochNo
ppEMaxL = ppLens . hkdEMaxL @era @Identity

-- | Desired number of pools
ppNOptL :: forall era. EraPParams era => Lens' (PParams era) Natural
ppNOptL = ppLens . hkdNOptL @era @Identity

-- | Pool influence
ppA0L :: forall era. EraPParams era => Lens' (PParams era) NonNegativeInterval
ppA0L = ppLens . hkdA0L @era @Identity

-- | Monetary expansion
ppRhoL :: forall era. EraPParams era => Lens' (PParams era) UnitInterval
ppRhoL = ppLens . hkdRhoL @era @Identity

-- | Treasury expansion
ppTauL :: forall era. EraPParams era => Lens' (PParams era) UnitInterval
ppTauL = ppLens . hkdTauL @era @Identity

-- | Decentralization parameter
ppDL :: forall era. EraPParams era => Lens' (PParams era) UnitInterval
ppDL = ppLens . hkdDL @era @Identity

-- | Extra entropy
ppExtraEntropyL :: forall era. EraPParams era => Lens' (PParams era) Nonce
ppExtraEntropyL = ppLens . hkdExtraEntropyL @era @Identity

-- | Protocol version
ppProtocolVersionL :: forall era. EraPParams era => Lens' (PParams era) BT.ProtVer
ppProtocolVersionL = ppLens . hkdProtocolVersionL @era @Identity

-- | Minimum UTxO value
ppMinUTxOValueL :: forall era. EraPParams era => Lens' (PParams era) Coin
ppMinUTxOValueL = ppLens . hkdMinUTxOValueL @era @Identity

-- | Minimum Stake Pool Cost
ppMinPoolCostL :: forall era. EraPParams era => Lens' (PParams era) Coin
ppMinPoolCostL = ppLens . hkdMinPoolCostL @era @Identity

-- PParamsUpdate versions of lenses

-- | The linear factor for the minimum fee calculation
ppuMinFeeAL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuMinFeeAL = ppuLens . hkdMinFeeAL @era @StrictMaybe

-- | The constant factor for the minimum fee calculation
ppuMinFeeBL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuMinFeeBL = ppuLens . hkdMinFeeBL @era @StrictMaybe

-- | Maximal block body size
ppuMaxBBSizeL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuMaxBBSizeL = ppuLens . hkdMaxBBSizeL @era @StrictMaybe

-- | Maximal transaction size
ppuMaxTxSizeL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuMaxTxSizeL = ppuLens . hkdMaxTxSizeL @era @StrictMaybe

-- | Maximal block header size
ppuMaxBHSizeL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuMaxBHSizeL = ppuLens . hkdMaxBHSizeL @era @StrictMaybe

-- | The amount of a key registration deposit
ppuKeyDepositL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuKeyDepositL = ppuLens . hkdKeyDepositL @era @StrictMaybe

-- | The amount of a pool registration deposit
ppuPoolDepositL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuPoolDepositL = ppuLens . hkdPoolDepositL @era @StrictMaybe

-- | epoch bound on pool retirement
ppuEMaxL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe EpochNo)
ppuEMaxL = ppuLens . hkdEMaxL @era @StrictMaybe

-- | Desired number of pools
ppuNOptL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Natural)
ppuNOptL = ppuLens . hkdNOptL @era @StrictMaybe

-- | Pool influence
ppuA0L :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe NonNegativeInterval)
ppuA0L = ppuLens . hkdA0L @era @StrictMaybe

-- | Monetary expansion
ppuRhoL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe UnitInterval)
ppuRhoL = ppuLens . hkdRhoL @era @StrictMaybe

-- | Treasury expansion
ppuTauL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe UnitInterval)
ppuTauL = ppuLens . hkdTauL @era @StrictMaybe

-- | Decentralization parameter
ppuDL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe UnitInterval)
ppuDL = ppuLens . hkdDL @era @StrictMaybe

-- | Extra entropy
ppuExtraEntropyL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Nonce)
ppuExtraEntropyL = ppuLens . hkdExtraEntropyL @era @StrictMaybe

-- | Protocol version
ppuProtocolVersionL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe BT.ProtVer)
ppuProtocolVersionL = ppuLens . hkdProtocolVersionL @era @StrictMaybe

-- | Minimum UTxO value
ppuMinUTxOValueL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuMinUTxOValueL = ppuLens . hkdMinUTxOValueL @era @StrictMaybe

-- | Minimum Stake Pool Cost
ppuMinPoolCostL :: forall era. EraPParams era => Lens' (PParamsUpdate era) (StrictMaybe Coin)
ppuMinPoolCostL = ppuLens . hkdMinPoolCostL @era @StrictMaybe

type PParamsDelta era = PParamsUpdate era

{-# DEPRECATED PParamsDelta "Use `PParamsUpdate` instead" #-}
