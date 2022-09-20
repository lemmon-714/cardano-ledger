{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.Newpp
  ( ShelleyNEWPP,
    ShelleyNewppState (..),
    NewppEnv (..),
    ShelleyNewppPredFailure (..),
    PredicateFailure,
  )
where

import Cardano.Ledger.BaseTypes (ShelleyBase, StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.EpochBoundary (obligation)
import Cardano.Ledger.Shelley.Era (ShelleyNEWPP)
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState,
    DState (..),
    PPUPState (..),
    PState (..),
    UTxOState,
    availableAfterMIR,
    pvCanFollow,
    _deposited,
    _irwd,
  )
import Cardano.Ledger.Shelley.PParams
  ( ProposedPPUpdates (..),
    emptyPPPUpdates, ShelleyPParamsHKD (..)
  )
import Cardano.Ledger.Shelley.TxBody (MIRPot (..))
import Control.State.Transition
  ( STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    (?!),
  )
import Data.Default.Class (Default, def)
import Data.Typeable (Typeable)
import Data.UMap (rewView)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Lens.Micro ((^.))

data ShelleyNewppState era
  = NewppState (PParams era) (PPUPState era)

data NewppEnv era
  = NewppEnv
      (DState (EraCrypto era))
      (PState (EraCrypto era))
      (UTxOState era)
      AccountState

data ShelleyNewppPredFailure era
  = UnexpectedDepositPot
      !Coin -- The total outstanding deposits
      !Coin -- The deposit pot
  deriving (Show, Eq, Generic)

instance NoThunks (ShelleyNewppPredFailure era)

instance
  ( Default (PParams era),
    EraPParams era,
    Typeable era
  ) =>
  STS (ShelleyNEWPP era)
  where
  type State (ShelleyNEWPP era) = ShelleyNewppState era
  type Signal (ShelleyNEWPP era) = Maybe (PParams era)
  type Environment (ShelleyNEWPP era) = NewppEnv era
  type BaseM (ShelleyNEWPP era) = ShelleyBase
  type PredicateFailure (ShelleyNEWPP era) = ShelleyNewppPredFailure era
  transitionRules = [newPpTransition]

instance Default (PParams era) => Default (ShelleyNewppState era) where
  def = NewppState def def

newPpTransition ::
  forall era.
  EraPParams era =>
  TransitionRule (ShelleyNEWPP era)
newPpTransition = do
  TRC
    ( NewppEnv dstate pstate utxoSt acnt,
      NewppState pp ppupSt,
      ppNew
      ) <-
    judgmentContext

  case ppNew of
    Just ppNew' -> do
      let Coin oblgCurr = obligation pp (rewView (_unified dstate)) (_pParams pstate)
          Coin oblgNew = obligation ppNew' (rewView (_unified dstate)) (_pParams pstate)
          diff = oblgCurr - oblgNew
          Coin availableReserves = availableAfterMIR ReservesMIR acnt (_irwd dstate)

      Coin oblgCurr == _deposited utxoSt
        ?! UnexpectedDepositPot (Coin oblgCurr) (_deposited utxoSt)

      if availableReserves + diff >= 0
        -- Note that instantaneous rewards from the treasury are irrelevant
        -- here, since changes in the protocol parameters do not change how much
        -- is needed from the treasury
        && (ppNew' ^. maxTxSizeL + ppNew' ^. maxBHSizeL)
          < ppNew' ^. maxBBSizeL
        then pure $ NewppState ppNew' (updatePpup ppupSt ppNew')
        else pure $ NewppState pp (updatePpup ppupSt pp)
    Nothing -> pure $ NewppState pp (updatePpup ppupSt pp)

-- | Update the protocol parameter updates by clearing out the proposals
-- and making the future proposals become the new proposals,
-- provided the new proposals can follow (otherwise reset them).
updatePpup ::
  forall era.
  EraPParams era =>
  PPUPState era ->
  PParams era ->
  PPUPState era
updatePpup ppupSt pp = PPUPState ps emptyPPPUpdates
  where
    ProposedPPUpdates newProposals = futureProposals ppupSt
    goodPV ppu =
      pvCanFollow (pp ^. protocolVersionL) $ protocolVersion @StrictMaybe @era ppu
    ps =
      if all goodPV newProposals
        then ProposedPPUpdates newProposals
        else emptyPPPUpdates
