{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS -fplugin=Language.PlutusTx.Plugin -fplugin-opt Language.PlutusTx.Plugin:dont-typecheck #-}
module Language.PlutusTx.Coordination.Contracts.Swap(
    enterFixed,
    enterFloating
    ) where

import qualified Language.PlutusTx          as PlutusTx
import           Language.PlutusTx.Prelude  (Ratio)
import           Ledger                     (Validator (..), Value(..), PubKey)
import qualified Ledger                     as Ledger
import           Wallet.API                 (WalletAPI(..), pubKey)

import qualified Language.PlutusTx.Coordination.Contracts.Swap.TH0 as TH0
import           Language.PlutusTx.Coordination.Contracts.Swap.TH0 (Role(..), Spread(..))
import           Language.PlutusTx.Coordination.Contracts.Swap.TH
import           Language.PlutusTx.Coordination.StateMachine.TH

initialDataScript :: 
    (Monad m, WalletAPI m)
    => SwapParams 
    -> Role 
    -> PubKey 
    -> m (SwapState, SwapAction)
initialDataScript SwapParams{..} role other = do
    own <- pubKey <$> myKeyPair
    let owners = case role of
            Fixed -> SwapOwners own other
            Floating -> SwapOwners other own
        margins = SwapMarginAccounts swapMarginPenalty swapMarginPenalty
    pure (Ongoing owners margins, NoAction)

-- | Enter into an fx swap assuming the "fixed" role
enterFixed :: 
    WalletAPI m
    => SwapParams 
    -> SwapOwners 
    -> m ()
enterFixed = undefined

-- | Enter into an fx swap assuming the "floating" role
enterFloating :: (Applicative m, WalletAPI m) => SwapParams -> SwapOwners -> m ()
enterFloating SwapParams{..} _ = pure ()

-- | The current minimum margin
currentMargin :: Role -> SwapParams -> (Ratio Int) -> Value
currentMargin r swp fll =
    let s = Spread {
                fixedRate = swapFixedRate swp,
                floatingRate = fll,
                amount = swapNotionalAmount swp,
                penalty = swapMarginPenalty swp
            }
    in $$(TH0.margin) r s

address :: SwapParams -> Ledger.Address'
address = Ledger.scriptAddress . validator

validator :: SwapParams -> Validator
validator swp = Validator val where
    val = Ledger.applyScript inner (Ledger.lifted swp)

    --   See note [Contracts and Validator Scripts] in
    --       Language.Plutus.Coordination.Contracts
    inner = Ledger.fromPlcCode $$(PlutusTx.plutus ([|| 
            \sp (dataScript :: (SwapState, SwapAction)) (redeemerScript :: (SwapState, SwapAction))  ptx ->
                let m = $$(stateMachine (StateMachine {
                                            step       = swapStep,
                                            eqState    = swapStateEq,
                                            finalState = swapStateFinished
                                            })) sp
                in 
                    if m dataScript redeemerScript ptx
                    then ()
                    else $$(PlutusTx.traceH) "State machine invalid step" (PlutusTx.error ())
            ||]))

{- Note [Swap Transactions]

The swap involves three transactions at two different times.

1. At t=0. Each participant deposits the margin. The outputs are locked with
   the same validator script, `swapValidator`
2. At t=n. The value of the floating rate, and consequently the values of the
   two payments are determined. Each participant gets their margin plus or
   minus the actual payment.

There is a risk of losing out if the interest rate moves outside the range of
fixedRate +/- (margin / notional amount). In a real financial contract this
would be dealt with by agreeing a "Variation Margin". This means that the
margin is adjusted at predefined dates before the actual payment is due. If one
of the parties fails to make the variation margin payment, the contract ends
prematurely and the other party gets to keep both margins.

Plutus should be able to handle variation margins in a series of validation
scripts. But it seems to me that they could get quite messy so I don't want to
write them by hand :) We can probably use TH to generate them at compile time.

-}
