{-# LANGUAGE TemplateHaskell     #-}
module Language.PlutusTx.Coordination.StateMachine.TH(
    StateMachine(..),
    stateMachine
    ) where

import           Language.Haskell.TH             (Q, TExp)
import qualified Language.PlutusTx.Prelude       as Prelude
import qualified Language.PlutusTx.Builtins      as Builtins
import qualified Language.PlutusTx.Validation.TH as TH
import           Ledger.Validation    (PendingTx(..), ValidatorHash, DataScriptHash(..), RedeemerHash(..))
import           Prelude                         hiding ((&&), and)

data StateMachine s i = StateMachine {
    step :: Q (TExp (PendingTx (ValidatorHash, DataScriptHash, RedeemerHash) -> s -> i -> s)),
    eqState :: Q (TExp (s -> s -> Bool))
    }

-- | A function that checks if the new state matches what is expected, and 
-- verifies that the hash of the current redeemer matches that of the new data 
-- script.
stateMachine :: 
    StateMachine s i
    -> Q (TExp ((s, i) -> (s, i) -> PendingTx (ValidatorHash, DataScriptHash, RedeemerHash) -> Bool))
stateMachine s = [|| \(currentState, _) (newState, action) p ->

    let
        infixr 3 &&
        (&&) :: Bool -> Bool -> Bool
        (&&) = $$(Prelude.and)

        actualState = $$(step s) p currentState action
        statesMatch = $$(eqState s) actualState newState
        
        -- check that our redeemer script hash is the same as the data script hash of the first tx output

        PendingTx _ (o1:_) _ _ _ _ (_, _, RedeemerHash h) = p

        hashesMatch = case $$(TH.scriptOutput) o1 of
            Nothing -> False -- o1 is a pay-to-pubkey output
            Just (_, DataScriptHash h2) -> Builtins.equalsByteString h h2

    in
        statesMatch && hashesMatch ||]