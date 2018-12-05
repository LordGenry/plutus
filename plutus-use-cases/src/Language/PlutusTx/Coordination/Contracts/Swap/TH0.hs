{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.PlutusTx.Coordination.Contracts.Swap.TH0(
    SwapOwners(..),
    SwapMarginAccounts(..),
    SwapParams(..),
    SwapAction(..),
    SwapState(..),
    Role(..),
    Spread(..),
    margin
    ) where
  
import qualified Language.PlutusTx                                as PlutusTx 
import qualified Language.PlutusTx.Prelude                        as P
import           Language.PlutusTx.Prelude                        (Ratio)
import           Language.Haskell.TH                              (Q, TExp)
import           Ledger                                           (PubKey, Value(..))
import           Ledger.Validation                                (Height, OracleValue)

data SwapOwners = SwapOwners {
  swapOwnersFixedLeg :: PubKey,
  swapOwnersFloating :: PubKey
  }

PlutusTx.makeLift ''SwapOwners

data SwapMarginAccounts = SwapMarginAccounts {
  marginAccFixed :: Value,
  marginAccFloating :: Value
  }

PlutusTx.makeLift ''SwapMarginAccounts

data SwapState = Ongoing SwapOwners SwapMarginAccounts | Settled

PlutusTx.makeLift ''SwapState

data SwapParams = SwapParams {
  swapNotionalAmount   :: Value,
  swapObservationTimes :: [Height],
  swapFixedRate        :: Ratio Int,
  swapMarginPenalty    :: Value,
  swapOracle           :: PubKey
  }

PlutusTx.makeLift ''SwapParams

data SwapAction = 
  Exchange (OracleValue (Ratio Int))
  | AdjustMarginFixedLeg (OracleValue (Ratio Int))
  | AdjustMarginFloatingLeg (OracleValue (Ratio Int))
  | ChangeOwnerFixedLeg PubKey
  | ChangeOwnerFloatingLeg PubKey
  | NoAction

PlutusTx.makeLift ''SwapAction

data Role = Fixed | Floating

PlutusTx.makeLift ''Role

data Spread = Spread {
    fixedRate    :: Ratio Int,
    floatingRate :: Ratio Int,
    amount       :: Value,
    penalty      :: Value
    }

PlutusTx.makeLift ''Spread

-- | The margin required for a spread between fixed and floating rate. This 
--   function will be used both in on-chain and off-chain code.
margin :: Q (TExp (Role -> Spread -> Value))
margin = [|| \rol (Spread fixed floating (Value amt) (Value pnlty)) ->
    let
        amt' :: Ratio Int
        amt' = $$(P.fromIntR) amt

        marginFx :: Int
        marginFx = $$(P.roundR) ($$(P.timesR) amt' ($$(P.minusR) floating fixed))

        marginFl :: Int
        marginFl = $$(P.roundR) ($$(P.timesR) amt' ($$(P.minusR) fixed floating))

    in
        case rol of
            Fixed -> 
                Value (pnlty + ($$(P.max) 0 marginFx))
            Floating -> 
                Value (pnlty + ($$(P.max) 0 marginFl))

      ||]