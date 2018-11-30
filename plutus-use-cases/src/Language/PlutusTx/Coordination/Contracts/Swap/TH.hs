{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.PlutusTx.Coordination.Contracts.Swap.TH(
    -- * Types
    SwapParams(..),
    SwapOwners(..),
    SwapMarginAccounts(..),
    SwapAction(..),
    SwapState(..),
    -- * Pieces of the state machine
    swapStep,
    swapStateEq,
    swapStateFinished
    ) where

import qualified Language.PlutusTx.Prelude                         as TH
import           Language.PlutusTx.Prelude                        (Ratio)
import qualified Language.PlutusTx                                as PlutusTx 
import qualified Language.PlutusTx.Validation                     as TH
import           Language.Haskell.TH                              (Q, TExp)
import           Ledger                                           (PubKey, Value(..))
import           Ledger.Validation                                (PendingTx', Height(..), PendingTx(..), PendingTxIn(..), PendingTxOut(..), OracleValue)
import qualified Ledger.Validation                                as TH

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
    swapMargin           :: Value,
    swapOracle           :: PubKey
    }

PlutusTx.makeLift ''SwapParams

data SwapAction = 
    Exchange (OracleValue (Ratio Int))
    | AdjustMarginFixedLeg
    | AdjustMarginFloatingLeg
    | ChangeOwnerFixedLeg PubKey
    | ChangeOwnerFloatingLeg PubKey

PlutusTx.makeLift ''SwapAction

swapStep :: Q (TExp (SwapParams -> PendingTx' -> SwapState -> SwapAction -> SwapState))
swapStep = [|| \swp p st action -> 
    let
        SwapParams (Value amt) obsTimes fixedRate (Value margin) oraclePk = swp

        PendingTx _ _ _ _ currentHeight _ _ = p

        (SwapOwners fx fl, SwapMarginAccounts (Value fixedMargin) (Value floatingMargin)) = case st of
            Ongoing so ma -> (so, ma)
            Settled -> PlutusTx.error () 
        
        infixr 3 &&
        (&&) :: Bool -> Bool -> Bool
        (&&) = $$(TH.and)

        totalValIn = $$(TH.foldr) (\(PendingTxIn _ _ (Value v')) v -> v + v') 0 ($$(TH.inputsOwnAddress) p)

        totalValOut = $$(TH.foldr) (\(PendingTxOut (Value v') _ _) v -> v + v') 0 ($$(TH.outputsOwnAddress) p)

        -- The last payment covered by the swap. 
        lastPaymentDate = $$(TH.foldr) (\(Height h') h -> $$(TH.max) h' h) 0 obsTimes

        -- Whether a payment is scheduled for the current block
        canExchangeNow = $$(TH.any) ($$(TH.eqHeight) currentHeight) obsTimes

    in

    case action of
        Exchange ov -> 
            let                    
                rt = $$(TH.extractVerifyAt) ov oraclePk currentHeight
                
                -- difference between the two rates
                rtDiff :: Ratio Int
                rtDiff = $$(TH.minusR) rt fixedRate

                amt' :: Ratio Int
                amt' = $$(TH.fromIntR) amt

                -- amount of money that changes hands in this exchange.
                delta :: Int
                delta = $$(TH.roundR) ($$(TH.timesR) amt' rtDiff)

                clamp :: Int -> Int -> Int -> Int
                clamp low high x = $$(TH.max) low ($$(TH.min) high x)

                fixedMargin' = clamp 0 fixedMargin (fixedMargin + delta)
                floatingMargin' = clamp 0 floatingMargin (floatingMargin - delta)

                isLast = let Height c = currentHeight in c == lastPaymentDate

            in
                if canExchangeNow
                then 
                    if isLast 
                    then Settled 
                    else Ongoing 
                            (SwapOwners fx fl) 
                            (SwapMarginAccounts (Value fixedMargin') (Value floatingMargin'))

                else $$(PlutusTx.traceH) "Cannot exchange payments now" (PlutusTx.error ())
                

        AdjustMarginFixedLeg -> 
            let 
                deltaMargin = totalValOut - totalValIn
                fixedMargin' = fixedMargin + deltaMargin
            in
                if deltaMargin > 0 && totalValOut == fixedMargin' + floatingMargin
                then Ongoing 
                        (SwapOwners fx fl) 
                        (SwapMarginAccounts (Value fixedMargin') (Value floatingMargin))

                else $$(PlutusTx.traceH) "AdjustMarginFixedLeg invalid" (PlutusTx.error ())

        AdjustMarginFloatingLeg -> 
            let 
                deltaMargin = totalValOut - totalValIn
                floatingMargin' = floatingMargin + deltaMargin
            in
                if deltaMargin > 0 && totalValOut == fixedMargin + floatingMargin'
                then Ongoing 
                        (SwapOwners fx fl) 
                        (SwapMarginAccounts (Value fixedMargin) (Value floatingMargin'))

                else $$(PlutusTx.traceH) "AdjustMarginFixedLeg invalid" (PlutusTx.error ())

        ChangeOwnerFixedLeg fx' -> 
            -- to change the owner of the fixed leg of the 
            -- swap, 
            -- 1. the txn needs to be signed by the current owner
            -- 2. the new owner should be reflected in the new state
            let 
                signedByOwner = $$(TH.txSignedBy) p fx
            in
                if signedByOwner && totalValOut == fixedMargin + floatingMargin
                then Ongoing 
                        (SwapOwners fx' fl) 
                        (SwapMarginAccounts (Value fixedMargin) (Value floatingMargin))

                else $$(PlutusTx.traceH) "ChangeOwnerFixedLeg invalid" (PlutusTx.error ())
                
        ChangeOwnerFloatingLeg fl' -> 
            -- to change the owner of the floating leg of the 
            -- swap, 
            -- 1. the txn needs to be signed by the current owner
            -- 2. the new owner should be reflected in the new state
            let 
                signedByOwner = $$(TH.txSignedBy) p fl
            in
                if signedByOwner && totalValOut == fixedMargin + floatingMargin
                then Ongoing 
                        (SwapOwners fx fl') 
                        (SwapMarginAccounts (Value fixedMargin) (Value floatingMargin))

                else $$(PlutusTx.traceH) "ChangeOwnerFloatingLeg invalid" (PlutusTx.error ())
                
    ||]

swapStateEq :: Q (TExp (SwapState -> SwapState -> Bool))
swapStateEq = [|| \l r -> 
        case (l, r) of
            (Settled, Settled) -> True

            (Ongoing (SwapOwners fxl fll) (SwapMarginAccounts (Value fxmL) (Value flmL)), Ongoing (SwapOwners fxr flr) (SwapMarginAccounts (Value fxmR) (Value flmR))) ->
                let and_ = $$(TH.and) in
                ($$(TH.eqPubKey) fxl fll) `and_` ($$(TH.eqPubKey) fxr flr) `and_` (fxmL == fxmR) `and_` (flmL == flmR)

            _ -> False
                
     ||]

swapStateFinished :: Q (TExp (SwapState -> Bool))
swapStateFinished = [|| \case Settled -> True; _ -> False ||]