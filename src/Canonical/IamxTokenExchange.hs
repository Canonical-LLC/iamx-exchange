module Canonical.IamxTokenExchange
    ( Asset (..)
    , Payout (..)
    , iamxExchange
    ) where

import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Codec.Serialise          ( serialise )
import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           PlutusTx
import           PlutusTx.Prelude
import           Ledger
  ( PubKeyHash
  , Address (..)
  )
import qualified PlutusTx.AssocMap as M
import           PlutusTx.AssocMap (Map)
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Contexts
import           Plutus.V1.Ledger.Scripts
import           Ledger.Typed.Scripts
import           Plutus.V1.Ledger.Credential

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Asset = Asset
  { aCurrencySymbol :: !CurrencySymbol
  , aTokenName      :: !TokenName
  }

data Payout = Payout
  { pAddress :: !PubKeyHash
  , pValue   :: !Value
  }

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------
{-# INLINABLE isScriptAddress #-}
isScriptAddress :: Address -> Bool
isScriptAddress Address { addressCredential } = case addressCredential of
  ScriptCredential _ -> True
  _ -> False

-- Verify that there is only one script input and get it's value.
{-# INLINABLE getOnlyScriptInput #-}
getOnlyScriptInput :: TxInfo -> Value
getOnlyScriptInput info =
  let
    isScriptInput :: TxInInfo -> Bool
    isScriptInput = isScriptAddress . txOutAddress . txInInfoResolved

    input = case filter isScriptInput . txInfoInputs $ info of
      [i] -> i
      _ -> traceError "expected exactly one script input"

  in txOutValue . txInInfoResolved $ input

{-# INLINABLE payoutToInequality #-}
payoutToInequality :: Payout -> (PubKeyHash, Value)
payoutToInequality Payout {..} = (pAddress, pValue)

{-# INLINABLE mergePayoutsValue #-}
mergePayoutsValue :: [Payout] -> Value
mergePayoutsValue = foldr (\x acc -> pValue x <> acc) mempty

{-# INLINABLE paidAtleastTo #-}
paidAtleastTo :: TxInfo -> PubKeyHash -> Value -> Bool
paidAtleastTo info pkh val = valuePaidTo info pkh `geq` val

{-# INLINABLE payoutPaid #-}
payoutPaid :: TxInfo -> Payout -> Bool
payoutPaid info Payout {..} = valuePaidTo info pAddress `geq` pValue

{-# INLINABLE mergeInequalities #-}
mergeInequalities
  :: Map PubKeyHash Value
  -> Map PubKeyHash Value
  -> Map PubKeyHash Value
mergeInequalities = M.unionWith (+)

{-# INLINABLE mergeAll #-}
mergeAll :: [Map PubKeyHash Value] -> Map PubKeyHash Value
mergeAll = foldr mergeInequalities M.empty

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------
instance Eq Asset where
  x == y
    =  aCurrencySymbol x == aCurrencySymbol y
    && aTokenName      x == aTokenName      y

instance Eq Payout where
  x == y
    =  pAddress x == pAddress y
    && pValue x == pValue y

PlutusTx.unstableMakeIsData ''Asset
PlutusTx.unstableMakeIsData ''Payout

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------

{-# INLINABLE exchangeValidator #-}
exchangeValidator ::  BuiltinData -> ScriptContext -> Bool
exchangeValidator _ _ = True

-------------------------------------------------------------------------------
-- Entry Points
-------------------------------------------------------------------------------

validator :: MintingPolicy
validator =
  mkMintingPolicyScript
    $$(PlutusTx.compile [|| wrapMintingPolicy exchangeValidator ||])

iamxExchange :: PlutusScript PlutusScriptV1
iamxExchange
  = PlutusScriptSerialised
  $ SBS.toShort
  $ LB.toStrict
  $ serialise
  $ Validator
  $ unMintingPolicyScript validator
