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
  ( PubKeyHash (..)
  , Address (..)
  )
import qualified PlutusTx.AssocMap as M
import           PlutusTx.AssocMap (Map)
import           Plutus.V1.Ledger.Value as V
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

validatePaymentMap :: Map PubKeyHash Value -> Bool
validatePaymentMap = error ()

findToken :: Value -> CurrencySymbol -> Map TokenName Integer
findToken = error ()

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
genZeroPolicyId :: CurrencySymbol
genZeroPolicyId = "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2"

extractPkh :: BuiltinByteString -> BuiltinByteString
extractPkh = sliceByteString 0 28

tokenNameToPkh :: TokenName -> PubKeyHash
tokenNameToPkh (TokenName bs) = PubKeyHash $ extractPkh bs

tokenNameToGen :: TokenName -> Integer
tokenNameToGen (TokenName bs) = indexByteString bs 28

isGenZero :: TokenName -> Bool
isGenZero = (==0) . tokenNameToGen

gen0ToGen1TokenName :: TokenName -> TokenName
gen0ToGen1TokenName (TokenName bs) = TokenName $ extractPkh bs <> consByteString 1 emptyByteString

toExpectedGen1Tokens
  :: CurrencySymbol
  -> Map TokenName Integer
  -> Map PubKeyHash Value
toExpectedGen1Tokens currencySym genZeros =
  let
    pkhsAndAssets :: [Map PubKeyHash Value]
    pkhsAndAssets
      = map (\(t, c) -> M.singleton (tokenNameToPkh t)
              $ V.singleton currencySym (gen0ToGen1TokenName t) c
            )
      $ M.toList genZeros

  in mergeAll pkhsAndAssets

genZeroTokensToBurnAndMint
  :: CurrencySymbol
  -> Map TokenName Integer
  -> Value
genZeroTokensToBurnAndMint c genZeros =
  let
    tokensToBurn = Value $ M.singleton c $ fmap negate genZeros
    tokensToMint
      = Value
      $ M.singleton c
      $ M.fromList
      $ map (\(t, i) -> (gen0ToGen1TokenName t, i))
      $ M.toList genZeros
  in tokensToBurn <> tokensToMint

{-# INLINABLE exchangeValidator #-}
exchangeValidator :: CurrencySymbol -> BuiltinData -> ScriptContext -> Bool
exchangeValidator genZero _ ctx =
  let
    info :: TxInfo
    !info = scriptContextTxInfo ctx

    inputValue :: Value
    !inputValue = valueSpent info

    genZeroTokens :: Map TokenName Integer
    !genZeroTokens
      = M.fromList
      $ filter (isGenZero . fst)
      $ M.toList
      $ findToken inputValue genZero

    expectedGenOneTokens :: Map PubKeyHash Value
    !expectedGenOneTokens = toExpectedGen1Tokens genZero genZeroTokens

    expectedMintValue :: Value
    !expectedMintValue = genZeroTokensToBurnAndMint genZero genZeroTokens

  in traceIfFalse "Gen 1 tokens not outputted!" (validatePaymentMap expectedGenOneTokens)
  && traceIfFalse "Incorrect values minted!" (expectedMintValue == txInfoMint info)

-------------------------------------------------------------------------------
-- Entry Points
-------------------------------------------------------------------------------

validator :: CurrencySymbol -> MintingPolicy
validator genZero =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \x -> wrapMintingPolicy $ exchangeValidator x ||])
    `applyCode`
     liftCode genZero

iamxExchange :: PlutusScript PlutusScriptV1
iamxExchange
  = PlutusScriptSerialised
  $ SBS.toShort
  $ LB.toStrict
  $ serialise
  $ Validator
  $ unMintingPolicyScript
  $ validator genZeroPolicyId
