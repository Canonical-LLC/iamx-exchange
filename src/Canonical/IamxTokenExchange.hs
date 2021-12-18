module Canonical.IamxTokenExchange
    ( iamxExchange
    ) where

import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Codec.Serialise          ( serialise )
import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           PlutusTx
import           PlutusTx.Prelude
import           Ledger
  ( PubKeyHash (..)
  )
import           Plutus.V1.Ledger.Value as V
import           Plutus.V1.Ledger.Contexts
import           Plutus.V1.Ledger.Scripts
import           Ledger.Typed.Scripts
import           Canonical.IamxTokenExchange.Types

{-# INLINABLE exchangeValidator #-}
exchangeValidator :: Config -> Action -> ScriptContext -> Bool
exchangeValidator Config {..} redeemer ctx =
  let
    info :: TxInfo
    !info = scriptContextTxInfo ctx

    inputValue :: Value
    !inputValue = valueSpent info

    iamxCurrencySymbol :: CurrencySymbol
    iamxCurrencySymbol = ownCurrencySymbol ctx

    mkIamxTokenValue :: Integer -> Value
    mkIamxTokenValue = singleton iamxCurrencySymbol iamxTokenName

    iamxTokenCount :: Value -> Integer
    iamxTokenCount v = valueOf v iamxCurrencySymbol iamxTokenName

    outputDiff :: Integer
    outputDiff = iamxTokenCount (valueProduced info) - iamxTokenCount inputValue

    validRewardAndMinting :: PubKeyHash -> Integer -> Bool
    validRewardAndMinting pkh rewardAmount
      =  iamxTokenCount (valuePaidTo info pkh) == rewardAmount
      && iamxTokenCount (txInfoMint info) == (rewardAmount - 1)
      && outputDiff == rewardAmount

  in traceIfFalse "Not signed by IAMX" (info `txSignedBy` iamxWallet)
  && case redeemer of
      InitialMint
        -> traceIfFalse "Invalid inital mint amount!"
          (txInfoMint info == mkIamxTokenValue 33_000_000_000)
        && traceIfFalse "Invalid inital utxo!"
          (any (\i -> txInInfoOutRef i == initialUtxo) $ txInfoInputs info)
      Burn
        -> traceIfFalse "Wrong number of IAMX tokens" (iamxTokenCount inputValue >= 1)
        && traceIfFalse "Invalid burn amount!"
            (txInfoMint info == mkIamxTokenValue (-1))
      Keep
        -> traceIfFalse "Wrong number of IAMX tokens" (iamxTokenCount inputValue >= 1)
        && traceIfFalse "Token not transfered to IAMX wallet!"
            (valuePaidTo info iamxWallet == mkIamxTokenValue 1)
      Exchange {..}
        -> traceIfFalse "Wrong number of IAMX tokens" (iamxTokenCount inputValue >= 1)
        && traceIfFalse "Invalid reward produced!" (case () of
        () -- Working around a bug where case statements desugar to
           -- equalities with Prelude.== instead of PlutusTx.Prelude.==
          | generation == 0 -> validRewardAndMinting rewardAddress 1
          | generation == 1 -> validRewardAndMinting rewardAddress 2
          | generation == 2 -> validRewardAndMinting rewardAddress 3
          | generation == 3 -> validRewardAndMinting rewardAddress 5
          | otherwise -> traceError "Invalid generation! Expected 0 - 3"
        )

-------------------------------------------------------------------------------
-- Entry Points
-------------------------------------------------------------------------------

validator :: Config -> MintingPolicy
validator config =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \x -> wrapMintingPolicy $ exchangeValidator x ||])
    `applyCode`
     liftCode config

iamxExchange :: Config -> PlutusScript PlutusScriptV1
iamxExchange config
  = PlutusScriptSerialised
  $ SBS.toShort
  $ LB.toStrict
  $ serialise
  $ Validator
  $ unMintingPolicyScript
  $ validator config
