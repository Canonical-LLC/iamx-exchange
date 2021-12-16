{-# OPTIONS_GHC -fno-warn-orphans #-}
import Cardano.Api                         hiding (TxId)

import Canonical.IamxTokenExchange
import Canonical.IamxTokenExchange.Types
import Prelude
import Options.Generic
import Ledger
import Data.String

instance ParseField PubKeyHash where
  parseField w x y z = fromString <$> parseField w x y z
  readField = fromString <$> readField

instance ParseRecord PubKeyHash where
  parseRecord = fromOnly <$> parseRecord

instance ParseFields PubKeyHash

instance ParseField CurrencySymbol where
  parseField w x y z = fromString <$> parseField w x y z
  readField = fromString <$> readField

instance ParseRecord CurrencySymbol where
  parseRecord = fromOnly <$> parseRecord

instance ParseFields CurrencySymbol

instance ParseField TokenName where
  parseField w x y z = fromString <$> parseField w x y z
  readField = fromString <$> readField

instance ParseRecord TokenName where
  parseRecord = fromOnly <$> parseRecord

instance ParseFields TokenName

instance ParseRecord Config where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

data Opts = Opts
  { plutusCorePath :: FilePath
  , config         :: Config
  }

instance ParseRecord Opts where
  parseRecord
    =   Opts
    <$> parseField
          (Just "The plutus core file path")
          (Just "out-file")
          (Just 'o')
          (Just "scripts/iamx-exchange.plutus")
    <*> parseRecord

main :: IO ()
main = do
  Opts {..} <- getRecord "Create IAMX Exchange smart contract"

  writeFileTextEnvelope plutusCorePath Nothing (iamxExchange config) >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote NFT validator to file " ++ plutusCorePath
