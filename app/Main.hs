{-# OPTIONS_GHC -fno-warn-orphans #-}
import Cardano.Api                         hiding (TxId)

import Canonical.IamxTokenExchange
import Canonical.IamxTokenExchange.Types
import Prelude
import Options.Generic
import Ledger
import Data.String
import Plutus.V1.Ledger.Bytes

parseUTxO :: String -> TxOutRef
parseUTxO s =
  let
    (x, y) = span (/= '#') s
  in
    TxOutRef (TxId $ getLedgerBytes $ fromString x) $ read $ tail y

instance ParseField TxOutRef where
  parseField w x y z = parseUTxO <$> parseField w x y z
  readField = parseUTxO <$> readField

instance ParseRecord TxOutRef where
  parseRecord = fromOnly <$> parseRecord

instance ParseFields TxOutRef

instance ParseField PubKeyHash where
  parseField w x y z = fromString <$> parseField w x y z
  readField = fromString <$> readField

instance ParseRecord PubKeyHash where
  parseRecord = fromOnly <$> parseRecord

instance ParseFields PubKeyHash

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
  deriving (Show)

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
  opts@Opts {..} <- getRecord "Create IAMX Exchange smart contract"

  print opts

  writeFileTextEnvelope plutusCorePath Nothing (iamxExchange config) >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote NFT validator to file " ++ plutusCorePath
