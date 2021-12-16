set -eux
cabal run exe:create-smart-contract -- \
  --out-file scripts/$BLOCKCHAIN_PREFIX/iamx-exchange.plutus \
  --iamx-wallet $(cat temp/$BLOCKCHAIN_PREFIX/iamx-pkh.txt) \
  --iamx-token-name 49414d58 \
  --initial-utxo $(./scripts/query/iamx.sh | tail -1 | cardano-cli-balance-fixer parse-as-utxo)

./scripts/hash-plutus.sh
