set -eux

utxo=${1:-$(./scripts/query/iamx.sh | tail -1 | head | cardano-cli-balance-fixer parse-as-utxo)}

cabal run exe:create-smart-contract -- \
  --out-file scripts/$BLOCKCHAIN_PREFIX/iamx-exchange.plutus \
  --iamx-wallet $(cat temp/$BLOCKCHAIN_PREFIX/iamx-pkh.txt) \
  --iamx-token-name IAMX \
  --initial-utxo $utxo

./scripts/hash-plutus.sh
