set -eux
cabal run exe:create-smart-contract -- \
  --out-file scripts/$BLOCKCHAIN_PREFIX/iamx-exchange.plutus \
  --iamx-wallet $(cat temp/$BLOCKCHAIN_PREFIX/iamx-pkh.txt) \
  --iamx-policy-id d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2 \
  --iamx-token-name 49414d58

./scripts/hash-plutus.sh
