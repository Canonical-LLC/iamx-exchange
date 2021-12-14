set -eu
cabal run exe:create-smart-contract -- scripts/iamx-exchange.plutus
./scripts/hash-plutus.sh
