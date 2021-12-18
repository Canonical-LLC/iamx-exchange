set -eux
mkdir -p scripts/temp/
mkdir -p ~/$BLOCKCHAIN_PREFIX
./scripts/wallets/make-wallet-and-pkh.sh iamx
./scripts/wallets/make-wallet-and-pkh.sh exchanger
./scripts/wallets/make-wallet-and-pkh.sh reward
