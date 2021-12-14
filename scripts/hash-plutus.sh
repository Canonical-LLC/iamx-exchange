cardano-cli address build \
  --payment-script-file scripts/iamx-exchange.plutus \
  $BLOCKCHAIN \
  --out-file scripts/$BLOCKCHAIN_PREFIX/iamx-exchange.addr
