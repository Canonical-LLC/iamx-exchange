cardano-cli address build \
  --payment-script-file scripts/$BLOCKCHAIN_PREFIX/iamx-exchange.plutus \
  $BLOCKCHAIN \
  --out-file scripts/$BLOCKCHAIN_PREFIX/iamx-exchange.addr

cardano-cli transaction policyid \
    --script-file scripts/$BLOCKCHAIN_PREFIX/iamx-exchange.plutus \
      > scripts/$BLOCKCHAIN_PREFIX/iamx-exchange-policy-id.txt
