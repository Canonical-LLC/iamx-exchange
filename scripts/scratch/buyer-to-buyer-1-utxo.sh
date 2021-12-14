set -eux

bodyFile=temp/consolidate-tx-body.01
signingKey=~/$BLOCKCHAIN_PREFIX/seller.skey
outFile=temp/consolidate-tx.01
senderAddr=$(cat ~/$BLOCKCHAIN_PREFIX/seller.addr)
receiverAddr=$(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr)

cardano-cli transaction build \
  --alonzo-era \
  $BLOCKCHAIN \
  --tx-in 07723a68a710ec079b9382eb84a92830565518fd31e58d362b066b5dde4e3476#0 \
  --tx-out "$receiverAddr +20000000 lovelace" \
  --change-address $receiverAddr \
  --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
  --out-file $bodyFile

echo "saved transaction to $bodyFile"

cardano-cli transaction sign \
   --tx-body-file $bodyFile \
   --signing-key-file $signingKey \
   $BLOCKCHAIN \
   --out-file $outFile

echo "signed transaction and saved as $outFile"

cardano-cli transaction submit \
 $BLOCKCHAIN \
 --tx-file $outFile

echo "submitted transaction"
