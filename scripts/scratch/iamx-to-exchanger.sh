set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/..

bodyFile=temp/consolidate-tx-body.01
signingKey=~/$BLOCKCHAIN_PREFIX/iamx.skey
outFile=temp/consolidate-tx.01
senderAddr=$(cat ~/$BLOCKCHAIN_PREFIX/iamx.addr)
receiverAddr=$(cat ~/$BLOCKCHAIN_PREFIX/exchanger.addr)

cardano-cli transaction build \
  --alonzo-era \
  $BLOCKCHAIN \
  --tx-in 1b8112217466923c8fcf0f6fb5cd9f66f7c725add3760c0cf6e465151478c206#0 \
  --tx-in 1b8112217466923c8fcf0f6fb5cd9f66f7c725add3760c0cf6e465151478c206#2 \
  --tx-out "$receiverAddr + 2000000 lovelace + 33000000000 $(cat $baseDir/$BLOCKCHAIN_PREFIX/iamx-exchange-policy-id.txt).49414d58" \
  --change-address $senderAddr \
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
