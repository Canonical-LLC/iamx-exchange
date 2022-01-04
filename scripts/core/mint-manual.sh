set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/..

iamxAddr=$1
iamxSigningKey=$2
exchangerAddr=$3
exchangerSigningKey=$4
rewardAddr=$5
imaxOutput=$6
mintValue=$7
redeemerFile=$8
utxo=$9

mintFile=$baseDir/$BLOCKCHAIN_PREFIX/iamx-exchange.plutus
scriptHash=$(cat $baseDir/$BLOCKCHAIN_PREFIX/iamx-exchange.addr)

$baseDir/hash-plutus.sh
bodyFile=temp/bid-tx-body.01
outFile=temp/bid-tx.01


imaxOutputFlag=""
if [ "$imaxOutput" != "" ];then
  imaxOutputFlag="$iamxAddr + $imaxOutput"
else
  imaxOutputFlag="$exchangerAddr + 1000000 lovelace"
fi


cardano-cli transaction build \
    --alonzo-era \
    $BLOCKCHAIN \
    --tx-in $utxo \
    --required-signer $iamxSigningKey \
    --required-signer $exchangerSigningKey \
    --tx-in-collateral $utxo \
    --tx-out "$imaxOutputFlag" \
    --change-address $exchangerAddr \
    --mint "$mintValue" \
    --mint-script-file $mintFile \
    --mint-redeemer-file $redeemerFile \
    --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
    --out-file $bodyFile

echo "saved transaction to $bodyFile"

cardano-cli transaction sign \
   --tx-body-file $bodyFile \
   --signing-key-file $iamxSigningKey \
   --signing-key-file $exchangerSigningKey \
   $BLOCKCHAIN \
   --out-file $outFile

echo "signed transaction and saved as $outFile"

cardano-cli transaction submit \
  $BLOCKCHAIN \
  --tx-file $outFile

echo "submitted transaction"

echo
