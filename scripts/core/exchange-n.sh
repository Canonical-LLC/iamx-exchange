set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/..

iamxAddr=$1
iamxSigningKey=$2
exchangerAddr=$3
exchangerSigningKey=$4
redeemerFile=$5
rewardAddr=$6
rewardOutput=$7
mintValue=$8

mintFile=$baseDir/$BLOCKCHAIN_PREFIX/iamx-exchange.plutus
scriptHash=$(cat $baseDir/$BLOCKCHAIN_PREFIX/iamx-exchange.addr)

$baseDir/hash-plutus.sh
bodyFile=temp/bid-tx-body.01
outFile=temp/bid-tx.01

changeOutput=$(cardano-cli-balance-fixer change --address $exchangerAddr $BLOCKCHAIN -o "+ 1 $(cat $baseDir/$BLOCKCHAIN_PREFIX/iamx-exchange-policy-id.txt).49414d58")

extraOutput=""
if [ "$changeOutput" != "" ];then
  extraOutput="+ $changeOutput"
fi


cardano-cli transaction build \
    --alonzo-era \
    $BLOCKCHAIN \
    $(cardano-cli-balance-fixer input --address $exchangerAddr $BLOCKCHAIN ) \
    --required-signer $iamxSigningKey \
    --required-signer $exchangerSigningKey \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $exchangerAddr $BLOCKCHAIN) \
    --tx-out "$exchangerAddr + 3000000 lovelace $extraOutput" \
    --tx-out "$rewardAddr + $rewardOutput" \
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
