set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/..

$baseDir/core/burn.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/iamx.addr) \
  ~/$BLOCKCHAIN_PREFIX/iamx.skey \
  $(cat ~/$BLOCKCHAIN_PREFIX/exchanger.addr) \
  ~/$BLOCKCHAIN_PREFIX/exchanger.skey \
  "-1 $(cat $baseDir/$BLOCKCHAIN_PREFIX/iamx-exchange-policy-id.txt).49414d58" \
  $baseDir/redeemers/burn.json
