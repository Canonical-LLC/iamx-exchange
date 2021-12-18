set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/..

$baseDir/core/mint.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/iamx.addr) \
  ~/$BLOCKCHAIN_PREFIX/iamx.skey \
  $(cat ~/$BLOCKCHAIN_PREFIX/iamx.addr) \
  ~/$BLOCKCHAIN_PREFIX/iamx.skey \
  $(cat ~/$BLOCKCHAIN_PREFIX/reward.addr) \
  "1744798 lovelace + 33000000000 $(cat $baseDir/$BLOCKCHAIN_PREFIX/iamx-exchange-policy-id.txt).49414d58" \
  "" \
  "32999999999 $(cat $baseDir/$BLOCKCHAIN_PREFIX/iamx-exchange-policy-id.txt).49414d58" \
  $baseDir/redeemers/initial.json
