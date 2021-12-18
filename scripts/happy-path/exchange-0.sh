set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/..
tempDir=$thisDir/../../temp

$baseDir/core/exchange-0.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/iamx.addr) \
  ~/$BLOCKCHAIN_PREFIX/iamx.skey \
  $(cat ~/$BLOCKCHAIN_PREFIX/exchanger.addr) \
  ~/$BLOCKCHAIN_PREFIX/exchanger.skey \
  $tempDir/$BLOCKCHAIN_PREFIX/redeemers/exchange.json \
  $(cat ~/$BLOCKCHAIN_PREFIX/reward.addr) \
  "2000000 lovelace + 1 $(cat $baseDir/$BLOCKCHAIN_PREFIX/iamx-exchange-policy-id.txt).49414d58"
