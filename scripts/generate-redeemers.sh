set -eux
thisDir=$(dirname "$0")
tempDir=$thisDir/../temp
generation=${1:-0}

exchangerPkh=$(cat $tempDir/$BLOCKCHAIN_PREFIX/exchanger-pkh.txt)
mkdir -p $tempDir/$BLOCKCHAIN_PREFIX/redeemers

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/redeemers/exchange.json
{
  "constructor": 2,
  "fields": [
    {
      "bytes": "$exchangerPkh"
    },
    {
      "int": $generation
    }
  ]
}

EOF
