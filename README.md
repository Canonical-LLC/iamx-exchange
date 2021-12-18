# IAMX Minting Contract

This is the contract describes the condititions for minting and burning IMAX tokens.

There are three possible minting scenarios:

- Initial
- Burn
- Reward

All of these scenarios require IMAX to sign the transactions.

## Initial Minting

The contract ensures that the initial mint has 33,000,000,000 IAMX tokens. The contract is compiled with a specific UTxO that must be spent. This ensures the initial operation can only be called once.

## Burn

If IMAX deems the token burnable due to off-chain rules, it will get burned.

## Reward

Rewards are distributed based on certain off-chain rules. Based on the generation passed in the following rewards are minted to the address IAMX
specifies.

- Gen 1: 2 token are rewarded.
- Gen 2: 3 tokens are rewarded.
- Gen 3: 5 tokens are rewarded.

## Building

To build, run:

```bash
$ cabal build
```

## Compiling the Smart Contracts

The smart contract requires UTxO is passed into the `create-smart-contract`.
To see the `create-smart-contract` help run:

```bash
$ cabal run create-smart-contract -- --help

Create IAMX Exchange smart contract

Usage: create-smart-contract [-o|--out-file STRING] --iamx-wallet STRING
                             --iamx-token-name STRING --initial-utxo STRING

```

- `--out-file` specifies the file path to write the plutus core file.
- `--iamx-wallet` is the public key hash corresponding to the signing key IAMX will use for signing transactions.
- `--iamx-token-name` is the ASCII token name, e.g. IAMX
- `--initial-utxo` is a required UTxO, owned by the `--iamx-wallet` wallet, used to ensure the initial minting can only happen once.

See `scripts/compile.sh` for an example usage.

# Testing

## Prerequistes

The testing flow depends on `cardano-cli-balance-fixer` utility. Clone https://github.com/Canonical-LLC/cardano-cli-balance-fixer and `cabal install` to build and install.

## General

When in a shell, before running anything below, source the env vars for file for either mainnet or testnet, depending on which you're testing on.


For testnet

```
$ source scripts/envars/testnet-env.envvars
```

For mainnet

```
$ source scripts/envars/mainnet-env.envvars
```

The environment variable files set `CARDANO_NODE_SOCKET_PATH` to the path of the appropriate Daedalus socket file (either Testnet Daedalus or the regular mainnet Daedalus). It you run a `cardano-node` on your own you should set this environment variable to your socket file location after sourcing the environment variable file.

## Init (only done once)

First create the wallets, get the protocol parameters, compile the plutus, and create the script address

```
$ ./scripts/wallets/make-all-wallets.sh
$ ./scripts/query-protocol-parameters.sh
$ ./scripts/compile.sh
```

## Make sure the `iamx` has funds

If you just created the wallets, find the iamx address (it will be different then the example value below).

```
$ cat ~/$BLOCKCHAIN_PREFIX/iamx.addr
addr_test1vz2wnmjhkvg6t59uh8q39svqq4cms6vdyha802apqwvstuq80a88a
```

If you're testing on the mainnet, you'll need to send some Ada to that address from your wallet (or have someone else send it).

If you're testing on the testnet, you can go to the faucet <https://testnets.cardano.org/en/testnets/cardano/tools/faucet/> and send Ada to that address.

Wait a bit and check that the funds are available

```
$ ./scripts/query/iamx.sh
++ scripts/query/find-utxo.sh iamx
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
6595d9126749b6df408578be6cdeef1ef22f17ebab057a3b198d18501d0a4d96     0        1000000000 lovelace + TxOutDatumHashNone
```

If you don't see any transactions, wait a bit longer and try again.

Once we have wallets and the sender has funds, we're ready for testing.

## Test happy path

First we must create the initial tokens. Run:

```bash
$ scripts/happy-path/initial.sh
$ scripts/wait/until-next-block.sh
$ scripts/query/iamx.sh
```

Which should display the newly minted 33 billion tokens

## Airdrop the Tokens to the Exchanger

Next will similate an airdrop.

Modify the `--tx-in` in the `scripts/scratch/iamx-to-exchanger.sh` file to include the newly minted UTxO from the iamx wallet, as well as a UTxO to cover fees.

Run:

```bash
$ scripts/scratch/iamx-to-exchanger.sh
$ scripts/wait/until-next-block.sh
$ scripts/query/exchanger.sh
```

The tokens should be transfered to the exchanger address.

## Gen 0 Exchange

The gen 0 exchange is not enforce by the contract but example scripts are given for the exchange. To execute the exchange run:

```bash
$ scripts/happy-path/exchange-0.sh
$ scripts/query/reward.sh # View the rewards were minted and sent
```

## Gen 1-3 Exchange

Generate the redeemer and run the exchange:

```bash
$ scripts/generate-redeemer 1 # The generation number
$ scripts/happy-path/exchange-n 2 # The reward
$ scripts/query/reward.sh # View the rewards were minted and sent
```

## Failure cases

To test various failure cases run the scripts in `scripts/failure-case` instead of the happy path scripts. Additionally, failure cases can be demonstrated by passing an invalid reward to `scripts/happy-path/exchange-n`
