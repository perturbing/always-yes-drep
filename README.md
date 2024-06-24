# always-yes-drep
A gimmick plutus based DREP that can only vote yes.
## Compile the script and register Drep
The script can be compiled with `nix run .#write-scripts`, which will output it in text envelope format at `/assets/V3/alwaysVoteYesDrep.plutus`. Note that this is parametrized by an utxo ref that needs to be consumed to register this Drep (see `plutus-scripts/exe/Main.hs`). 

With this script compiled we can then determine the drep script hash via
```bash
cardano-cli conway governance hash script --script-file /assets/V3/alwaysVoteYesDrep.plutus > alwaysVoteYesDrep.hash
```
With the following we can create a registration certificate.
```bash
cardano-cli conway governance drep registration-certificate \
 --drep-script-hash $(cat alwaysVoteYesDrep.hash) \
 --key-reg-deposit-amt 500000000 \
 --out-file register-alwaysVoteYesDrep.cert
```
Note that sending this cert to the chain requires us to witness the drep, which we can if we spend the hardcoded UTXO ref in that transaction via
```bash
cardano-cli conway transaction build --testnet-magic 42 \
 --tx-in $(cardano-cli query utxo --address $(cat payment.addr) --output-json --testnet-magic 42 | jq -r 'keys[0]') \
 --tx-in-collateral $(cardano-cli query utxo --address $(cat payment.addr) --output-json --testnet-magic 42 | jq -r 'keys[0]') \
 --certificate-file register-alwaysVoteYesDrep.cert \
 --certificate-script-file alwaysVoteYesDrep.plutus \
 --certificate-redeemer-value {} \
 --change-address $(cat payment.addr) \
 --out-file tx
cardano-cli transaction sign --testnet-magic 42 --signing-key-file payment.skey --tx-body-file tx --out-file tx.signed
cardano-cli transaction submit --testnet-magic 42 --tx-file tx.signed
```
We can check that it is registered via
```bash
cardano-cli conway query drep-state --testnet-magic 42 --all-dreps
```
## How to use delegate to it
To delegate to this Drep use
```bash
cardano-cli conway stake-address vote-delegation-certificate \
--stake-verification-key-file stake.vkey \
--drep-script-hash $(cat alwaysVoteYesDrep.hash) \
--out-file vote-deleg.cert
```
and submit it to the chain.
## How to vote yes with it
To vote you can use
```bash
cardano-cli conway governance vote create \
    --yes \
    --governance-action-tx-id "yourGovActionHashHere" \
    --governance-action-index "yourGovActionIndexHere" \
    --drep-script-hash $(cat alwaysVoteYesDrep.hash) \
    --out-file constitution.vote
```
and submit it to the chain by witnessing the drep script via
```bash
cardano-cli conway transaction build --testnet-magic 42 \
 --tx-in $(cardano-cli query utxo --address $(cat payment.addr) --output-json --testnet-magic 42 | jq -r 'keys[0]') \
 --tx-in-collateral $(cardano-cli query utxo --address $(cat payment.addr) --output-json --testnet-magic 42 | jq -r 'keys[0]') \
 --vote-file register-alwaysVoteYesDrep.cert \
 --vote-script-file alwaysVoteYesDrep.plutus \
 --vote-redeemer-value {} \
 --change-address $(cat payment.addr) \
 --out-file tx
```