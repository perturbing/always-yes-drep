# always-yes-drep
A gimmick plutus based DRep that can only vote yes.

The hash of this DRep on the private testnet is given by `3b943c3e9598ef0e8d6bf504e7ddeee73232b1825380765c04e25055` (registered in tx `76fb8a6573ae34bd4cc5b12f153776cf85e388b9cf56c4cc39bef90396ed6899`)
## Compile the script and register Drep
The script can be compiled with `nix run .#write-scripts`, which will output it in text envelope format at `/assets/V3/alwaysVoteYesDrep.plutus`. Note that this is parametrized by an utxo ref that needs to be consumed to register this Drep (see `plutus-scripts/exe/Main.hs`). 

With this script compiled we can then determine the drep script hash via
```bash
cardano-cli conway governance hash script --script-file ./assets/V3/alwaysVoteYesDrep.plutus > alwaysVoteYesDrep.hash
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
cardano-cli conway transaction build --testnet-magic 5 \
 --tx-in $(cardano-cli query utxo --address $(cat payment.addr) --output-json --testnet-magic 5 | jq -r 'keys[0]') \
 --tx-in-collateral $(cardano-cli query utxo --address $(cat payment.addr) --output-json --testnet-magic 5 | jq -r 'keys[0]') \
 --certificate-file register-alwaysVoteYesDrep.cert \
 --certificate-script-file alwaysVoteYesDrep.plutus \
 --certificate-redeemer-value {} \
 --change-address $(cat payment.addr) \
 --out-file tx
cardano-cli transaction sign --testnet-magic 5 --signing-key-file payment.skey --tx-body-file tx --out-file tx.signed
cardano-cli transaction submit --testnet-magic 5 --tx-file tx.signed
```
We can check that it is registered via
```bash
cardano-cli conway query drep-state --testnet-magic 5 --all-dreps
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
    --out-file myGovAction.vote
```
and submit it to the chain by witnessing the drep script via the following `build-raw` command (there is a bug in the cli)
```bash
ADA_ONLY_UTXO=$(cardano-cli query utxo --address $(cat payment.addr) --output-json --testnet-magic 5 | jq -r 'to_entries | map(select(.value.value | keys | length == 1 and contains(["lovelace"]))) | .[0].key')
ADA_ONLY_UTXO_LOVELACE_VALUE=$(cardano-cli query utxo --address $(cat payment.addr) --output-json --testnet-magic 5 | jq '.'\"$ADA_ONLY_UTXO\"'' | jq '.value.lovelace')
FEE=5000000
PAYMENT_RETURN_LOVELACE=$(($ADA_ONLY_UTXO_LOVELACE_VALUE - $FEE))
cardano-cli query protocol-parameters --testnet-magic 5 --out-file pparams.json
cardano-cli conway transaction build-raw \
 --tx-in $ADA_ONLY_UTXO \
 --tx-in-collateral $ADA_ONLY_UTXO \
 --vote-file myGovAction.vote \
 --vote-script-file alwaysVoteYesDrep.plutus \
 --vote-redeemer-value {} \
 --vote-execution-units "(4000000000,4000000)" \
 --tx-out $(cat payment.addr)+$PAYMENT_RETURN_LOVELACE \
 --fee $FEE \
 --protocol-params-file pparams.json \
 --out-file tx
 cardano-cli transaction sign --testnet-magic 5 --signing-key-file payment.skey --tx-body-file tx --out-file tx.signed
 cardano-cli transaction submit --testnet-magic 5 --tx-file tx.signed
```
And to convince you that the script can never vote no, try voting not with it :)