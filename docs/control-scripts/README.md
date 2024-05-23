# Control Scripts

The YTxP architecture itself is enabled by the specification of a number of Plutus scripts which we call "control scripts".
These control scripts are independent of the business logic of protocols that use the YTxP architecture.
Protocols _using_ the YTxP architecture will make use of these scripts, and possibly additional scripts.

In the YTxP architecture, we make an effort to identify "scripts" with "transaction families"; but in the scripts that enable the architecture, we can't yet do that.
The YTxP architecture can't be described in terms of itself.

For this reason, we specify scripts in the way that most non-YTxP architectures specify them; as identified with the "thing" that they lock:

- In the case of a validator, we identify the scripts with the UTxOs that they lock. I.e., a "yielding validator script" is identified with a "yielding UTxO".
  In particular, we make a loose assertion that there is only one valid semantic interpretation of UTxOs locked at the script's address.
- In the case of a minting policy, we identify the script with the tokens it can mint or burn.
  In particular, we make a loose assertion that the all tokens bearing the same currency symbol have only one valid semantic interpretation.
- In the case of a staking validator, we identify the script with a rewards account and the actions that can be taken on it.

We do not provide templates for describing scripts in this way.
However, we do attempt to take a structured approach.
If your protocol has scripts that do not follow the YTxP architecture, you may benefit from describing your scripts in this manner.
