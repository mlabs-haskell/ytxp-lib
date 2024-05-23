A sequence of the following transactions (in order):

- Mint Authorised Script ST
    Mint a token with the `authorisedScriptsSTCS` currency symbol and arbitrary token name. This token will be used to identify Authorised Scripts.
    This token could be minted by an admin, a multi-sig, or some governance vote.
- Reference Script Deploying
    One or more authorised reference script must be deployed. The UTxO for these reference scripts <span class="underline">must</span> contain a token with the `authorisedScriptsSTCS`.
- Staking Validator Registering
    You must register <span class="underline">all</span> staking validators during this step, including the `yieldingStakingValidators`
    and the yielded-to staking validators (the ones that encode the TxF business logic)
- Staking Validator Delegating
    You <span class="underline">should</span> delegate all _relevant_ staking validators during this step.
    Note that "withdraw-0" `YieldedTo` scripts don't need to be delegated -- registering is enough.
