A sequence of the following families (in order):

- Reference Script Deploying
    The script where we are <span class="underline">sending</span> the reference scripts should be deployed first.
    This will allow us to subsequently deploy more scripts at once.
- Staking Validator Registering
    You must register <span class="underline">all</span> staking validators during this step, including the `yieldingStakingValidators`
    and the yielded-to staking validators (the ones that encode the TxF business logic)
- Staking Validator Delegating
    You <span class="underline">should</span> delegate all _relevant_ staking validators during this step.
    Note that "withdraw-0" `YieldedTo` scripts don't need to be delegated -- registering is enough.
