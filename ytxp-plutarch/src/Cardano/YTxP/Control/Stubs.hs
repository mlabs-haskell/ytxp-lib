{- | Module: Caradno.YTxP.Control.Stubs
Description: Development stubs. Use these when you don't
have some scripts fully implemented yet.
-}
module Cardano.YTxP.Control.Stubs (
  alwaysSucceedsValidator,
  alwaysSucceedsTwoArgumentScript,
  noncedValidatorWrapper,
  noncedTwoArgumentScriptWrapper,
) where

import Plutarch.LedgerApi (PScriptContext)

--------------------------------------------------------------------------------
-- Validator stubs

-- | A validator that will never fail
alwaysSucceedsValidator ::
  forall (s :: S).
  Term s (PData :--> PData :--> PScriptContext :--> POpaque)
alwaysSucceedsValidator = plam $ \_ _ _ -> popaque (pconstant ())

{- | Compile a nonce into the script, changing the script hash. This can be useful if you need
many stubbed validators with different addresses.
-}
noncedValidatorWrapper ::
  forall (nonceType :: S -> Type) (s :: S).
  Term s nonceType ->
  Term s (PData :--> PData :--> PScriptContext :--> POpaque) ->
  Term s (PData :--> PData :--> PScriptContext :--> POpaque)
noncedValidatorWrapper nonce wrappedScript =
  plet nonce $
    const
      (plam $ \datum redeemer ctx -> wrappedScript # datum # redeemer # ctx)

--------------------------------------------------------------------------------
-- Two argument script (minting policies and staking validator) stubs

-- | A two argument script (minting policy or staking validator) that will never fail
alwaysSucceedsTwoArgumentScript ::
  forall (s :: S).
  Term s (PData :--> PScriptContext :--> POpaque)
alwaysSucceedsTwoArgumentScript = plam $ \_ _ -> popaque (pconstant ())

{- | Compiles a nonce into the script, changing the script hashes.
This is useful if you need many stubbed validators with different addresses
-}
noncedTwoArgumentScriptWrapper ::
  forall (nonceType :: S -> Type) (s :: S).
  Term s nonceType ->
  Term s (PData :--> PScriptContext :--> POpaque) ->
  Term s (PData :--> PScriptContext :--> POpaque)
noncedTwoArgumentScriptWrapper nonce wrappedScript =
  plet nonce $
    const
      (plam $ \redeemer ctx -> wrappedScript # redeemer # ctx)
