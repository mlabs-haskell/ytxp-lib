# Maintenance (Adding, updating, or removing transaction flows or components)

- First, determine if your upgrade path requires migration.
  Examples might be adding a field to a datum of a component.
  - Determine where this migration will happen. It is acceptable to deploy a "Component  Upgrading" transaction family that is only used to move from one version of component to another.
- Determine if there may be any transaction flows that need to be "terminated". For instance, terminating a "batching flow" may be required to force all users to upgrade to the new semantics of an optimized batching flow.
  - If your transaction flow is a DAG, termination can be done by removing all "entry point" transaction families. In-process flows can complete, but new flows can't begin.
  - If your transaction flow loops:
    - remove all entry point transaction families
    - remove the transaction families that allows loops to progress past their "safe stopping point".
  This will allow in progress loops to terminate and any non-looping branches of the graph to proceed, but new loops or branches cannot start.
- If you need to, ensure that all in-process transaction flows have reached their termination point by executing the appropriate transaction.
- Mint a new AuthorisedScriptST Token and deploy a new authorised reference script. Ensure that transactions coming from interrupted loops have a migration path; these may require dedicated transaction families.
