# Blockchain utility

Repository with some abstract building blocks to be used for construction of blockchain solutions.
Repository provides following packages:

* Package `snowdrop-core`
    * Defines read-only db access computation `ERoComp` (see modules `Snowdrop.Core.BaseM`, `Snowdrop.Core.ERoComp`).
    * Defines value modification object type `ChangeSet`, a transaction to state `Tx`.
    * Defines means of stateful conversion of raw byte transaction representation to `Tx` (`Expander` data type).
    * Defines a data type `Validator` for validation of `Tx`.
* Package `snowdrop-block`
    * Defines block sequence structural verification function `verifyFork` and its configuration `BlkConfiguration`.
    * Defines block processing function `tryApplyFork` and its configuration `BlkStateConfiguration`.
* Package `snowdrop-execution`
    * Defines `DbAccessActions` and `DbModifyActions` -- method dictionary data types which are to be used for execution of `DbAccess` actions on base of real storage.
    * Defines mempool functionality for accumulating transactions -- candidates for inclusion into blocks.
    * Defines monad for execution of `ERoComp` in `IO` (module `Snowdrop.Execution.IOExecutor`)
* Package `snowdrop-util`
    * Various helper data types and functions needed for other packages.
     
The db access functor `DbAccess` is designed for use with key-value databases which
(as believed by authors of repository) is a primary case for blockchain processing node.
Processing of blocks is viewed as a read-only state access operation that returns
modifiaction object as result (or an error).
Utilities don't pay much attention to modification of state and presumes it to be a trivial operation
(executed sequentially for a series of changes under lock to database).

Block processing functionality is defined independently from definitions
of transaction processing in `snowdrop-core` and can be used indendently.
Its usage with definitions of transaction from `snowdrop-core` is defined
by configuration in module `Snowdrop.Block.State`.

Prefix `snowdrop` used all over the codebase states for the codename for a project within Serokell,
under umbrella of which these building blocks are being developed.
The whole project (along with its repositories and documentation) will be announced and made public in Q4 2018.
