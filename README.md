# Blockchain utility

A repository with abstract building blocks, maintained by [Serokell](https://serokell.io), to be used for the construction of blockchain solutions.
The repository provides the following packages:

* Package `snowdrop-core`
    * Defines a read-only db access computation `ERoComp` (see modules `Snowdrop.Core.BaseM`, `Snowdrop.Core.ERoComp`).
    * Defines a value modification object type `ChangeSet`, a transaction to state `Tx`.
    * Defines means of stateful conversion of raw byte transaction representation to `Tx` (`Expander` data type).
    * Defines a data type `Validator` for validation of `Tx`.
* Package `snowdrop-block`
    * Defines a block sequence structural verification function `verifyFork` and its configuration `BlkConfiguration`.
    * Defines a block processing function `tryApplyFork` and its configuration `BlkStateConfiguration`.
* Package `snowdrop-execution`
    * Defines `DbAccessActions` and `DbModifyActions` -- method dictionary data types which are to be used for the execution of `DbAccess` actions on a base of real storage.
    * Defines the mempool functionality for accumulating transactions -- candidates for inclusion into blocks.
    * Defines a monad for execution of `ERoComp` in `IO` (module `Snowdrop.Execution.IOExecutor`)
* Package `snowdrop-util`
    * Defines various helper data types and functions needed for other packages.
     
The db access functor `DbAccess` is designed for usage with key-value databases.
The processing of blocks is viewed as a read-only state access operation that returns a state
modification object as result (or an error).
Utilities don't pay much attention to the modification of state and presume it to be a trivial operation
(executed sequentially for a series of changes under lock to database).

The block processing functionality is defined independently from definitions
of transaction processing in `snowdrop-core` and can be used independently.
Its usage with definitions of transaction processing from `snowdrop-core` is defined
by the configuration in module `Snowdrop.Block.State`.

Prefix `snowdrop` used all over the codebase refers to the codename of a project within Serokell.
These building blocks are being developed under umbrella of Snowdrop project.
The whole Snowdrop project (along with its repositories and documentation) will be announced and made public in Q4 2018.

## How to build

Use following command to build this package:

```
stack build
```

To build haddock use the following command (currently building Haddock for one of repo's deps results into out of memory):

```
stack haddock --fast --no-haddock-deps
```
## About Serokell

`blockchain-util` is maintained and funded with :heart: by [Serokell](https://serokell.io/). The names and logo for Serokell are trademark of Serokell OÜ.

We love open source software! See [our other projects](https://serokell.io/community?utm_source=github) or [hire us](https://serokell.io/hire-us?utm_source=github) to design, develop and grow your idea!
