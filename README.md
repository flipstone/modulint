# modulint has been retired, please see the successor project: https://github.com/flipstone/henforcer

# modulint

`modulint` is a tool to help keep your forest of Haskell modules organized
cleanly.

## Installation

`modulint` has not yet been released to hackage, so you should install it from
source either by cloning and building the repo, or adding a extra dep to your
`stack.yaml` like below and running `stack install modulint`

```
extra-deps:
- git: https://github.com/flipstone/modulint
  commit: <SHA of the latest master commit>
```

## Initialization

Once the `modulint` command is installed you'll need to initialize modulint for
your project. `cd` to your product directory and run the following command:

`modulint --init`

or

`stack exec modulint -- --init`

This will create an empty modulint configuration file at `modulint.dhall` in
the root of your project plus a `.modulint/package.dhall` file that defines the
configuration file format and is imported by `modulint.dhall`.

_Note: `modulint` may require a later version of `dhall` than is available in
earlier lts stackage snapshots. You may need to add an extra-dep like below to
get a compatible version of `dhall`, and its dependency `atomic-write`. You
may need to tweak `dhall` version below based on the `repline` version in your
lts as well -- use a `dhall` > `1.32` if your LTS has `repline` >= `0.4`.

```
# modulint needs a later dhall, which needs a later atomic-write
- dhall-1.32.0
- atomic-write-0.2.0.7
```


## Execution

Running `modulint` with no arguments from the project directory. You can also
run it from any directory by specifying the `--config <config path>` option
to tell `modulint` where the configuration file for the project lives.

## Configuration

`modulint` uses `dhall` files for configuration. The `.modulint/package.dhall`
file created by `modulint --init` defines the configuration options that are
available as well as default values for them all. You can use it as a handy
reference for configuring `modulint`.

Note that the default `modulint` configuration does not enforce any particular
rules, so running `modulint` immediately after installation will not report
module structure errors.

Source options are described below. More precise definitions can be found in
the [package.dhall](src/Modulint/Config/package.dhall) file.

### Source Paths

The `sourcePaths` option is the most import -- this option defines which
directories of files (relative to `modulint.dhall`) `modulint` will scan for
Haskell files when it runs.  You should set it such that any files you want
`modulint` to examine are found.

### Module Trees

`modulint` uses "module trees" as part of its configuration. A "module tree"
refers to a root module (e.g. `Data.Text`) and all the modules are prefixed by
it (e.g. `Data.Text.Encoding` and `Data.Text.Lazy`). For modules in your
project this almost always means a `src/Foo/MyModule.hs` file and any `.hs`
files contained inside the `src/Foo/MyModule` directory.

### Tree Dependencies

The `treeDependencies` options lets you declare that one module tree depends on
other trees. Declaring such a dependency tells `modulint` that you don't want
the dependency targetso to import anything from the dependent tree, which would
cause a backwards dependency rendering the two module trees logically
inseparable.  `modulint` will report any imports causing a backward dependency
as an error.

### Encapsulated Trees

The `encapsulatedTrees` options lets you declare that the root of a module tree
is effectively a public interface that any modules outside the tree should be
using. `modulint` will report an error if any module outside the tree attempts
to import a module from inside the encapsulated tree.

### Qualification Rules

The `qualificationRules` option lets you declare that certain modules must or
must not be imported as `qualified` as well an what aliases may be used for
the module when it is qualified.

