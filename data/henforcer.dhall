--| This loads the Henforcer package that was installed when you ran
--  henforcer --init aliased as `Ml` and the record auto-complete operator
--  `::` to construct a Henforcer Config value with all the default values,
--  setting only `sourcePaths`. You can add you configuration values below
--  as more field slike `sourcePaths`. Consult the package.dhall file for a
--  reference to settings and the provided defaults.
let Ml = ./.henforcer/package.dhall in Ml.Config::{ sourcePaths = [ "src" ] }
