module Henforcer.Import.Scheme.Alias
  ( Alias (..)
  , determineAlias
  ) where

import qualified CompatGHC

data Alias
  = WithAlias CompatGHC.ModuleName
  | WithoutAlias
  deriving (Eq)

determineAlias :: CompatGHC.ImportDecl CompatGHC.GhcPs -> Alias
determineAlias =
  maybe WithoutAlias (WithAlias . CompatGHC.unLoc) . CompatGHC.ideclAs
