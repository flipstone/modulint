module Modulint.Scheme.Alias
  ( Alias(..)
  , determineAlias
  ) where

import qualified CompatGHC as GHC

data Alias
  = WithAlias GHC.ModuleName
  | WithoutAlias
  deriving (Eq)

determineAlias :: GHC.ImportDecl GHC.GhcPs -> Alias
determineAlias =
  maybe WithoutAlias (WithAlias . GHC.unLoc) . GHC.ideclAs
