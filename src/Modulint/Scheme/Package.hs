module Modulint.Scheme.Package
  ( Package(..)
  , determinePackage
  ) where

import qualified CompatGHC as GHC

data Package
  = WithPackage String
  | WithoutPackage
  deriving (Eq)

determinePackage :: GHC.ImportDecl GHC.GhcPs -> Package
determinePackage idecl =
  case GHC.ideclPkgQual idecl of
    GHC.NoRawPkgQual -> WithoutPackage
    GHC.RawPkgQual x -> WithPackage . GHC.unpackFS $ GHC.sl_fs x
