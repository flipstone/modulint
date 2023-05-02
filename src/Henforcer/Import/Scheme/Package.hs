module Henforcer.Import.Scheme.Package
  ( Package (..)
  , determinePackage
  ) where

import qualified CompatGHC

data Package
  = WithPackage String
  | WithoutPackage
  deriving (Eq)

determinePackage :: CompatGHC.ImportDecl CompatGHC.GhcPs -> Package
determinePackage idecl =
  case CompatGHC.ideclPkgQual idecl of
    CompatGHC.NoRawPkgQual -> WithoutPackage
    CompatGHC.RawPkgQual x -> WithPackage . CompatGHC.unpackFS $ CompatGHC.sl_fs x
