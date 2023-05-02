module Henforcer.Import.Scheme.Scheme
  ( Scheme (..)
  , AllowedSchemes
  , buildScheme
  ) where

import qualified Data.Map.Strict as M

import qualified CompatGHC
import Henforcer.Import.Scheme.Alias (Alias, determineAlias)
import Henforcer.Import.Scheme.Package (Package, determinePackage)
import Henforcer.Import.Scheme.Safe (Safe, determineSafe)

data Scheme = Scheme
  { qualification :: CompatGHC.ImportDeclQualifiedStyle
  , alias :: Alias
  , safe :: Safe
  , package :: Package
  }
  deriving (Eq)

type AllowedSchemes =
  M.Map CompatGHC.ModuleName [Scheme]

buildScheme :: CompatGHC.ImportDecl CompatGHC.GhcPs -> Scheme
buildScheme imp =
  Scheme
    { qualification = CompatGHC.ideclQualified imp
    , alias = determineAlias imp
    , safe = determineSafe imp
    , package = determinePackage imp
    }
