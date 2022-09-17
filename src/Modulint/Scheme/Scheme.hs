module Modulint.Scheme.Scheme
  ( Scheme(..)
  , AllowedSchemes
  , buildScheme
  )
  where

import qualified Data.Map.Strict as M

import qualified CompatGHC as GHC
import Modulint.Scheme.Alias (Alias, determineAlias)
import Modulint.Scheme.Package (Package, determinePackage)
import Modulint.Scheme.Safe (Safe, determineSafe)

data Scheme =
  Scheme
    { qualification ::  GHC.ImportDeclQualifiedStyle
    , alias :: Alias
    , safe :: Safe
    , package :: Package
    }
  deriving (Eq)

type AllowedSchemes =
  M.Map GHC.ModuleName [Scheme]

buildScheme :: GHC.ImportDecl GHC.GhcPs -> Scheme
buildScheme imp =
  Scheme
    { qualification = GHC.ideclQualified imp
    , alias = determineAlias imp
    , safe = determineSafe imp
    , package = determinePackage imp
    }
