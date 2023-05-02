{- |
Module      : Henforcer.Import.Import
Description :
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : development@flipstone.com
Stability   : experimental
Portability : POSIX
-}
module Henforcer.Import.Import
  ( Import (srcModule, importDecl)
  , srcLocation
  , importedModule
  , getImports
  , importIsOpenWithNoHidingOrAlias
  ) where

import qualified CompatGHC
import Henforcer.Import.Scheme (Alias (WithoutAlias), Scheme (Scheme), buildScheme)

-- | `Import` is a subset of a CompatGHC.HsModule to be a slightly more ergonomic interface.
data Import = Import
  { srcModule :: CompatGHC.ModuleName -- Do not support unnamed modules just yet!
  , importDecl :: CompatGHC.LImportDecl CompatGHC.GhcPs
  }

srcLocation :: Import -> CompatGHC.SrcSpan
srcLocation = CompatGHC.locA . CompatGHC.getLoc . importDecl

importedModule :: Import -> CompatGHC.ModuleName
importedModule = CompatGHC.unLoc . CompatGHC.ideclName . CompatGHC.unLoc . importDecl

getImports :: CompatGHC.HsModule -> [Import]
getImports hsMod =
  let mbName = fmap CompatGHC.unLoc $ CompatGHC.hsmodName hsMod
   in case mbName of
        Nothing -> []
        Just n -> fmap (Import n) $ CompatGHC.hsmodImports hsMod

importIsOpenWithNoHidingOrAlias :: Import -> Bool
importIsOpenWithNoHidingOrAlias imp =
  let rawImportDecl = CompatGHC.unLoc $ importDecl imp
   in case buildScheme rawImportDecl of
        Scheme CompatGHC.NotQualified WithoutAlias _ _ ->
          case CompatGHC.ideclHiding rawImportDecl of
            Nothing -> True
            Just _ -> False
        _ -> False
