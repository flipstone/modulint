module Modulint.Import
  ( Import(srcModule, importDecl)
  , srcLocation
  , importedModule
  , getModuleImports
  )
where

import qualified CompatGHC as GHC

{-| `Import` is a subset of a GHC.HsModule to be a slightly more ergonomic interface. -}
data Import =
  Import
    { srcModule :: GHC.ModuleName
    , importDecl :: GHC.LImportDecl GHC.GhcPs
    }

srcLocation :: Import -> GHC.SrcSpan
srcLocation = GHC.locA . GHC.getLoc . importDecl

importedModule :: Import -> GHC.ModuleName
importedModule = GHC.unLoc . GHC.ideclName . GHC.unLoc .importDecl

getModuleImports :: GHC.HsModule -> Either String [Import]
getModuleImports hsmod =
  case GHC.hsmodName hsmod of
    Nothing ->
      -- This appears to be the way ghc signals no module head
      Left "Modulint does not yet support modules files without a module head"
    Just locName ->
      pure (fmap (Import (GHC.unLoc locName)) (GHC.hsmodImports hsmod))
