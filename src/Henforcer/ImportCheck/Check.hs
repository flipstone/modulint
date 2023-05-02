{- |
Module      : Henforcer.ImportCheck.Check
Description :
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : development@flipstone.com
Stability   : experimental
Portability : POSIX
-}
module Henforcer.ImportCheck.Check
  ( ImportChecker
  , newImportChecker
  , checkModule
  ) where

import qualified Control.Monad as Monad
import qualified Control.Monad.ST as ST
import qualified Data.Foldable as Fold
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as M
import qualified Data.STRef as STRef

import qualified CompatGHC
import qualified Henforcer.Config as Config
import qualified Henforcer.Import as Import
import Henforcer.ImportCheck.CheckFailure
  ( CheckFailure
      ( DependencyViolation
      , EncapsulationViolation
      , OpenImportViolation
      , QualificationViolation
      )
  , CheckedDependency (CheckedDependency, dependencySource, dependencyTarget)
  )
import qualified Henforcer.TreeName as TreeName

data ImportChecker = ImportChecker
  { dependencies :: [CheckedDependency]
  , encapsulatedTrees :: [TreeName.TreeName]
  , allowedQualifications :: Import.AllowedSchemes
  , allowedOpenUnaliasedImports :: Import.AllowedOpenUnaliasedImports
  }

checkModule :: ImportChecker -> CompatGHC.HsModule -> [CheckFailure]
checkModule = checkImports

newImportChecker :: Config.Config -> ImportChecker
newImportChecker config =
  let explodeDependencies decl =
        fmap (CheckedDependency (Config.moduleTree decl)) $
          Config.treeDependencies decl

      configuredDependencies =
        concatMap explodeDependencies
          . Config.dependencyDeclarations
          $ config
   in ImportChecker
        { dependencies = configuredDependencies
        , encapsulatedTrees = Config.encapsulatedTrees config
        , allowedQualifications = Config.allowedQualifications config
        , allowedOpenUnaliasedImports = Config.allowedOpenUnaliasedImports config
        }

checkImports ::
  ImportChecker
  -> CompatGHC.HsModule
  -> [CheckFailure]
checkImports importChecker hsModule =
  let imports = Import.getImports hsModule
      mbModuleName = fmap CompatGHC.unLoc $ CompatGHC.hsmodName hsModule
   in ST.runST $ do
        failures <- STRef.newSTRef []
        Fold.traverse_ (checkImport failures importChecker) imports
        checkOpenImport failures mbModuleName (allowedOpenUnaliasedImports importChecker) imports
        STRef.readSTRef failures

checkImport ::
  STRef.STRef s [CheckFailure]
  -> ImportChecker
  -> Import.Import
  -> ST.ST s ()
checkImport failures checker imp = do
  Fold.traverse_
    (checkImportAgainstDependency failures imp)
    (dependencies checker)

  Fold.traverse_
    (checkImportAgainstEncapsulation failures imp)
    (encapsulatedTrees checker)

  let targetName = Import.importedModule imp

  Fold.traverse_
    (checkImportQualification failures imp)
    (M.lookup targetName (allowedQualifications checker))

checkImportAgainstDependency ::
  STRef.STRef s [CheckFailure]
  -> Import.Import
  -> CheckedDependency
  -> ST.ST s ()
checkImportAgainstDependency failures imp dep = do
  let depSource = dependencySource dep
      depTarget = dependencyTarget dep

      -- If the file that contains the import belongs to the module tree that is
      -- the dependency target then we need to check wether the module being
      -- imported (the imported target) is contained in the module tree that the
      -- dependency is declared for. This is -- once a dependency is declared, we
      -- don't allow dependency targets to import modules that depend on them.
      dependencyViolated =
        TreeName.treeContainsModule depTarget (Import.srcModule imp)
          && TreeName.treeContainsModule depSource (Import.importedModule imp)

  Monad.when
    dependencyViolated
    (addFailure failures (DependencyViolation imp dep))

checkImportAgainstEncapsulation ::
  STRef.STRef s [CheckFailure]
  -> Import.Import
  -> TreeName.TreeName
  -> ST.ST s ()
checkImportAgainstEncapsulation failures imp encapsulatedTree = do
  let
    -- If the module being imported belongs to an encapsulated module tree
    -- then it may only be directly imported from within that tree. Imports
    -- by modules outside the encapsulated tree constitute a violation.
    encapsulationViolated =
      TreeName.treeStrictlyContainsModule encapsulatedTree (Import.importedModule imp)
        && (not . TreeName.treeContainsModule encapsulatedTree $ Import.srcModule imp)

  Monad.when
    encapsulationViolated
    (addFailure failures (EncapsulationViolation imp encapsulatedTree))

checkImportQualification ::
  STRef.STRef s [CheckFailure]
  -> Import.Import
  -> [Import.Scheme]
  -> ST.ST s ()
checkImportQualification failures imp alloweds =
  if elem (Import.buildScheme . CompatGHC.unLoc $ Import.importDecl imp) alloweds
    then pure ()
    else addFailure failures (QualificationViolation imp alloweds)

checkOpenImport ::
  STRef.STRef s [CheckFailure]
  -> Maybe CompatGHC.ModuleName
  -> Import.AllowedOpenUnaliasedImports
  -> [Import.Import]
  -> ST.ST s ()
checkOpenImport failures mbModName openAllowed imports =
  let openImports = filter Import.importIsOpenWithNoHidingOrAlias imports
   in case (openImports, openAllowed, mbModName) of
        ([], _, _) ->
          pure ()
        (x : xs, Import.GlobalAllowedOpenUnaliasedImports nat, _) ->
          checkNonEmptyOpenImports failures (x NEL.:| xs) nat
        (x : xs, Import.PerModuleOpenUnaliasedImports allowedMap, Just modName) ->
          Fold.traverse_ (checkNonEmptyOpenImports failures (x NEL.:| xs)) $ M.lookup modName allowedMap
        (_, _, _) ->
          pure () -- TODO: Support unamed modules? But if it isn't named, how can we look up a value?

checkNonEmptyOpenImports ::
  STRef.STRef s [CheckFailure]
  -> NEL.NonEmpty Import.Import
  -> Import.MaxOpenUnaliasedImportsNat
  -> ST.ST s ()
checkNonEmptyOpenImports failures nonEmptyOpenImports nat =
  if NEL.length nonEmptyOpenImports > fromIntegral nat
    then addFailure failures $ OpenImportViolation nonEmptyOpenImports nat
    else pure ()

addFailure :: STRef.STRef s [CheckFailure] -> CheckFailure -> ST.ST s ()
addFailure failures err =
  STRef.modifySTRef failures (err :)
