module Modulint.Check
  ( CheckFailure(..)
  , CheckedDependency(..)
  , ImportChecker
  , newImportChecker
  , checkModule
  , errorMessagesFromList
  , toSDoc
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.ST as ST
import qualified Data.Foldable as Fold
import qualified Data.Map.Strict as M
import qualified Data.STRef as STRef

import qualified CompatGHC as GHC
import Modulint.CheckFailure
    ( CheckFailure(DependencyViolation,EncapsulationViolation,QualificationViolation), CheckedDependency(CheckedDependency, dependencySource, dependencyTarget), errorMessagesFromList, toSDoc )
import qualified Modulint.Config as Config
import qualified Modulint.Import as Import
import qualified Modulint.Scheme as Scheme
import qualified Modulint.TreeName as TreeName

data ImportChecker =
  ImportChecker
    { dependencies           :: [CheckedDependency]
    , encapsulatedTrees      :: [TreeName.TreeName]
    , allowedQualifications  :: Scheme.AllowedSchemes
    }

checkModule :: ImportChecker -> Either String GHC.HsModule  -> Either String [CheckFailure]
checkModule importChecker =
  (checkModuleImports importChecker =<<)

checkModuleImports :: ImportChecker -> GHC.HsModule -> Either String [CheckFailure]
checkModuleImports checker =
  fmap (checkImports checker) . Import.getModuleImports

newImportChecker :: Config.Config -> ImportChecker
newImportChecker config =
  let
    explodeDependencies decl =
      fmap (CheckedDependency (Config.moduleTree decl))
      $ Config.treeDependencies decl

    configuredDependencies =
      concatMap explodeDependencies
      . Config.dependencyDeclarations
      $ config
  in
    ImportChecker
      { dependencies          = configuredDependencies
      , encapsulatedTrees     = Config.encapsulatedTrees config
      , allowedQualifications = Config.allowedQualifications config
      }

checkImports :: Foldable f
             => ImportChecker
             -> f Import.Import
             -> [CheckFailure]
checkImports importChecker imports =
  ST.runST $ do
    failures <- STRef.newSTRef []
    Fold.traverse_ (checkImport failures importChecker) imports
    STRef.readSTRef failures

checkImport :: STRef.STRef s [CheckFailure]
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

  let
    targetName = Import.importedModule imp

  Fold.traverse_
    (checkImportQualification failures imp)
    (M.lookup targetName (allowedQualifications checker))

checkImportAgainstDependency :: STRef.STRef s [CheckFailure]
                             -> Import.Import
                             -> CheckedDependency
                             -> ST.ST s ()
checkImportAgainstDependency failures imp dep = do
  let
    depSource       = dependencySource dep
    depTarget       = dependencyTarget dep

    -- If the file that contains the import belongs to the module tree that is
    -- the dependency target then we need to check wether the module being
    -- imported (the imported target) is contained in the module tree that the
    -- dependency is declared for. This is -- once a dependency is declared, we
    -- don't allow dependency targets to import modules that depend on them.
    dependencyViolated  =
      TreeName.treeContainsModule depTarget (Import.srcModule imp)
      && TreeName.treeContainsModule depSource (Import.importedModule imp)

  Monad.when
    dependencyViolated
    (addFailure failures (DependencyViolation imp dep))

checkImportAgainstEncapsulation :: STRef.STRef s [CheckFailure]
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

checkImportQualification :: STRef.STRef s [CheckFailure]
                         -> Import.Import
                         -> [Scheme.Scheme]
                         -> ST.ST s ()
checkImportQualification failures imp alloweds =
  if
    elem (Scheme.buildScheme . GHC.unLoc $ Import.importDecl imp) alloweds
  then
    pure ()
  else
    addFailure failures (QualificationViolation imp alloweds)

addFailure :: STRef.STRef s [CheckFailure] -> CheckFailure -> ST.ST s ()
addFailure failures err =
  STRef.modifySTRef failures (err :)
