module Modulint.Dependencies
  ( DependencyGraph
  , buildDependencyGraph
  , TreeName
  , formatTreeName
  , treeNameDepth
  , nodes
  , Target
  , targetTreeName
  , targetCauses
  , dependencyTargets
  ) where

import qualified Control.Monad as Monad
import           Data.Bifunctor (bimap)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Language.Haskell.Exts.Syntax as Syntax
import qualified Numeric.Natural as Nat

import qualified Modulint.Imports as Imports

data TreeName =
  TreeName String (Maybe TreeName)
  deriving (Show, Ord, Eq)

formatTreeName :: TreeName -> String
formatTreeName (TreeName name maybeRest) =
  maybe name joinName maybeRest
    where
      joinName rest =
        name ++ ('.' : formatTreeName rest)

treeNameDepth :: TreeName -> Nat.Natural
treeNameDepth =
  go 1
    where
      go depth (TreeName _ child) =
        case child of
          Nothing ->
            depth

          Just treeName ->
            go (depth + 1) treeName

newtype Targets =
  Targets (Map.Map TreeName (Set.Set Imports.Import))
  deriving (Show)

instance Semigroup Targets where
  (Targets t1) <> (Targets t2) =
    Targets (Map.unionWith (<>) t1 t2)

newtype DependencyGraph =
  DependencyGraph (Map.Map TreeName Targets)
  deriving (Show)

instance Semigroup DependencyGraph where
  (DependencyGraph g1) <> (DependencyGraph g2) =
    DependencyGraph (Map.unionWith (<>) g1 g2)

instance Monoid DependencyGraph where
  mempty =
    DependencyGraph mempty

data Target =
  Target TreeName (Set.Set Imports.Import)
  deriving (Show, Eq, Ord)

targetTreeName :: Target -> TreeName
targetTreeName (Target treeName _) =
  treeName

targetCauses :: Target -> Set.Set Imports.Import
targetCauses (Target _ causes) =
  causes

nodes :: DependencyGraph -> Set.Set TreeName
nodes (DependencyGraph graph) =
  Set.fromList $ Map.keys graph

dependencyTargets :: TreeName -> DependencyGraph -> [Target]
dependencyTargets name (DependencyGraph graph) =
  targetsToList $
  Map.findWithDefault
    (Targets Map.empty)
    name
    graph

targetsToList :: Targets -> [Target]
targetsToList (Targets targetMap) =
  fmap (uncurry Target) $ Map.toList targetMap


buildDependencyGraph :: Set.Set Imports.Import -> DependencyGraph
buildDependencyGraph =
  foldMap mkImportDependencies

singletonGraph :: TreeName -> TreeName -> Imports.Import -> DependencyGraph
singletonGraph source target cause =
  DependencyGraph $
    Map.singleton source $
      Targets (Map.singleton target (Set.singleton cause))

mkImportDependencies :: Imports.Import -> DependencyGraph
mkImportDependencies imp =
  fromMaybe mempty $ do
    sourceTree <- moduleNameToTreeName (Imports.importSource imp)
    targetTree <- moduleNameToTreeName (Imports.importTarget imp)

    (sourceAncTree, targetAncTree)
      <- earliestDivergentAncestors sourceTree targetTree

    Just $
      singletonGraph sourceAncTree targetAncTree imp

moduleNameParts :: String -> [String]
moduleNameParts moduleName =
  case break (== '.') moduleName of
    (firstPart, '.':rest) ->
      firstPart : moduleNameParts rest

    (onlyPart, _) ->
      [onlyPart]

moduleNameToTreeName :: Syntax.ModuleName a -> Maybe TreeName
moduleNameToTreeName (Syntax.ModuleName _ name) =
  foldr (\x -> Just . TreeName x) Nothing $ moduleNameParts name

earliestDivergentAncestors :: TreeName -> TreeName -> Maybe (TreeName, TreeName)
earliestDivergentAncestors = go
  where
    go (TreeName sourceName _) (TreeName targetName Nothing)
      = Just (TreeName sourceName Nothing, TreeName targetName Nothing) -- this can capture the case where A.B imports A
    go (TreeName sourceName Nothing) (TreeName targetName (Just _))
      | sourceName == targetName = Nothing  -- case where A imports A.B - we don't care
      | otherwise = Just (TreeName sourceName Nothing, TreeName targetName Nothing)
    go (TreeName sourceName (Just nxtSource)) (TreeName targetName (Just nxtTarget))
      | sourceName == targetName = Monad.join bimap (TreeName sourceName . Just)
                               <$> go nxtSource nxtTarget
      | otherwise = Just (TreeName sourceName Nothing, TreeName targetName Nothing)

