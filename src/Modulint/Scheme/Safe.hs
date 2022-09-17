module Modulint.Scheme.Safe
  ( Safe(..)
  , determineSafe
  ) where

import qualified CompatGHC as GHC

data Safe
  = WithSafe
  | WithoutSafe
  deriving (Eq)

determineSafe :: GHC.ImportDecl pass -> Safe
determineSafe imp =
  if GHC.ideclSafe imp
  then WithSafe
  else WithoutSafe
