{- |
Module      : Henforcer.Import.Scheme.Safe
Description :
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : development@flipstone.com
Stability   : experimental
Portability : POSIX
-}
module Henforcer.Import.Scheme.Safe
  ( Safe (..)
  , determineSafe
  ) where

import qualified CompatGHC

data Safe
  = WithSafe
  | WithoutSafe
  deriving (Eq)

determineSafe :: CompatGHC.ImportDecl pass -> Safe
determineSafe imp =
  if CompatGHC.ideclSafe imp
    then WithSafe
    else WithoutSafe
