{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      : Henforcer.Import.MaxOpenUnaliasedImports
Description :
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : development@flipstone.com
Stability   : experimental
Portability : POSIX
-}
module Henforcer.Import.MaxOpenUnaliasedImports
  ( AllowedOpenUnaliasedImports (..)
  , MaxOpenUnaliasedImportsNat
  , allowedOpenUnaliasedImportsDecoder
  , showMaxOpenUnaliasedImportsNat
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Dhall
import qualified Numeric.Natural as Nat

import qualified CompatGHC

data AllowedOpenUnaliasedImports
  = GlobalAllowedOpenUnaliasedImports MaxOpenUnaliasedImportsNat
  | PerModuleOpenUnaliasedImports (M.Map CompatGHC.ModuleName MaxOpenUnaliasedImportsNat)

allowedOpenUnaliasedImportsDecoder :: Dhall.Decoder AllowedOpenUnaliasedImports
allowedOpenUnaliasedImportsDecoder =
  Dhall.union $
    fmap
      GlobalAllowedOpenUnaliasedImports
      (Dhall.constructor (T.pack "GlobalAllowedOpenUnaliasedImports") maxOpenUnaliasedImportNatDecoder)
      <> fmap
        PerModuleOpenUnaliasedImports
        ( Dhall.constructor
            (T.pack "PerModuleOpenUnaliasedImports")
            (Dhall.map moduleName maxOpenUnaliasedImportNatDecoder)
        )

-- | A wrapper around 'Nat.Natural' for clarity
newtype MaxOpenUnaliasedImportsNat = MaxOpenUnaliasedImportsNat Nat.Natural
  deriving (Num, Eq, Ord, Real, Enum, Integral)

showMaxOpenUnaliasedImportsNat :: MaxOpenUnaliasedImportsNat -> String
showMaxOpenUnaliasedImportsNat (MaxOpenUnaliasedImportsNat n) = show n

maxOpenUnaliasedImportNatDecoder :: Dhall.Decoder MaxOpenUnaliasedImportsNat
maxOpenUnaliasedImportNatDecoder =
  fmap MaxOpenUnaliasedImportsNat Dhall.natural

-- TODO combine this with other definition
moduleName :: Dhall.Decoder CompatGHC.ModuleName
moduleName =
  fmap CompatGHC.mkModuleName Dhall.string
