{- |
Module      : Henforcer.Haddock
Description : Wrappers around Haddock functionality and helpers for ease of use.
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : development@flipstone.com
-}
module Henforcer.Haddock
  ( tryCreateInterfaces
  , HaddockError
  ) where

import Henforcer.Haddock.Interface (HaddockError, tryCreateInterfaces)
