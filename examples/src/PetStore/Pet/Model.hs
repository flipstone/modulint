{- |
Module      : PetStore.Pet.Model
Description : An example module
Copyright   : (c) Flipstone, 2022
License     : BSD-3-clause
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module PetStore.Pet.Model where

import System.IO.Unsafe (unsafePerformIO)
import qualified UnliftIO
import qualified Prelude

-- | This is some haddock
foo :: Prelude.IO ()
foo = unsafePerformIO Prelude.undefined

-- Note this is not haddock
bar :: Prelude.IO ()
bar = Prelude.undefined