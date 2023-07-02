{- |
Module      : Henforcer.Haddock.Interface
Description : Creating Haddock Interfaces in a more ergonomic way, with some enumerated errors.
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : development@flipstone.com
-}
module Henforcer.Haddock.Interface
  ( tryCreateInterfaces
  , HaddockError
  ) where

import qualified Documentation.Haddock as Haddock
import qualified System.IO as SysIO
import qualified System.IO.Silently as Silently
import qualified UnliftIO

import qualified CompatGHC

{- | Conditionally creates a list of 'Haddock.Interface's, silencing the normal Haddock output, so
 we do not generate a ton of noise.
-}
tryCreateInterfaces :: CompatGHC.ModSummary -> IO (Either HaddockError [Haddock.Interface])
tryCreateInterfaces moduleSummary =
  case CompatGHC.ml_hs_file $ CompatGHC.ms_location moduleSummary of
    Nothing ->
      pure . Left . UnableToGetFileFromSummary . CompatGHC.moduleName $ CompatGHC.ms_mod moduleSummary
    Just f -> do
      ifaces <- silentlyCreate [f]
      pure $ case ifaces of
        Left _ -> pure []
        Right [] -> Left . NoHaddockInterfacesCreatedFor . CompatGHC.moduleName $ CompatGHC.ms_mod moduleSummary
        Right nonEmptyIfaces -> Right nonEmptyIfaces

{- | silentlyCreate will create the Haddock interfaces for a given set of files. This uses 'try' to
 handle exceptions, so that creating interfaces does not cause a ghc panic because we do not
 attempt to load any dependencies. Output is to stdout for the operation is silenced as
 creating the interface would print for every import of a dependency otherwise.
-}
silentlyCreate :: [String] -> IO (Either UnliftIO.SomeException [Haddock.Interface])
silentlyCreate =
  Silently.silence . UnliftIO.try . Haddock.createInterfaces []

-- | The various errors we can encounter processing haddock information.
data HaddockError
  = UnableToGetFileFromSummary CompatGHC.ModuleName
  | NoHaddockInterfacesCreatedFor CompatGHC.ModuleName
  | NoInterfacesCreatedAtAll

-- | Make sure that we can show the haddock errors in with other ghc output.
instance CompatGHC.Outputable HaddockError where
  ppr haddockError =
    CompatGHC.sep
      [ toSdoc haddockError
      , CompatGHC.blankLine
      ]

{- | The only part of the 'CompatGHC.Diagnostic' class that we really care about is the
 'diagnosticMessage', used for printing.
-}
instance CompatGHC.Diagnostic HaddockError where
  diagnosticMessage = CompatGHC.mkSimpleDecorated . CompatGHC.ppr
  diagnosticReason = const CompatGHC.ErrorWithoutFlag
  diagnosticHints = const []

toSdoc :: HaddockError -> CompatGHC.SDoc
toSdoc (UnableToGetFileFromSummary n) =
  CompatGHC.hsep
    [ CompatGHC.text "Error processing haddocks for module"
    , CompatGHC.text (CompatGHC.moduleNameString n)
    , CompatGHC.text "; unable to get a source file from module summary"
    ]
toSdoc (NoHaddockInterfacesCreatedFor n) =
  CompatGHC.hsep
    [ CompatGHC.text "Error processing haddocks for module"
    , CompatGHC.text (CompatGHC.moduleNameString n)
    , CompatGHC.text "; failure from haddock in creation of interface files"
    ]
toSdoc NoInterfacesCreatedAtAll =
  CompatGHC.hsep
    [ CompatGHC.text "No interfaces created at all"
    ]
