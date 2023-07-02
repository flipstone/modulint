{- |
Module      : Henforcer
Description :
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : development@flipstone.com
-}
module Henforcer
  ( plugin
  ) where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Monad as Monad
import qualified System.IO.Unsafe as UnsafeIO

import qualified CompatGHC
import qualified Henforcer.Config as Config
import qualified Henforcer.Haddock as Haddock
import qualified Henforcer.HaddockCheck as HaddockCheck
import qualified Henforcer.ImportCheck as ImportCheck
import qualified Henforcer.Options as Options

-- Using an MVar and unsafePerformIO here is a very ugly hack, but the plugin interface gives us no
-- way to load the configuration once for the entire set of modules being compiled. This is a big
-- enough performance win that the cost seems likely worth it otherwise.
globalConfigState :: MVar.MVar Config.Config
{-# NOINLINE globalConfigState #-}
globalConfigState = UnsafeIO.unsafePerformIO MVar.newEmptyMVar

plugin :: CompatGHC.Plugin
plugin =
  CompatGHC.defaultPlugin
    { CompatGHC.pluginRecompile = CompatGHC.purePlugin
    , CompatGHC.typeCheckResultAction = typeCheckResultAction
    }

typeCheckResultAction ::
  [CompatGHC.CommandLineOption]
  -> CompatGHC.ModSummary
  -> CompatGHC.TcGblEnv
  -> CompatGHC.TcM CompatGHC.TcGblEnv
typeCheckResultAction commandLineOpts modSummary tcGblEnv = do
  config <- CompatGHC.liftIO $ loadConfigIfNeeded commandLineOpts

  let ic = ImportCheck.newImportChecker config
      docE = Config.allowedDocumentation config
      name = CompatGHC.moduleName $ CompatGHC.ms_mod modSummary


  Monad.when (HaddockCheck.documentationRuleAppliesToModule docE name) $ do
    eitherErrOrIfaces <- CompatGHC.liftIO $ Haddock.tryCreateInterfaces modSummary
    case eitherErrOrIfaces of
      Left err -> CompatGHC.addMessages . CompatGHC.mkErrorMsgsWithGeneratedSrcSpan $ [err]
      Right ifaces ->
        CompatGHC.addMessages . CompatGHC.mkErrorMsgsWithGeneratedSrcSpan $
          HaddockCheck.checkOnlyGivenModule docE name ifaces

  CompatGHC.addMessages
        . ImportCheck.errorMessagesFromList
        $ ImportCheck.checkModule ic tcGblEnv

  pure tcGblEnv

loadConfigIfNeeded :: [CompatGHC.CommandLineOption] -> IO Config.Config
loadConfigIfNeeded commandLineOpts = do
  mbConfig <- MVar.tryReadMVar globalConfigState
  case mbConfig of
    Just c -> pure c
    Nothing -> do
      opts <- Options.parseGivenOptions commandLineOpts
      config <- Config.loadConfigFile (Options.configPath opts)
      -- If we've been beaten to filling the global config, then oh well, but we do not want to
      -- block on it.
      _ <- MVar.tryPutMVar globalConfigState config
      pure config
