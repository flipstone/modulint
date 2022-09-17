module Modulint
  ( plugin
  ) where

import qualified CompatGHC
import qualified Modulint.Check as Check
import qualified Modulint.Config as Config
import qualified Modulint.Options as Options

plugin :: CompatGHC.Plugin
plugin =
  CompatGHC.defaultPlugin
    { CompatGHC.parsedResultAction = parsedResultAction
    , CompatGHC.pluginRecompile = CompatGHC.purePlugin
    }

parsedResultAction :: [CompatGHC.CommandLineOption] -> modSummary -> CompatGHC.PluginResult -> CompatGHC.Hsc CompatGHC.PluginResult
parsedResultAction commandLineOpts _modSummary parsedMod = do
  opts <- CompatGHC.liftIO $ Options.parseGivenOptions commandLineOpts
  config <- CompatGHC.liftIO $ Config.loadConfigFile (Options.configPath opts)

  let
    ic = Check.newImportChecker config
  case Check.checkModule ic (pure $ CompatGHC.getHsModule parsedMod) of
    Left _ -> -- Since this is run as a plugin, we won't even get here if the module couldn't parse
      pure parsedMod
    Right cfs -> do
      CompatGHC.printMsgs $ Check.errorMessagesFromList cfs
      pure parsedMod
