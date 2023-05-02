module Main
  ( main
  ) where

import qualified Config.Initialize as Initialize
import qualified Henforcer.Options as Options

main :: IO ()
main = do
  opts <- Options.parseOptions
  if Options.initialize opts
    then Initialize.initializeConfigPath (Options.configPath opts)
    else putStrLn "The only option currently supported for the executable usage is init."

-- config <- Config.loadConfigFile (Options.configPath opts)
-- runWithConfig config

-- runWithConfig :: Config.Config -> IO ()
-- runWithConfig cfg =
--   GHC.defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
--       GHC.runGhc (Just libdir) $ do
--         dflags <- GHC.getSessionDynFlags
--         GHC.setSessionDynFlags dflags
--         explodedDirs <- GHC.liftIO $ traverseDir isHaskellFile $ Config.sourcePaths cfg
--         newTargets <- traverse GHC.guessTargetForFile explodedDirs
--         GHC.liftIO $ print explodedDirs
--         GHC.setTargets newTargets

--         graph <- GHC.depanal [] True
--         let ic = ImportCheck.newImportChecker cfg
--             docE = Config.allowedDocumentation cfg
--             topoSorted = Maybe.catMaybes $ fmap GHC.moduleGraphNodeModSum . GHC.flattenSCCs $ GHC.topSortModuleGraph False graph Nothing

--         -- GHC.liftIO $ print $ fmap GHC.ms_mod_name summaries
--         GHC.liftIO $ print $ fmap GHC.ms_mod_name topoSorted
--         haddockErrorsOrRuleFailures <- GHC.liftIO $ checkHaddock docE topoSorted

--         importFailures <- traverse (checkImport ic) topoSorted

--         case haddockErrorsOrRuleFailures of
--           Left _ ->
--             -- TODO: Handle errors
--             GHC.liftIO $ print ""
--           Right fails ->
--             GHC.liftIO . handleFailures $ (fails <> join importFailures)

-- checkImport :: ImportCheck.ImportChecker -> GHC.ModSummary -> GHC.Ghc [GHC.SDoc]
-- checkImport ic summary = do
--   parsedMod <- GHC.parseModule summary
--   let
--     hsMod = GHC.unLoc $ GHC.pm_parsed_source parsedMod

--   pure . fmap ImportCheck.toSDoc $ ImportCheck.checkModule ic hsMod

-- checkHaddock :: H.DocumentationRules -> [GHC.ModSummary] -> IO (Either H.HaddockError [GHC.SDoc])
-- checkHaddock docE summaries = do
--   ifaces <- H.silentlyCreateInterfaces' summaries
--   case ifaces of
--     Left err -> do
--       print err
--       pure $ pure []
--     Right [] -> do
--       putStrLn "No interfaces!"
--       pure $ pure []
--     Right xs -> do
--       let
--         errOrDocs = ((fmap GHC.ppr . H.checkInterfaces docE)) xs
--       case errOrDocs of
--         [] -> do
--           putStrLn "No docs"
--           pure $ pure errOrDocs
--         docs -> do
--           putStrLn "Docs"
--           print docs
--           pure $ pure errOrDocs

-- handleFailures :: [GHC.SDoc] -> IO ()
-- handleFailures failures =
--   case length failures of
--     0 -> do
--       putStrLn "No errors found"
--       Exit.exitSuccess
--     errCount -> do
--       putStr $ formatFailures failures
--       putStrLn $ "\nhenforcer: " <> show errCount <> " errors found!"
--       Exit.exitWith checksFailed

-- isHaskellFile :: FilePath -> Bool
-- isHaskellFile filePath =
--   FilePath.takeExtension filePath == ".hs"

-- checksFailed :: Exit.ExitCode
-- checksFailed =
--   Exit.ExitFailure 20

-- formatFailures :: [GHC.SDoc] -> String
-- formatFailures =
--   GHC.renderWithDefaultContext . GHC.vcat

-- traverseDir :: (FilePath -> Bool)
--             -> [FilePath]
--             -> IO [FilePath]
-- traverseDir isFileOfInterest initial =
--   let
--     go accum baseDir = do
--         contents <- Dir.listDirectory baseDir

--         let
--           fullPaths = fmap (FilePath.combine baseDir) contents

--         Monad.foldM foldEntry accum fullPaths

--     foldEntry accum path = do
--         status <- Posix.getFileStatus path

--         if Posix.isDirectory status then
--           go accum path
--         else if Posix.isRegularFile status && isFileOfInterest path then
--           pure $ accum <> [path]
--         else
--           pure accum
--   in
--     fmap join $ traverse (go []) initial
