module Main
  ( main
  ) where

import qualified System.Exit as Exit
import qualified System.FilePath as FilePath

import qualified CompatGHC as GHC
import qualified Config.Initialize as Initialize
import qualified Modulint.Check as Check
import qualified Modulint.Config as Config
import qualified Modulint.Directory as Dir
import qualified Modulint.Options as Options

main :: IO ()
main = do
  opts <- Options.parseOptions
  if Options.initialize opts
    then Initialize.initializeConfigPath (Options.configPath opts)
    else runChecks opts

runChecks :: Options.Options -> IO ()
runChecks opts = do
  res <- runWithOpts opts
  print (fst res)
  case (length (snd res), length (snd res)) of
    (0,0) -> do
      putStrLn "modulint: All checks passed!"
      Exit.exitSuccess

    (errCount,0) -> do
      putStr $ formatCheckFailures (snd res)
      putStrLn $ "\nmodulint: " <> show errCount <> " errors found!"
      Exit.exitWith checksFailed

    (0, _parseErrCount) -> do
      -- FIXME format parse failure messages
      print $ fst res

      Exit.exitWith moduleParseFailure

    (checkErrCount,_parseErrCount) -> do
      putStrLn $ formatCheckFailures (snd res)
      putStrLn $ "\nmodulint: " <> show checkErrCount <> " errors found!"

      -- FIXME format parse failure messages
      print $ fst res

      Exit.exitWith moduleParseAndChecksFailed

runWithOpts :: Options.Options -> IO ([String],[Check.CheckFailure])
runWithOpts opts = do
  config <- Config.loadConfigFile (Options.configPath opts)
  flags <- GHC.initDynFlags (GHC.defaultDynFlags GHC.fakeSettings GHC.fakeLlvmConfig)
  foldSrcPaths flags (Check.newImportChecker config) (Config.sourcePaths config)

foldSrcPaths :: Traversable t =>
                GHC.DynFlags ->
                Check.ImportChecker ->
                t FilePath ->
                IO ([String],[Check.CheckFailure])
foldSrcPaths flags checker =
  foldMap (foldSrcPath flags checker)

foldSrcPath :: GHC.DynFlags
            -> Check.ImportChecker
            -> FilePath
            -> IO ([String],[Check.CheckFailure])
foldSrcPath flags checker =
  Dir.foldDirectory isHaskellFile (checkFileMerge flags checker) mempty

isHaskellFile :: FilePath -> Bool
isHaskellFile filePath =
  FilePath.takeExtension filePath == ".hs"

moduleParseFailure :: Exit.ExitCode
moduleParseFailure =
  Exit.ExitFailure 10

checksFailed :: Exit.ExitCode
checksFailed =
  Exit.ExitFailure 20

moduleParseAndChecksFailed :: Exit.ExitCode
moduleParseAndChecksFailed =
  Exit.ExitFailure 30

formatCheckFailures :: [Check.CheckFailure] -> String
formatCheckFailures =
  GHC.renderWithDefaultContext . GHC.vcat . fmap Check.toSDoc

checkFileMerge :: GHC.DynFlags -> Check.ImportChecker -> ([String],[Check.CheckFailure]) -> FilePath -> IO ([String],[Check.CheckFailure])
checkFileMerge flags importChecker errsAndFailures fileName = do
  checked <- parseAndCheckFile flags importChecker fileName
  pure $ case checked of
    Left parseFailure -> errsAndFailures <> ([parseFailure], mempty)
    Right x -> errsAndFailures <> (mempty, x)

parseAndCheckFile :: GHC.DynFlags -> Check.ImportChecker -> FilePath -> IO (Either String [Check.CheckFailure])
parseAndCheckFile flags importChecker =
  fmap (Check.checkModule importChecker) . GHC.runParser flags
