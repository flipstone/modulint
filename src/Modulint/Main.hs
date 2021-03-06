module Modulint.Main
  ( main
  , checksFailed
  ) where

import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Language.Haskell.Exts.Parser as Parse
import qualified System.Exit as Exit

import qualified Modulint.Config as Config
import qualified Modulint.Check as Check
import qualified Modulint.ErrorMessage as ErrorMessage
import qualified Modulint.Imports as Imports
import qualified Modulint.Initialize as Initialize
import qualified Modulint.ModuleName as ModuleName
import qualified Modulint.Options as Options
import qualified Modulint.Qualification as Qualification
import qualified Modulint.TreeName as TreeName

main :: Options.Options -> IO ()
main options = do
  if
    Options.initialize options
  then
    Initialize.initializeConfigPath (Options.configPath options)
  else
    run options

run :: Options.Options -> IO ()
run options = do
  config <- Config.loadConfigFile (Options.configPath options)

  let
    importChecker = Check.newImportChecker config

  Fold.traverse_
    (printDirectoryImports importChecker)
    (Config.sourcePaths config)

printDirectoryImports :: Check.ImportChecker -> FilePath -> IO ()
printDirectoryImports importChecker directoryPath = do
  result <- Imports.loadSourceTreeImports directoryPath

  case result of
    Parse.ParseFailed srcLoc errMsg -> do
      putStrLn $ concat
        [ "Unable to load imports due to parse failure at "
        , show srcLoc
        , ": "
        , errMsg
        ]
      Exit.exitWith moduleParseFailure

    Parse.ParseOk allImports -> do
      let
        checkFailures =
          Check.checkImports importChecker allImports

        errorOutput =
          List.intercalate
            "\n\n"
            (map (ErrorMessage.format . failureMessage) checkFailures)

      case length checkFailures of
        0 -> do
          putStrLn "modulint: All checks passed!"
          Exit.exitWith Exit.ExitSuccess

        errCount -> do
          putStrLn errorOutput
          putStrLn $ "\nmodulint: " <> show errCount <> " errors found!"
          Exit.exitWith checksFailed

failureMessage :: Check.CheckFailure -> ErrorMessage.Msg
failureMessage failure =
  case failure of
    Check.DependencyViolation imp dep ->
      ErrorMessage.build
        (Imports.srcLocation imp)
        (formatDependencyViolation imp dep)

    Check.EncapsulationViolation imp treeName ->
      ErrorMessage.build
        (Imports.srcLocation imp)
        (formatEncapsulationViolation imp treeName)

    Check.QualificationViolation imp violation ->
      ErrorMessage.build
        (Imports.srcLocation imp)
        (formatQualificationViolation imp violation)

formatDependencyViolation :: Imports.Import -> Check.CheckedDependency -> ErrorMessage.Body
formatDependencyViolation imp dep =
  let
    depSource = Check.dependencySource dep
    depTarget = Check.dependencyTarget dep
  in
    ErrorMessage.line $
      [ formatImportSubject imp
      , "is forbidden by the declaration that the module tree"
      , TreeName.format depSource
      , "depends on"
      , TreeName.format depTarget
      ]

formatEncapsulationViolation :: Imports.Import -> TreeName.TreeName -> ErrorMessage.Body
formatEncapsulationViolation imp treeName =
  ErrorMessage.line
    [ formatImportSubject imp
    , "is forbidden because it is an internal module of the encapsulated tree"
    , TreeName.format treeName
    ]

formatQualificationViolation :: Imports.Import -> [Qualification.Scheme] -> ErrorMessage.Body
formatQualificationViolation imp alloweds =
  ErrorMessage.line
    [ formatImportSubject imp
    , "is improper because it does not match one of the allowed qualification"
    , "schemes. It was imported:"
    ]
  <> ErrorMessage.indent (formatQualificationScheme (Imports.qualification imp))
  <> ErrorMessage.line []
  <> ErrorMessage.line ["But it may only be imported in the followings ways:"]
  <> ErrorMessage.indent (foldMap formatQualificationScheme alloweds)

formatQualificationScheme :: Qualification.Scheme -> ErrorMessage.Body
formatQualificationScheme allowed =
  ErrorMessage.line
    [ case Qualification.qualification allowed of
        Qualification.Qualified -> "qualified"
        Qualification.Unqualified -> "unqualified"

    , case Qualification.alias allowed of
        Qualification.WithoutAlias -> "without alias"
        Qualification.WithAlias name -> "as " <> ModuleName.format name
    ]

formatImportSubject :: Imports.Import -> String
formatImportSubject imp =
  unwords
    [ "The import of"
    , ModuleName.format (Imports.importedModule imp)
    ]

moduleParseFailure :: Exit.ExitCode
moduleParseFailure =
  Exit.ExitFailure 10

checksFailed :: Exit.ExitCode
checksFailed =
  Exit.ExitFailure 20
