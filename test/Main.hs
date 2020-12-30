module Main
  ( main
  ) where

import qualified Control.Exception as Exception
import qualified Test.HUnit as HUnit
import qualified Test.HUnit.Text as HUText
import qualified System.Directory as Dir
import qualified System.IO as IO
import qualified System.Exit as Exit

import qualified Modulint.Main as Modulint
import qualified Modulint.Options as Options

main :: IO ()
main = do
  let
    printToStdoutWithoutProgress =
      HUText.putTextToHandle IO.stdout False

  (counts,_) <- HUText.runTestText printToStdoutWithoutProgress modulintTest

  if
    (HUnit.errors counts + HUnit.failures counts) > 0
  then
    Exit.exitWith (Exit.ExitFailure 1)
  else
    Exit.exitWith Exit.ExitSuccess

modulintTest :: HUnit.Test
modulintTest =
  HUnit.TestCase $ do
    -- Remove just the prelude file so that init can write a new one. This
    -- lets the test verify that the prelude being distributed is compatible
    -- with the test configuration in the example dir
    Dir.removeFile "examples/.modulint/prelude.dhall"

    Modulint.main initExamplesOptions

    assertExitsWith
      Modulint.checksFailed
      (Modulint.main runExampleOptions)

assertExitsWith :: Exit.ExitCode -> IO a -> HUnit.Assertion
assertExitsWith expectedCode action = do
  result <- Exception.try action

  case result of
    Left actualCode ->
      HUnit.assertEqual "Exit code" expectedCode actualCode

    Right _ ->
      HUnit.assertFailure $
        "Expected process to exit with "
        <> show expectedCode
        <> " but it did not explicitly exit!"


initExamplesOptions :: Options.Options
initExamplesOptions =
  Options.Options
    { Options.configPath = "examples/modulint.dhall"
    , Options.initialize = True
    }

runExampleOptions :: Options.Options
runExampleOptions =
  initExamplesOptions
    { Options.initialize = False
    }
