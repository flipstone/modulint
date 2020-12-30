module Modulint.Config
  ( Config(..)
  , DependencyDeclaration(..)
  , loadConfigFile
  ) where

import qualified Data.Text as T
import qualified Data.Void as Void
import qualified Dhall
import qualified Dhall.Src as DhallSrc
import           System.FilePath ((</>))
import qualified System.FilePath as FilePath

import qualified Modulint.TreeName as TreeName

data Config =
  Config
    { sourcePaths :: [FilePath]
    , dependencyDeclarations :: [DependencyDeclaration]
    , encapsulatedTrees :: [TreeName.TreeName]
    } deriving (Show)

data DependencyDeclaration =
  DependencyDeclaration
    { moduleTree :: TreeName.TreeName
    , treeDependencies :: [TreeName.TreeName]
    } deriving (Show)

loadConfigFile :: FilePath -> IO Config
loadConfigFile configPath = do
  rawConfig <- Dhall.inputFile configDecoder configPath
  pure $
    rawConfig
      { sourcePaths = map (relativize configPath) (sourcePaths rawConfig)
      }

relativize :: FilePath -> FilePath -> FilePath
relativize configPath sourcePath =
  FilePath.takeDirectory configPath </> sourcePath

configDecoder :: Dhall.Decoder Config
configDecoder =
  Dhall.record $
    Config
      <$> Dhall.field (T.pack "sourcePaths")        (Dhall.list Dhall.string)
      <*> Dhall.field (T.pack "treeDependencies")   (Dhall.list dependencyDeclarationDecoder)
      <*> Dhall.field (T.pack "encapsulatedTrees")  (Dhall.list treeName)

dependencyDeclarationDecoder :: Dhall.Decoder DependencyDeclaration
dependencyDeclarationDecoder =
  Dhall.record $
    DependencyDeclaration
      <$> Dhall.field (T.pack "moduleTree") treeName
      <*> Dhall.field (T.pack "dependencies") (Dhall.list treeName)

treeName :: Dhall.Decoder TreeName.TreeName
treeName =
  let
    extractTreeName input =
      case TreeName.parseTreeName input of
        Right validName ->
          pure validName

        Left err ->
          Dhall.extractError (T.pack err)
  in
    parseDecoder extractTreeName Dhall.string

parseDecoder :: (a -> Dhall.Extractor DhallSrc.Src Void.Void b)
             -> Dhall.Decoder a
             -> Dhall.Decoder b
parseDecoder parseB decoderA =
  let
    extractB expr =
      Dhall.fromMonadic $ do
        a <- Dhall.toMonadic (Dhall.extract decoderA expr)
        Dhall.toMonadic (parseB a)

  in
    decoderA
      { Dhall.extract = extractB
      }

