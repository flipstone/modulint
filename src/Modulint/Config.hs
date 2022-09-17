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

import qualified CompatGHC as GHC
import qualified Modulint.Scheme as Scheme
import qualified Modulint.TreeName as TreeName

data Config =
  Config
    { sourcePaths :: [FilePath]
    , dependencyDeclarations :: [DependencyDeclaration]
    , encapsulatedTrees :: [TreeName.TreeName]
    , allowedQualifications :: Scheme.AllowedSchemes
    }

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
      <$> Dhall.field (T.pack "sourcePaths")           (Dhall.list Dhall.string)
      <*> Dhall.field (T.pack "treeDependencies")      (Dhall.list dependencyDeclarationDecoder)
      <*> Dhall.field (T.pack "encapsulatedTrees")     (Dhall.list treeName)
      <*> Dhall.field (T.pack "allowedQualifications") (Dhall.map moduleName (Dhall.list qualificationScheme))

dependencyDeclarationDecoder :: Dhall.Decoder DependencyDeclaration
dependencyDeclarationDecoder =
  Dhall.record $
    DependencyDeclaration
      <$> Dhall.field (T.pack "moduleTree") treeName
      <*> Dhall.field (T.pack "dependencies") (Dhall.list treeName)

qualificationScheme :: Dhall.Decoder Scheme.Scheme
qualificationScheme =
  Dhall.record $
    Scheme.Scheme
      <$> Dhall.field (T.pack "qualification") qualification
      <*> Dhall.field (T.pack "alias") alias
      <*> Dhall.field (T.pack "safe") safe
      <*> Dhall.field (T.pack "package") package

qualification :: Dhall.Decoder GHC.ImportDeclQualifiedStyle
qualification =
  Dhall.union $
       (const GHC.QualifiedPre <$> Dhall.constructor (T.pack "QualifiedPre")  Dhall.unit)
    <> (const GHC.NotQualified <$> Dhall.constructor (T.pack "Unqualified")   Dhall.unit)
    <> (const GHC.QualifiedPost <$> Dhall.constructor (T.pack "QualifiedPost") Dhall.unit)

alias :: Dhall.Decoder Scheme.Alias
alias =
  Dhall.union $
       (const Scheme.WithoutAlias <$> Dhall.constructor (T.pack "WithoutAlias")  Dhall.unit)
    <> (Scheme.WithAlias          <$> Dhall.constructor (T.pack "WithAlias")     moduleName)

safe :: Dhall.Decoder Scheme.Safe
safe =
  Dhall.union $
       (const Scheme.WithoutSafe <$> Dhall.constructor (T.pack "WithoutSafe") Dhall.unit)
    <> (const Scheme.WithSafe <$> Dhall.constructor (T.pack "WithSafe") Dhall.unit)

package :: Dhall.Decoder Scheme.Package
package =
  Dhall.union $
       (const Scheme.WithoutPackage <$> Dhall.constructor (T.pack "WithoutPackage") Dhall.unit)
    <> (Scheme.WithPackage <$> Dhall.constructor (T.pack "WithPackage") Dhall.string)


moduleName :: Dhall.Decoder GHC.ModuleName
moduleName =
  fmap GHC.mkModuleName Dhall.string

treeName :: Dhall.Decoder TreeName.TreeName
treeName =
  let
    extractTreeName :: String -> Dhall.Extractor DhallSrc.Src Void.Void TreeName.TreeName
    extractTreeName input =
      case TreeName.parse input of
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
