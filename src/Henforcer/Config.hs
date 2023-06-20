{- |
Module      : Henforcer.Config
Description :
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : development@flipstone.com
Stability   : Stable
-}
module Henforcer.Config
  ( Config (..)
  , DependencyDeclaration (..)
  , loadConfigFile
  ) where

import Prelude

import qualified Data.Text as T
import qualified Data.Void as Void
import qualified Dhall
import qualified Dhall.Src as DhallSrc
import System.FilePath ((</>))
import qualified System.FilePath as FilePath

import qualified CompatGHC
import qualified Henforcer.HaddockCheck as H
import qualified Henforcer.Import as Import
import qualified Henforcer.TreeName as TreeName

data Config = Config
  { sourcePaths :: [FilePath]
  , dependencyDeclarations :: [DependencyDeclaration]
  , encapsulatedTrees :: [TreeName.TreeName]
  , allowedQualifications :: Import.AllowedSchemes
  , allowedDocumentation :: H.DocumentationRules
  , allowedOpenUnaliasedImports :: Import.AllowedOpenUnaliasedImports
  }

data DependencyDeclaration = DependencyDeclaration
  { moduleTree :: TreeName.TreeName
  , treeDependencies :: [TreeName.TreeName]
  }

loadConfigFile :: FilePath -> IO Config
loadConfigFile configPath = do
  rawConfig <- Dhall.inputFile configDecoder configPath
  pure $
    rawConfig
      { sourcePaths = fmap (relativize configPath) (sourcePaths rawConfig)
      }

relativize :: FilePath -> FilePath -> FilePath
relativize configPath sourcePath =
  FilePath.takeDirectory configPath </> sourcePath

configDecoder :: Dhall.Decoder Config
configDecoder =
  Dhall.record $
    Config
      <$> Dhall.field (T.pack "sourcePaths") (Dhall.list Dhall.string)
      <*> Dhall.field (T.pack "treeDependencies") (Dhall.list dependencyDeclarationDecoder)
      <*> Dhall.field (T.pack "encapsulatedTrees") (Dhall.list treeName)
      <*> Dhall.field (T.pack "allowedQualifications") (Dhall.map CompatGHC.moduleNameDecoder (Dhall.list qualificationScheme))
      <*> Dhall.field (T.pack "documentationRules") H.documentationRulesDecoder
      <*> Dhall.field (T.pack "allowedOpenUnaliasedImports") Import.allowedOpenUnaliasedImportsDecoder

dependencyDeclarationDecoder :: Dhall.Decoder DependencyDeclaration
dependencyDeclarationDecoder =
  Dhall.record $
    DependencyDeclaration
      <$> Dhall.field (T.pack "moduleTree") treeName
      <*> Dhall.field (T.pack "dependencies") (Dhall.list treeName)

qualificationScheme :: Dhall.Decoder Import.Scheme
qualificationScheme =
  Dhall.record $
    Import.Scheme
      <$> Dhall.field (T.pack "qualification") qualification
      <*> Dhall.field (T.pack "alias") alias
      <*> Dhall.field (T.pack "safe") safe
      <*> Dhall.field (T.pack "package") package

qualification :: Dhall.Decoder CompatGHC.ImportDeclQualifiedStyle
qualification =
  Dhall.union $
    (const CompatGHC.QualifiedPre <$> Dhall.constructor (T.pack "QualifiedPre") Dhall.unit)
      <> (const CompatGHC.NotQualified <$> Dhall.constructor (T.pack "Unqualified") Dhall.unit)
      <> (const CompatGHC.QualifiedPost <$> Dhall.constructor (T.pack "QualifiedPost") Dhall.unit)

alias :: Dhall.Decoder Import.Alias
alias =
  Dhall.union $
    (const Import.WithoutAlias <$> Dhall.constructor (T.pack "WithoutAlias") Dhall.unit)
      <> (Import.WithAlias <$> Dhall.constructor (T.pack "WithAlias") CompatGHC.moduleNameDecoder)

safe :: Dhall.Decoder Import.Safe
safe =
  Dhall.union $
    (const Import.WithoutSafe <$> Dhall.constructor (T.pack "WithoutSafe") Dhall.unit)
      <> (const Import.WithSafe <$> Dhall.constructor (T.pack "WithSafe") Dhall.unit)

package :: Dhall.Decoder Import.Package
package =
  Dhall.union $
    (const Import.WithoutPackage <$> Dhall.constructor (T.pack "WithoutPackage") Dhall.unit)
      <> (Import.WithPackage <$> Dhall.constructor (T.pack "WithPackage") Dhall.string)

treeName :: Dhall.Decoder TreeName.TreeName
treeName =
  let extractTreeName :: String -> Dhall.Extractor DhallSrc.Src Void.Void TreeName.TreeName
      extractTreeName input =
        case TreeName.parse input of
          Right validName ->
            pure validName
          Left err ->
            Dhall.extractError (T.pack err)
   in parseDecoder extractTreeName Dhall.string

parseDecoder ::
  (a -> Dhall.Extractor DhallSrc.Src Void.Void b)
  -> Dhall.Decoder a
  -> Dhall.Decoder b
parseDecoder parseB decoderA =
  let extractB expr =
        Dhall.fromMonadic $ do
          a <- Dhall.toMonadic (Dhall.extract decoderA expr)
          Dhall.toMonadic (parseB a)
   in decoderA
        { Dhall.extract = extractB
        }
