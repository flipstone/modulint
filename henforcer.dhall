let Ml = ./.henforcer/package.dhall

in  Ml.Config::{
    , sourcePaths = [ "src"-- , "app"
    ]
    , treeDependencies =
      [ { moduleTree = "Henforcer", dependencies = [ "Scratch" ] } ]
    , encapsulatedTrees =
      [ "Henforcer.Haddock"
      , "Henforcer.Import"
      , "Henforcer.Import.Scheme"
      , "Henforcer.ImportCheck"
      ]
    , allowedQualifications = toMap
        { Prelude = [ Ml.unqualified ]
        , `Data.Foldable` = [ Ml.qualifiedAs "Fold" ]
        , GHC = [ Ml.qualified ] # [ Ml.qualifiedAs "GHC" ] # Ml.qualifiedEither
        , `GHC.Utils.Outputable` =
            [ Ml.qualified, Ml.qualifiedAs "GHC" ] # Ml.qualifiedEither
        , CompatGHC = Ml.qualifiedEither
        }
    , documentationRules = Ml.globalModuleDocumentationRule (Ml.minimumDocumentedAndMaxUndocumented 1 0)

    , allowedOpenUnaliasedImports =
        Ml.perModuleOpenUnaliasedImports (toMap { `Henforcer.Config` = 2 })
    }
