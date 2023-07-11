let Ml = ./.henforcer/package.dhall

in  Ml.Config::{
    , sourcePaths = [ "src" ]
    , treeDependencies =
      [ { moduleTree = "PetStore", dependencies = [ "Service" ] } ]
    , encapsulatedTrees = [ "Service.ThirdPartyPetsSite" ]
    , allowedQualifications = toMap
        { Prelude = [ Ml.qualifiedAs "Foo" ]
        , `PetStore.Pet.Model` = [ Ml.qualifiedAs "PetModel", Ml.unqualified ]
        }
    , documentationRules =
            Ml.emptyDocumentationRules
        //  { globalDocumentationAmount = Some
                (Ml.minimumDocumentedAndMaxUndocumented 1 0)
            }
    , allowedOpenUnaliasedImports = Ml.globalMaxAllowedOpenUnaliasedImports 1
    }
