let
    -- An inlined version of List/map to allow running with no network connection
    map
    : forall (a : Type) -> forall (b : Type) -> (a -> b) -> List a -> List b
    = \(a : Type) ->
      \(b : Type) ->
      \(f : a -> b) ->
      \(xs : List a) ->
        List/build
          b
          ( \(list : Type) ->
            \(cons : b -> list -> list) ->
              List/fold a xs list (\(x : a) -> cons (f x))
          )

let
    -- A ModuleName is a dot-delimited module name corresponding to a
    -- Haskell module. E.G. "Control.Monad" ModuleNames cannot be the
    -- empty string.
    ModuleName =
      Text

let
    -- A TreeName is a dot-delimited module-like name corresponding to a
    -- location in the directory tree. E.G. "Control.Monad" corresponds to
    -- all modules beginning with "Control.Monad.", as well as "Control.Monad"
    -- itself. Textually it appears the same as a ModuleName, but is used to
    -- denote the tree rather than an individual module. TreeNames cannot abe
    -- the empty string.
    TreeName =
      Text

let
    -- A Dependency declares that one subtree of the project depends on
    -- other subtrees. This forbids any modules in the subtrees listed in
    -- `dependencies` from importing any module contained within the subtree
    -- indicated by `moduleTree`
    Dependency =
      { moduleTree : TreeName, dependencies : List TreeName }

let
    -- Indicates whether an allowed qualification is quailfied pre, qualified post, or unqualified
    Qualification =
      < QualifiedPre | QualifiedPost | Unqualified >

let
    -- Indicates whether an allowed qualification has an alias, and if so what
    -- the alias must be.
    Alias =
      < WithAlias : ModuleName | WithoutAlias >

let
    -- Indicates whethter an allowed import is only imported if it can be done so safely.
    Safe =
      < WithSafe | WithoutSafe >

let
    -- Indicates whether an allowed import is a package import and if so what the package must be.
    Package =
      < WithPackage : Text | WithoutPackage >

let BothDocumentationRule =
      { maximumUndocumented : Natural, minimumDocumented : Natural }

let
    -- Indicates either the most haddockable items that can be undocumented or the smallest number
    -- of said items that must be documented.
    DocumentationRule =
      < MaximumUndocumented : Natural
      | MinimumDocumented : Natural
      | Both : BothDocumentationRule
      >

let
    -- Describes an allowed qualification scheme for a module when it is imported.
    -- When allowed qualifications are declared for a module, any import of that
    -- module must match one of the `AllowedQualifications` given for it in the
    -- configuration.
    AllowedQualification =
      { qualification : Qualification
      , alias : Alias
      , safe : Safe
      , package : Package
      }

let
    -- A key-value list of 'ModuleName' and numbers that represent the number of imports that are
    -- allowed to be imported completely open with no alias.
    OpenUnaliasedImportMap =
      List { mapKey : ModuleName, mapValue : Natural }

let
    -- Allows for either a global value of open and unaliased imports or determining those on a per
    -- module basis.
    AllowedOpenUnaliasedImports =
      < GlobalAllowedOpenUnaliasedImports : Natural
      | PerModuleOpenUnaliasedImports : OpenUnaliasedImportMap
      >

let globalMaxAllowedOpenUnaliasedImports =
      \(num : Natural) ->
        AllowedOpenUnaliasedImports.GlobalAllowedOpenUnaliasedImports num

let
    -- Build an `AllowedQualification` for unqualified imports without an alias
    unqualified =
      { qualification = Qualification.Unqualified
      , alias = Alias.WithoutAlias
      , safe = Safe.WithoutSafe
      , package = Package.WithoutPackage
      }

let
    -- Build an `AllowedQualification` for unqualified imports that must have the
    -- provided alias
    unqualifiedAs =
      \(aliasName : Text) ->
        { qualification = Qualification.Unqualified
        , alias = Alias.WithAlias aliasName
        , safe = Safe.WithoutSafe
        , package = Package.WithoutPackage
        }

let
    -- Build an `AllowedQualification` for qualified imports without an alias
    qualified =
      { qualification = Qualification.QualifiedPre
      , alias = Alias.WithoutAlias
      , safe = Safe.WithoutSafe
      , package = Package.WithoutPackage
      }

let
    -- Build an `AllowedQualification` for qualified imports that must have the
    -- provided alias
    qualifiedAs =
      \(aliasName : Text) ->
        { qualification = Qualification.QualifiedPre
        , alias = Alias.WithAlias aliasName
        , safe = Safe.WithoutSafe
        , package = Package.WithoutPackage
        }

let
    -- Build an `AllowedQualification` for qualified post imports without an alias
    qualifiedPost =
      { qualification = Qualification.QualifiedPost
      , alias = Alias.WithoutAlias
      , safe = Safe.WithoutSafe
      , package = Package.WithoutPackage
      }

let
    -- Build an `AllowedQualification` for qualified post imports that must have the
    -- provided alias
    qualifiedPostAs =
      \(aliasName : Text) ->
        { qualification = Qualification.QualifiedPost
        , alias = Alias.WithAlias aliasName
        , safe = Safe.WithoutSafe
        , package = Package.WithoutPackage
        }

let
    -- Build a list `AllowedQualification` for qualified pre or qualified post imports without
    -- an alias
    qualifiedEither =
      [ { qualification = Qualification.QualifiedPre
        , alias = Alias.WithoutAlias
        , safe = Safe.WithoutSafe
        , package = Package.WithoutPackage
        }
      , { qualification = Qualification.QualifiedPost
        , alias = Alias.WithoutAlias
        , safe = Safe.WithoutSafe
        , package = Package.WithoutPackage
        }
      ]

let
    -- Build a list of `AllowedQualification` for qualified pre or qualified post imports that
    -- must have the provided alias
    qualifiedEitherAs =
      \(aliasName : Text) ->
        [ { qualification = Qualification.QualifiedPre
          , alias = Alias.WithAlias aliasName
          , safe = Safe.WithoutSafe
          , package = Package.WithoutPackage
          }
        , { qualification = Qualification.QualifiedPost
          , alias = Alias.WithAlias aliasName
          , safe = Safe.WithoutSafe
          , package = Package.WithoutPackage
          }
        ]

let
    -- Mark an import as requiring safe
    setWithSafe =
      \(import : AllowedQualification) -> import // { safe = Safe.WithSafe }

let
    -- Modify a list of `AllowedQualification` to only allow safe imports
    onlySafe =
      \(imports : List AllowedQualification) ->
        map AllowedQualification AllowedQualification setWithSafe imports

let
    -- Mark an import as being a package import and what the package is.
    setWithPackage =
      \(packageName : Text) ->
      \(import : AllowedQualification) ->
        import // { package = Package.WithPackage packageName }

let
    -- Modify a list of `AllowedQualification` to only allow package imports
    onlyPackage =
      \(packageName : Text) ->
      \(imports : List AllowedQualification) ->
        map
          AllowedQualification
          AllowedQualification
          (setWithPackage packageName)
          imports

let
    -- Set minimum documented rule, to make writing configs more ergonomic
    minimumDocumentedRule =
      \(num : Natural) -> DocumentationRule.MinimumDocumented num

let
    -- Set maximum undocumented rule, to make writing configs more ergonomic
    maximumUndocumentedRule =
      \(num : Natural) -> DocumentationRule.MaximumUndocumented num

let minimumDocumentedAndMaxUndocumented =
      \(min : Natural) ->
      \(max : Natural) ->
        DocumentationRule.Both
          { maximumUndocumented = max, minimumDocumented = min }

let AllowedQualificationMap =
      List { mapKey : ModuleName, mapValue : List AllowedQualification }

let perModuleOpenUnaliasedImports =
      AllowedOpenUnaliasedImports.PerModuleOpenUnaliasedImports

let PerModuleDocumentationRules =
      List { mapKey : ModuleName, mapValue : DocumentationRule }

let ModuleHeaderRule =
      { mustContainDescription : Bool
      , mustContainCopyright : Bool
      , mustContainLicense : Bool
      , mustContainMaintainer : Bool
      , mustContainStability : Bool
      , mustContainPortablity : Bool
      , mustContainSafety : Bool
      , mustContainLanguage : Bool
      , mustContainExtensions : Bool
      }

let ModuleHeaderRules =
      List { mapKey : ModuleName, mapValue : ModuleHeaderRule }

let DocumentationRules =
      { globalDocumentationAmount : Optional DocumentationRule
      , perModuleDocumentationAmounts : PerModuleDocumentationRules
      , moduleHeaderRules : ModuleHeaderRules
      }

let emptyDocumentationRules =
      { globalDocumentationAmount = None
      , perModuleDocumentationAmounts = toMap {=} : PerModuleDocumentationRules
      , moduleHeaderRules = toMap {=} : ModuleHeaderRules
      }

let Config =
      { -- Paths that henforcer should search for Haskell files when run
        -- You most likely want to override this.
        sourcePaths : List Text
      , treeDependencies : List Dependency
      , encapsulatedTrees : List TreeName
      , allowedQualifications : AllowedQualificationMap
      , allowedOpenUnaliasedImports : AllowedOpenUnaliasedImports
      , documentationRules : DocumentationRules
      }

in  { Config =
      { Type = Config
      , default =
        { treeDependencies = [] : List Dependency
        , encapsulatedTrees = [] : List TreeName
        , allowedQualifications = toMap {=} : AllowedQualificationMap
        , documentationRules = emptyDocumentationRules
        , allowedOpenUnaliasedImports =
            AllowedOpenUnaliasedImports.PerModuleOpenUnaliasedImports
              (toMap {=} : OpenUnaliasedImportMap)
        }
      }
    , Qualification
    , Alias
    , AllowedQualification
    , AllowedQualificationMap
    , unqualified
    , unqualifiedAs
    , qualified
    , qualifiedAs
    , qualifiedPost
    , qualifiedPostAs
    , qualifiedEither
    , qualifiedEitherAs
    , onlySafe
    , onlyPackage
    , Dependency
    , TreeName
    , ModuleName
    , DocumentationRule
    , minimumDocumentedRule
    , maximumUndocumentedRule
    , minimumDocumentedAndMaxUndocumented
    , globalMaxAllowedOpenUnaliasedImports
    , perModuleOpenUnaliasedImports
    , DocumentationRules
    , emptyDocumentationRules
    , BothDocumentationRule
    , ModuleHeaderRules
    , PerModuleDocumentationRules
    }
