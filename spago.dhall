{ name =
    "deques"
, dependencies =
    [ "lists"
    , "maybe"
    , "psci-support"
    , "spec"
    , "spec-discovery"
    , "spec-quickcheck"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
