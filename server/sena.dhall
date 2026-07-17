let Config =
  { port : Natural
  , defaultName : Text
  , defaultTitle : Text
  , defaultNotice : Text
  , defaultStartTime : Double
  , staticDir : Text
  , dataDir : Text
  }

let defaultConfig =
  { port = 8080
  , defaultName = "Garden Mansion"
  , defaultTitle = "Functional Co-living Management System"
  , defaultNotice = "Welcome to our shared flat!"
  , defaultStartTime = 1577836800000.0
  , staticDir = "static"
  , dataDir = "db"
  }

let withOverrides =
  \(overrides : { port : Optional Natural
        , defaultName : Optional Text
        , defaultTitle : Optional Text
        , defaultNotice : Optional Text
        , defaultStartTime : Optional Double
        , staticDir : Optional Text
        , dataDir : Optional Text
        }) ->
    defaultConfig
    // { port = merge { Some = \(x : Natural) -> x, None = defaultConfig.port } overrides.port
       , defaultName = merge { Some = \(x : Text) -> x, None = defaultConfig.defaultName } overrides.defaultName
       , defaultTitle = merge { Some = \(x : Text) -> x, None = defaultConfig.defaultTitle } overrides.defaultTitle
       , defaultNotice = merge { Some = \(x : Text) -> x, None = defaultConfig.defaultNotice } overrides.defaultNotice
       , defaultStartTime = merge { Some = \(x : Double) -> x, None = defaultConfig.defaultStartTime } overrides.defaultStartTime
       , staticDir = merge { Some = \(x : Text) -> x, None = defaultConfig.staticDir } overrides.staticDir
       , dataDir = merge { Some = \(x : Text) -> x, None = defaultConfig.dataDir } overrides.dataDir
       }

in withOverrides
  { port = None Natural
  , defaultName = Some "花园公馆"
  , defaultTitle = Some "花园公馆"
  , defaultNotice = Some "Here is notice for you!"
  , defaultStartTime = None Double
  , staticDir = None Text
  , dataDir = None Text
  }
