module App.Bootstrap where

import Prelude

import App.Constant (dbPrefix, defaultModelUsers)
import App.Models (DBKey(..), dbOps)
import App.Route (routers)
import App.Schema (parseModelExpenses, parseModelMessages, parseModelMeta, parseModelUsers)
import App.Types (Env(..), ApplicationConfig)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Romi.Db (dbCreate)
import Romi.Logger (Logger, LoggerLevel(..), createLogger, info, record)
import Romi.Logger.Transport.Console (ConsoleTransport(..))
import Romi.Server (createStaticHandler, createServer, listen)
import Utils (currentDir, log', pathJoin, readDhallFile)

envInit :: ApplicationConfig -> Aff Env
envInit config = do
  db <- dbCreate (pathJoin currentDir config.dataDir) dbPrefix
  let env = Env { db, logger, user: Nothing }
  _ <- runReaderT (runExceptT (do
    dbOps.putOrIf Users defaultModelUsers $ isLeft <<< parseModelUsers
    dbOps.putOrIf Messages "[]" $ isLeft <<< parseModelMessages
    dbOps.putOrIf Expenses "[]" $ isLeft <<< parseModelExpenses
    dbOps.putOrIf Meta { webUrl: "http://localhost:" <> show config.port
                       , webName: config.defaultName
                       , webTitle: config.defaultTitle
                       , webNotice: config.defaultNotice
                       , webStartTime: config.defaultStartTime
                       } $ isLeft <<< parseModelMeta
    pure unit
  )) env
  pure env
  where
    logger :: Logger Aff
    logger = createLogger
        { level: Debug
        , labels: []
        , transports:
            [ ConsoleTransport
                { useColor: true
                , labelTemplate: "[<cyan>{name}</cyan>]"
                , timeFormat: "YY/M/D H:m:s"
                , template: "<blue>{time}</blue> {level} (<bold>{pid}</bold>) {labels}: {msg}"
                }
            ]
        }

bootstrap :: Aff Unit
bootstrap = do
  config <- liftEffect $ readDhallFile "sena.dhall"
  case config of
    Left err -> log' $ "Error reading config: " <> show err
    Right config' -> do
      env <- envInit config'
      let Env ({ logger }) = env
      info logger $ "<bold><magenta>Garden Mansion</magenta> - <cyan>By Arimura Sena</cyan></bold>"
      info logger $ "<bold><red>Open source at</red> : <blue>https://github.com/biyuehu/gardenmansion</blue> </bold>"
      info logger $ "<bold><yellow>星奏は永遠に、ただ一人の推し</yellow></bold>"
      record logger $ "Static directory: " <> config'.staticDir
      record logger $ "Database directory: " <> config'.dataDir
      info logger $ "Starting server..."
      server <- createServer routers env $ Just (\reqPrim -> \resPrim -> createStaticHandler "static" reqPrim resPrim)
      liftEffect $ listen server config'.port $ launchAff_ $ record logger $ "Server is running on <yellow>http://localhost:" <> show config'.port <> "</yellow>"

