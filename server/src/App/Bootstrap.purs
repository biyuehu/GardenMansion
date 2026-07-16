module App.Bootstrap where

import Prelude

import App.Constant (dbDirectory, dbPrefix, defaultModelMeta, defaultModelUsers, defaultServerPort)
import App.Models (DBKey(..), dbOps)
import App.Route (routers)
import App.Schema (parseModelExpenses, parseModelMessages, parseModelMeta, parseModelUsers)
import App.Types (Env(..))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Either (isLeft)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, launchAff_, runAff_)
import Effect.Class (liftEffect)
import Romi.Db (dbClose, dbCreate)
import Romi.Logger (Logger, LoggerLevel(..), createLogger, info, record)
import Romi.Logger.Transport.Console (ConsoleTransport(..))
import Romi.Server (createServer, listen)
import Utils (onShutdownSignal)

envInit :: Aff Env
envInit = do
  db <- dbCreate dbDirectory dbPrefix
  let env = Env { db, logger, user: Nothing }
  _ <- runReaderT (runExceptT (do
    dbOps.putOrIf Users defaultModelUsers $ isLeft <<< parseModelUsers
    dbOps.putOrIf Messages "[]" $ isLeft <<< parseModelMessages
    dbOps.putOrIf Expenses "[]" $ isLeft <<< parseModelExpenses
    dbOps.putOrIf Meta defaultModelMeta $ isLeft <<< parseModelMeta
    pure unit
  )) env
  liftEffect $ onShutdownSignal $ runAff_ (\_ -> pure unit) $ dbClose db
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
  env <- envInit
  let Env ({ logger }) = env
  info logger $ "starting server..."
  server <- createServer routers env Nothing
  liftEffect $ listen server defaultServerPort $ launchAff_ $ record logger $ "Server is running on <yellow>http://localhost:" <> show defaultServerPort <> "</yellow>"
