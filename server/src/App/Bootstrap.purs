module App.Bootstrap
  ( bootstrap
  )
  where

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
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Romi.Db (DB, dbCreate)
import Romi.Server (createServer, listen)
import Utils (log')

dbInit :: Aff DB
dbInit = do
  db <- dbCreate dbDirectory dbPrefix
  runReaderT (runExceptT (do
    dbOps.putOrIf Users defaultModelUsers $ isLeft <<< parseModelUsers
    dbOps.putOrIf Messages "[]" $ isLeft <<< parseModelMessages
    dbOps.putOrIf Expenses "[]" $ isLeft <<< parseModelExpenses
    dbOps.putOrIf Meta defaultModelMeta $ isLeft <<< parseModelMeta
    pure unit
  )) db
  pure db


bootstrap :: Aff Unit
bootstrap = do
  db <- dbInit
  log' "starting server..."
  server <- createServer routers (Env { db, user: Nothing }) Nothing
  liftEffect $ listen server defaultServerPort (log $ "Server is running on http://localhost:" <> show defaultServerPort)
