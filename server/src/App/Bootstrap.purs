module App.Bootstrap
  ( bootstrap
  )
  where

import Prelude

import App.Components (logger)
import App.Constant (dbDirectory, dbPrefix, defaultModelMeta, defaultModelUsers, defaultServerPort)
import App.Handler (createExpense, createMessage, createUser, fetchAllExpenses, fetchAllMessages, fetchAllUsers, fetchMeta)
import App.Models (DBKey(..), dbm)
import App.Schema (parseModelExpenses, parseModelMessages, parseModelMeta, parseModelUsers)
import App.Types (State(..))
import Control.Monad.Reader (runReaderT)
import Data.Either (isLeft)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Romi.Components (Component(..), Components, Rule(..))
import Romi.Db (DB, dbCreate)
import Romi.Request (Method(..))
import Romi.Server (createServer, listen)
import Utils (log')

dbInit :: Aff DB
dbInit = do
  db <- dbCreate dbDirectory dbPrefix
  runReaderT (do
    dbm.putOrIf Users (show defaultModelUsers) $ isLeft <<< parseModelUsers
    dbm.putOrIf Messages "[]" $ isLeft <<< parseModelMessages
    dbm.putOrIf Expenses "[]" $ isLeft <<< parseModelExpenses
    dbm.putOrIf Meta (show defaultModelMeta) $ isLeft <<< parseModelMeta
  ) db
  pure db

components :: Components State
components =
  [ Before Any logger
  , Route GET "/api/messages" fetchAllMessages
  , Route POST "/api/messages" createMessage
  , Route GET "/api/expenses" fetchAllExpenses
  , Route POST "/api/expenses" createExpense
  , Route GET "/api/users" fetchAllUsers
  , Route POST "/api/users" createUser
  , Route GET "/api/meta" fetchMeta
  ]

bootstrap :: Aff Unit
bootstrap = do
  db <- dbInit
  log' "starting server..."
  server <- createServer components (State { db, user: Nothing }) Nothing
  liftEffect $ listen server defaultServerPort (log $ "Server is running on http://localhost:" <> show defaultServerPort)
