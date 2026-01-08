module App.Bootstrap
  ( bootstrap
  )
  where

import Prelude

import App.Constant (dbDirectory, dbPrefix, defaultModelMeta, defaultModelUsers, defaultServerPort)
import App.Models (DBKey(..), dbm)
import App.Route (routers)
import App.Schema (parseModelExpenses, parseModelMessages, parseModelMeta, parseModelUsers)
import App.Types (State(..))
import Control.Monad.Reader (runReaderT)
import Data.Array (length)
import Data.Either (Either(..), isLeft)
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
  runReaderT (do
    dbm.putOrIf Users (show defaultModelUsers) $ \dat -> isLeft $ parseModelUsers dat >>= \users -> if length users == 0 then Left "" else Right ""
    dbm.put Users (show defaultModelUsers)
    dbm.putOrIf Messages "[]" $ isLeft <<< parseModelMessages
    dbm.putOrIf Expenses "[]" $ isLeft <<< parseModelExpenses
    dbm.putOrIf Meta (show defaultModelMeta) $ isLeft <<< parseModelMeta
  ) db
  pure db


bootstrap :: Aff Unit
bootstrap = do
  db <- dbInit
  log' "starting server..."
  server <- createServer routers (State { db, user: Nothing }) Nothing
  liftEffect $ listen server defaultServerPort (log $ "Server is running on http://localhost:" <> show defaultServerPort)
