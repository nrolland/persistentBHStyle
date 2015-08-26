{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where


import           Control.Monad.IO.Class
-- import Database.Persist  -- only Persistent
import           Control.Applicative                   (pure)
import           Control.Monad.Logger                  (MonadLogger,
                                                        NoLoggingT (..),
                                                        monadLoggerLog,
                                                        runNoLoggingT)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource.Internal
import           Data.Time.Clock
import           Database.Esqueleto
import           Database.Persist.Sqlite


import           Database
import           Gender
import           Query
--instance MonadLogger IO where
--   monadLoggerLog _ _ _ = pure $ pure ()

import           Lib

main :: IO ()
main = do runSqlite "example.db" $ do
            -- Create table structure
            runMigration migrateAll
            -- Insert initial data
            insertInitialData
            liftIO $ print "run a query----------"
            c1 :: Maybe Client <- getClientById 1
            liftIO $ print c1
            liftIO $ print "people over 25--------"
            results1 <- getPeopleOver25FromSpainOrGermanyJoin
            mapM_ (\(Entity _ r) -> liftIO $ print r) results1
            liftIO $ print "money by client---------"
            results2 ::  [(Entity Client, Value (Maybe Double))] <- getMoneyByClient
            mapM_ (\(Entity _ r, Value l) -> liftIO $ print r >> print l) results2

mainWithExplicitConnection :: IO ()
mainWithExplicitConnection = runNoLoggingT x
    where x = withSqliteConn ":memory:" $ \(conn :: SqlBackend) ->
               NoLoggingT (flip runSqlPersistM conn y) :: (NoLoggingT IO) ()
          y = runMigration (migrateAll::Migration)  :: SqlPersistT (NoLoggingT (ResourceT IO))   ()

mainWithExplicitConnection2:: IO ()
mainWithExplicitConnection2 =
    runNoLoggingT $ withSqliteConn ":memory:" $ \conn ->
        NoLoggingT $ flip runSqlPersistM conn $ runMigration migrateAll

mainWithExplicitPool :: IO ()
mainWithExplicitPool =  runNoLoggingT $ withSqlitePool ":memory:" 3 $ \pool ->
                        NoLoggingT $ flip runSqlPersistMPool pool $ do
                          runMigration migrateAll

--insertInitialData :: PersistStore m => m ()
insertInitialData = do
  spain   <- insert $ Country "Spain" True
  germany <- insert $ Country "Germany" True
  uk      <- insert $ Country "United Kingdom" False
  _usa    <- insert $ Country "United States of America" False

  client1  <- insert $ Client "Alejandro" "Serrano" "Home Town, 1" spain (Just Male) Nothing
  client2  <- insert $ Client "Werner" "Heisenberg" "A place in Wurzburg" germany (Just Male) (Just 50)
  _client3 <- insert $ Client "Doctor" "Who" "Police Box" uk Nothing Nothing

  let longDescription = "blah blah blah"
  product1 <- insert $ Product "Travel to the XIX Century" longDescription 12.3 3
  product2 <- insert $ Product "TM-223 Machine" longDescription 1245.0 10

  _ <- insert $ Purchase client1 product1 1 20.0
  _ <- insert $ Purchase client1 product2 5 1002.3
  _ <- insert $ Purchase client2 product1 3 58.0

  return ()


