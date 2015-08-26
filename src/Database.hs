{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Database where

import           Data.Time.Clock
import           Database.Persist.TH
import           Gender


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Country
  name      String
  canWeSend Bool default=False
  UniqueCountryName name
  deriving Show
Client
  firstName String
  lastName  String
  address   String
  country   CountryId
  gender    Gender Maybe
  age       Int Maybe
  UniqueClient firstName lastName address country
  AnotherConstraint firstName lastName address country
  deriving Show
Product
  name        String
  description String
  price       Double
  inStock     Int
  deriving Show
Purchase
  client  ClientId
  product ProductId
  number  Int
  amount  Double
  deriving Show
|]

