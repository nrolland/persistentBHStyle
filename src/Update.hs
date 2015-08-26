{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Update where

import           Control.Monad
import           Data.Char
import           Database.Esqueleto
import qualified Database.Persist        as P
import           Database.Persist.Sqlite (runSqlite)

import           Database
-- import Chapter11.Gender

-- capitalizeNamesSlow :: m ()
capitalizeNamesSlow = do
  clients <- P.selectList [] []
  mapM_ (\(Entity ident client) ->
             let c:rest  = clientFirstName client
              in P.replace ident $ client { clientFirstName = (toUpper c):rest })
        clients

-- discount :: m ()
discount = do
  P.updateWhere [ ProductPrice P.<=. 10000 ] [ ProductPrice P.*=. 0.9 ]
  P.updateWhere [ ProductPrice P.>. 10000 ] [ ProductPrice P.*=. 0.97 ]

-- betterDiscount :: m ()
betterDiscount = update $ \product -> do
  let totalAmount = sub_select $
                    from $ \purchase -> do
                    where_ $ product ^. ProductId ==. purchase ^. PurchaseProduct
                    groupBy (purchase ^. PurchaseProduct)
                    return $ sum_ (purchase ^. PurchaseAmount)
  --where_ $ (isNothing totalAmount ) -- :: Key Product)   --  totalAmount <. just (val 10)
  set product [ ProductPrice *=. val 0.9 ]

-- cleanProductStock :: m ()
cleanProductStock = P.deleteWhere [ ProductInStock P.==. 0 ]

-- cleanProductStock' :: m ()
cleanProductStock' =
  delete $
  from $ \product -> do
    where_ $ product ^. ProductInStock ==. val 0
           &&. (notExists $ from $ \purchase ->
                            where_ (purchase ^. PurchaseProduct ==. product ^. ProductId))
