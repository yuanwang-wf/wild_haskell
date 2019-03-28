{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Lib where

type UserName = String
data DataResult = DataResult String deriving (Eq, Show)

class Monad m => Cache m where
    getFromCache :: String -> m (Maybe [DataResult])
    storeCache :: [DataResult] -> m ()


class Monad m => DataSource m where
    getFromSource :: String -> m [DataResult]

class Monad m => Logging m where
    logMsg :: String -> m ()

requestData :: (Cache m, DataSource m, Logging m) => UserName -> m [DataResult]
requestData userName = do
    cache <- getFromCache userName
    result <- case cache of
        Just dataResult -> return dataResult
        Nothing         -> getFromSource userName
    storeCache result
    logMsg $ "Result data for user " <> userName <> " - data: " <> show result
    return result

newtype NotInCache a = NotInCache { unNoCache :: IO a }
    deriving (Monad, Applicative, Functor)

instance Cache NotInCache where
    getFromCache _ = NotInCache $ return Nothing
    storeCache _ = NotInCache $ return ()

instance DataSource NotInCache where
    getFromSource user = return $ [DataResult $ "source: " <> user]

newtype InCache a = InCache { unInCache :: IO a }
    deriving (Monad, Applicative, Functor)

instance Cache InCache where
    getFromCache user = InCache $ return $ Just [DataResult $ "cache: " <> user ]
    storeCache _ = InCache $ return ()

instance DataSource InCache where
    getFromSource _ = undefined

instance Logging NotInCache where
    logMsg = NotInCache . putStrLn

instance Logging InCache where
    logMsg = InCache . putStrLn