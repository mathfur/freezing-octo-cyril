{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# Rank2Types #-}

module Candidate where

import Control.Monad
import Data.Char
import Control.Applicative
import Control.Monad.List
import Control.Monad.Reader
import Prelude

import Data.List

-- =========================================================

class ToSQL a where
    to_sql :: a -> String
    lengthS :: a -> Int
    lengthS = length . to_sql
    filter_statements :: String -> [a] -> [a]
    filter_statements seed = filter ((include_by_prefix seed).to_sql)
        where
            include_by_prefix :: String -> (String -> Bool)
            include_by_prefix seed sql = having_seed seed $ ((subsequences . map toLower . map head . words) sql)
            having_seed :: String -> [String] -> Bool
            having_seed = elem

data TableName = TableName String deriving (Show, Eq)
data ColName = ColName String deriving (Show, Eq)
data Context = Context { getTableNames :: [TableName], getColNames :: [ColName] } deriving (Show, Eq)

instance ToSQL TableName where
    to_sql (TableName s) = s

instance ToSQL ColName where
    to_sql (ColName s) = s

is_ok :: Statement -> Bool
is_ok = (flip (<) 30) . lengthS

filterStatement :: String -> [Statement] -> [String]
filterStatement prefixes = map to_sql . filter_statements prefixes . filter is_ok

table :: ListT (Reader Context) TableName
table = ask >>= (ListT . return . getTableNames)

col :: ListT (Reader Context) ColName
col = ask >>= (ListT . return . getColNames)

getCandidates :: String -> [TableName] -> [ColName] -> [String]
getCandidates seed table_names col_names = filterStatement seed candidates
    where
        candidates = runReader (runListT all_statements) $ Context table_names col_names

-- =========================================================

data Statement = ShowCreateTable TableName
               | SelectStar ColName TableName
               | ShowVariables
               deriving (Show, Eq)

instance ToSQL Statement where
    to_sql (ShowCreateTable t) = "SHOW CREATE TABLE " ++ to_sql t
    to_sql ShowVariables = "SHOW VARIABLES"
    to_sql (SelectStar col table) = "SELECT " ++ to_sql col ++ " FROM " ++ to_sql table

showCreateTable :: ListT (Reader Context) Statement
showCreateTable = ShowCreateTable <$> table

showSelectStar :: ListT (Reader Context) Statement
showSelectStar = SelectStar <$> col <*> table

showVariables :: ListT (Reader Context) Statement
showVariables = return ShowVariables


all_statements :: ListT (Reader Context) Statement
all_statements = do
    x <- showCreateTable
    y <- showVariables
    (\a b -> (ListT . return) [a, b]) x y
