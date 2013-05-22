{-# LANGUAGE OverloadedStrings #-} 
{-# OPTIONS_GHC -Wall #-}

import Web.Scotty
import Data.Monoid (mconcat)
import Data.Text.Lazy hiding (map)
import Prelude hiding (concat)

import Candidate

main :: IO ()
main = scotty 3000 $ do
  get "/:word" $ do
    let table_names = map TableName ["companies", "group", "users"]
    let col_names = map ColName ["*"]
    seed <- param "word"
    let t = mconcat $ map pack $ map (\line -> "<li>" ++ line ++ "</li>") $ getCandidates (unpack seed) table_names col_names
    html $ mconcat $  ["<ul>", t, " </ul>"]
