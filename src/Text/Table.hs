module Text.Table ( tableFromRows
                  , TableHeader(..)
                  , TableRows(..)) where

data Show a => TableHeader a = TableHeader [a]
data Show a => TableRows a   = TableRows [[a]]

tableFromRows :: TableHeader a -> TableRows a -> String
tableFromRows = undefined
