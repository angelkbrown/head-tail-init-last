module HeadTailInitLast where

middle :: [a] -> [a]
middle list = if length list < 3 then [] else init(tail(list))

secondToLast :: [Int] -> Int
secondToLast list = if length list < 2 then 0 else last(init(list))

ends :: [a] -> [a]
ends list = if null list then [] else head(list) : last(list) : []

firstTwo :: [a] -> [a]
firstTwo list = if length list < 2 then (if null list then [] else [head(list)]) else head(list) : head(tail(list)) : []
