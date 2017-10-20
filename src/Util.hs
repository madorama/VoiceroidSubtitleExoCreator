module Util where

import System.FilePath

mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f = aux 0
  where
    aux _ [] = []
    aux n (x:xs) = f n x : aux (n + 1) xs

concatMapi :: (Int -> a -> [b]) -> [a] -> [b]
concatMapi f xs = concat $ mapi f xs

isExtension :: String -> FilePath -> Bool
isExtension s path = takeExtension path == '.' : s
