module Qafny.FileUtils where

countDepth :: String -> Int
countDepth = length . filter (== '/')
