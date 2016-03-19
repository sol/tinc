module Main (main) where

import           System.Environment

import           Tinc.Ghci

main :: IO ()
main = getArgs >>= ghci
