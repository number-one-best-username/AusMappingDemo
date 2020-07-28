module Main where

import Data.Geospatial

import PackPop (packPop)

main :: IO ()
main = do
  packPop
  print "Done!"
