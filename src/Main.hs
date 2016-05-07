module Main where

import Snap.Http.Server (quickHttpServe)

import MySnap (runMySnap)
import MySite (mySite)

main :: IO ()
main = quickHttpServe . runMySnap $ mySite
