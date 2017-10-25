module Main where

import Reflex.Dom.Core

import ChatWidget

main :: 
  IO ()
main = 
  mainWidget chatWidget
