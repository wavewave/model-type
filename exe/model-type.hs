module Main where

import System.Console.CmdArgs

import HEP.Automation.Model.Type.Type
import HEP.Automation.Model.Type.Command

main :: IO () 
main = do 
  putStrLn "model-type"
  param <- cmdArgs mode

  commandLineProcess param