module HEP.Automation.Model.Type.Command where

import HEP.Automation.Model.Type.Type
import HEP.Automation.Model.Type.Job

commandLineProcess :: Model_type -> IO ()
commandLineProcess Test = do 
  putStrLn "test called"
  startJob
