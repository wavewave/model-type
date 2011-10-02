{-# LANGUAGE DeriveDataTypeable #-}

module HEP.Automation.Model.Type.Type where 

import System.Console.CmdArgs

data Model_type = Test 
              deriving (Show,Data,Typeable)

test :: Model_type
test = Test 

mode :: Model_type
mode = modes [test]

