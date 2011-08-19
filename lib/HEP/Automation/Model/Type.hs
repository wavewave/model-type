{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies, TypeSynonymInstances #-}

module HEP.Automation.Model.Type where

-- import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

import Data.Typeable
import Data.Data

import Data.SafeCopy

import qualified Data.Map as M

import Data.Acid 

-- import Data.Serialize.Get


data ModelInfo = ModelInfo { 
  model_name :: String, 
  model_baseurl :: String, 
  model_feynrules :: String, 
  model_ufo :: String 
} deriving (Show,Typeable,Data)

$(deriveSafeCopy 0 'base ''ModelInfo)

type ModelInfoRepository = M.Map String ModelInfo 

addModel :: ModelInfo -> Update ModelInfoRepository () 
addModel minfo = do 
  m <- get 
  put $ M.insert (model_name minfo) minfo m                           
 
queryModel :: String -> Query ModelInfoRepository (Maybe ModelInfo) 
queryModel name = do 
  m <- ask 
  return (M.lookup name m)

$(makeAcidic ''ModelInfoRepository [ 'addModel, 'queryModel ] )

