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

addModel :: ModelInfo -> Update ModelInfoRepository (Maybe ModelInfo) 
addModel minfo = do 
  m <- get 
  let (r,m') = M.insertLookupWithKey (\_k _o n -> n) (model_name minfo) minfo m
  put m'
  return r
 
queryModel :: String -> Query ModelInfoRepository (Maybe ModelInfo) 
queryModel name = do 
  m <- ask 
  return (M.lookup name m)

queryAll :: Query ModelInfoRepository [ModelInfo]
queryAll = do m <- ask   
              return (M.elems m)


updateModel :: ModelInfo -> Update ModelInfoRepository (Maybe ModelInfo)
updateModel minfo = do 
  m <- get 
  let (r,m') = M.updateLookupWithKey (\_k _n -> Just minfo) (model_name minfo) m
  put m'
  return r

deleteModel :: String -> Update ModelInfoRepository (Maybe ModelInfo)
deleteModel name = do 
  m <- get
  let r = M.lookup name m  
  case r of 
    Just _ -> do  
      let m' = M.delete name m  
      put m' 
      return r
    Nothing -> return Nothing


$(makeAcidic ''ModelInfoRepository [ 'addModel, 'queryModel, 'queryAll, 'updateModel, 'deleteModel] )

