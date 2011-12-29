{-# LANGUAGE DeriveDataTypeable, 
             TemplateHaskell, 
             TypeFamilies, 
             TypeSynonymInstances #-}

module HEP.Automation.Model.Type where

import Control.Applicative 
import Control.Monad.Reader
import Control.Monad.State
import Data.Typeable
import Data.Data
import Data.SafeCopy
import qualified Data.Map as M

import Data.Acid 
import Data.UUID
import Data.UUID.V5 

data ModelInfo = ModelInfo { 
  model_uuid :: UUID, 
  model_name :: String
} deriving (Show,Typeable,Data)

instance SafeCopy UUID where 
  putCopy uuid = contain $ safePut (toByteString uuid) 
  getCopy = contain 
            $ maybe (fail "cannot parse UUID") return . fromByteString 
              =<< safeGet

$(deriveSafeCopy 0 'base ''ModelInfo)

type ModelInfoRepository = M.Map UUID ModelInfo 

addModel :: ModelInfo -> Update ModelInfoRepository (Maybe ModelInfo) 
addModel minfo = do 
  m <- get 
  let (r,m') = M.insertLookupWithKey (\_k _o n -> n) (model_uuid minfo) minfo m
  put m'
  return r
 
queryModel :: UUID -> Query ModelInfoRepository (Maybe ModelInfo) 
queryModel uuid = do 
  m <- ask 
  return (M.lookup uuid m)

queryAll :: Query ModelInfoRepository [ModelInfo]
queryAll = do m <- ask   
              return (M.elems m)


updateModel :: ModelInfo -> Update ModelInfoRepository (Maybe ModelInfo)
updateModel minfo = do 
  m <- get 
  let (r,m') = M.updateLookupWithKey (\_ _ -> Just minfo) (model_uuid minfo) m
  put m'
  return r

deleteModel :: UUID -> Update ModelInfoRepository (Maybe ModelInfo)
deleteModel uuid = do 
  m <- get
  let r = M.lookup uuid m  
  case r of 
    Just _ -> do  
      let m' = M.delete uuid m  
      put m' 
      return r
    Nothing -> return Nothing


$(makeAcidic ''ModelInfoRepository [ 'addModel
                                   , 'queryModel
                                   , 'queryAll
                                   , 'updateModel
                                   , 'deleteModel] )

