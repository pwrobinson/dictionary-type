{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Dictionary
where
import Data.Dictionary.Request


-- | A Dictionary data structure `d` that is able to run requests of type `Request k` in monad `m`. Return values signals whether the request was successful. Note that we may want to handle requests differently for different key types. 
class Dictionary d k m where
    runRequest :: Monad m => Request k -> d -> m Bool
