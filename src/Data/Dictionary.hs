{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Dictionary
where
import Data.Dictionary.Request


-- | A Dictionary data structure `d` that is able to run requests of type `Request k` in monad `m`. Return values signals whether the request was successful.
class Dictionary d k m where
    runRequest :: Monad m => Request k -> d -> m Bool
{-
instance Dictionary (SkipList Int Int) Int IO where
    runRequest (Lookup k) s = do
        r <- lookup s k
        case r of
            Nothing -> return False
            Just _  -> return True
    runRequest (Insert k a) s = insert s k a
    runRequest (Delete k) s = delete s k


instance Dictionary (MVar (M.IntMap Int )) Int IO where
    runRequest (Lookup k) m = do
        mymap <- readMVar m
        case M.lookup k mymap of
            Nothing -> return False
            Just _ -> return True
    runRequest (Insert k a) m =
        modifyMVar m $ \mymap -> do
            -- let s1 = M.size mymap -- O(1)
            let mymap' = M.insert k a mymap
            -- let s2 = M.size mymap' -- O(1)
            return $! (mymap',M.null mymap')
    runRequest (Delete k) m = do
        modifyMVar m $ \mymap -> do
            -- let s1 = M.size mymap -- O(1)
            let mymap' = M.delete k mymap
            -- let s2 = M.size mymap' -- O(1)
            return $! (mymap',M.null mymap')

newtype IORefPrimOps a = IORefPrimOps (IORef (M.IntMap a))
instance Dictionary (IORefPrimOps Int) Int IO where
    runRequest (Lookup k) (IORefPrimOps m) = do
        mymap <- readIORef m
        case M.lookup k mymap of
            Nothing -> return False
            Just _ -> return True
    runRequest (Insert k a) (IORefPrimOps m) =
        atomicModifyIORefCAS m $ \mymap ->
            let !mymap' = M.insert k a mymap in
            (mymap',M.null mymap')
    runRequest (Delete k) (IORefPrimOps m) =
        atomicModifyIORefCAS m $ \mymap ->
            let !mymap' = M.delete k mymap in
            (mymap',M.null mymap')

newtype IORefPrimOpsHashMap a = IORefPrimOpsHashMap (IORef (UO.HashMap Int a))
instance Dictionary (IORefPrimOpsHashMap Int) Int IO where
    runRequest (Lookup k) (IORefPrimOpsHashMap m) = do
        mymap <- readIORef m
        case UO.lookup k mymap of
            Nothing -> return False
            Just _ -> return True
    runRequest (Insert k a) (IORefPrimOpsHashMap m) =
        atomicModifyIORefCAS m $ \mymap ->
            let !mymap' = UO.insert k a mymap in
            (mymap',UO.null mymap')
    runRequest (Delete k) (IORefPrimOpsHashMap m) =
        atomicModifyIORefCAS m $ \mymap ->
            let !mymap' = UO.delete k mymap in
            (mymap',UO.null mymap')

instance Dictionary (IORef (M.IntMap Int)) Int IO where
    runRequest (Lookup k) m = do
        mymap <- readIORef m
        case M.lookup k mymap of
            Nothing -> return False
            Just _ -> return True
    runRequest (Insert k a) m =
        atomicModifyIORef' m $ \mymap ->
            let !mymap' = M.insert k a mymap in
            (mymap',M.null mymap')
    runRequest (Delete k) m =
        atomicModifyIORef' m $ \mymap ->
            let !mymap' = M.delete k mymap in
            (mymap',M.null mymap')

instance Dictionary (TVar (M.IntMap Int)) Int IO where
    runRequest (Lookup k) m = do
        mymap <- readTVarIO m
        case M.lookup k mymap of
            Nothing -> return False
            Just _ -> return True
    runRequest (Insert k a) m =
        atomically $ stateTVar m $ \mymap ->
            let !mymap' = M.insert k a mymap in
            (M.null mymap',mymap')
    runRequest (Delete k) m =
        atomically $ stateTVar m $ \mymap ->
            let !mymap' = M.delete k mymap in
            (M.null mymap',mymap')

instance Dictionary (H.HashTable Int Int) Int IO where
    runRequest (Lookup k) s = do
        r <- H.lookup s k
        case r of
            Nothing -> return False
            Just _  -> return True
    runRequest (Insert k a) s = H.insert s k a
    runRequest (Delete k) s = H.delete s k

instance Dictionary (C.HashTable Int Int) Int IO where
    runRequest (Lookup k) s = do
        r <- C.lookup s k
        case r of
            Nothing -> return False
            Just _  -> return True
    runRequest (Insert k a) s = C.insert s k a
    runRequest (Delete k) s = C.delete s k
-}
