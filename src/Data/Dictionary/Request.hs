{-# LANGUAGE BangPatterns #-}

module Data.Dictionary.Request
where
import System.Random
import Control.Exception
import Prelude hiding (lookup)


data Request k
    = Insert !k !k
    | Delete !k
    | Lookup !k
    | Update !k !k -- ^ only used for the Test module, not for benchmarking
    deriving (Show,Eq)

isLookup (Lookup _ ) = True
isLookup _ = False

isInsert (Insert _ _) = True
isInsert _ = False

isDelete (Delete _ ) = True
isDelete _ = False

-- | Generates a list of `Request`s according to the specified probability distribution and the key range.
generateTests :: (Random k)
              => Int             -- ^ length of the list
              -> (Float,Float)   -- ^ (prob. of a lookup, prob. of an insert); the prob. of a delete = 1 - (prob. of lookup) - (prob. of insert)
              -> (k,k)       -- ^ range from which integer keys are being chosen uniformly
              -> IO [Request k]
generateTests !l !(f@(fLookups,fInserts)) !keyRange
    | l <= 0 = return []
    | fLookups + fInserts > 1
        = throw $ AssertionFailed "Fractions need to add up to 1"
    | otherwise = do
        !p <- randomIO :: IO Float
        !k <- randomRIO keyRange
        !a <- randomIO
        let op = if p < fInserts then
                    Insert k a
                 else if p < fInserts + fLookups then
                    Lookup k
                 else Delete k
        (:) op <$> generateTests (l-1) f keyRange
