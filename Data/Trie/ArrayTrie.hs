{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}

module Data.Trie.ArrayTrie
( Trie (..)
) where

import GHC.Exts
import Data.Bits (bitSize)
import Control.Monad.ST.Safe (runST)
import Control.Monad (forM_)
import Data.Primitive.ByteArray
import Data.Primitive.Array

import qualified Data.Trie.Generic as G

data Trie a = Trie
    { valueIn   ::                !(Maybe a)
    , labelsA   :: {-# UNPACK #-} !ByteArray
    , childrenA :: {-# UNPACK #-} !(Array (Trie a)) }

{-# INLINE sizeOfArray #-}
sizeOfArray :: Array a -> Int
sizeOfArray (Array arr#) = I# (sizeofArray# arr#)

{-# INLINE childrenNum #-}
childrenNum :: Trie a -> Int
childrenNum Trie{..} = sizeOfArray childrenA

wordSize :: Int
wordSize = bitSize (undefined :: Int) `div` 8

instance G.Trie Trie where
    mkTrie v cs = runST $ do
        let n = length cs

        xArr <- newByteArray (n * wordSize)
        forM_ (zip [0..] cs) $ \(k, (x, _)) ->
            writeByteArray xArr k x
        xs <- unsafeFreezeByteArray xArr

        yArr <- newArray n undefined
        forM_ (zip [0..] cs) $ \(k, (_, c)) ->
            writeArray yArr k c
        ys <- unsafeFreezeArray yArr

        return $ Trie v xs ys

    unTrie t@Trie{..} = (valueIn,
        [ ( indexByteArray labelsA k
          , indexArray childrenA k )
        | k <- [0 .. childrenNum t - 1] ] )

    setValue x t = t { valueIn = x }

instance Show a => Show (Trie a) where
    show = show . G.toList

-- instance (Ord a, Binary a, Binary b) => Binary (Trie a b) where
--     put (Trie x ts) = put (x, ts)
--     get = uncurry Trie <$> get
-- --     put t = put (size t) >> mapM_ put (toAscList t)
-- --     get   = fromDistinctAscList <$> get
