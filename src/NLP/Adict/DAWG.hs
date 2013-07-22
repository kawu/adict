{-# LANGUAGE RecordWildCards #-}

-- | A directed acyclic word graph.

module NLP.Adict.DAWG
( DAWG (..)
, Node (..)
, DAWGM
, fromTrie
, fromDAWG
) where

import NLP.Adict.DAWG.Internal
