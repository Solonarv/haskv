{-# LANGUAGE OverloadedLists #-}
module Data.DotaKV.Format where

import           Data.Coerce
import           Data.Monoid
import           Data.Void

import           Control.Monad.Writer
import           Data.IntMap.Lazy     (IntMap, (!))
import qualified Data.IntMap.Lazy     as IntMap
import qualified Data.Text            as Text

import           Data.DotaKV.Types

inferFormat :: Bool                                   -- | Align values across separate arrays?
            -> KVFile KVObjSeq base syn ()
            -> KVFile KVObjSeq base syn Int
inferFormat alignAcross = mapContents (if alignAcross then goAcross 0 else goSimple 0)
  where
    goSimple, goAcross :: Int -> KVObjSeq base syn () -> KVObjSeq base syn Int
    goSimple cur (KVObjSeq items) = KVObjSeq out
      where
        (out, maximum -> valueIndent) = runWriter $ traverse go items
        go = \case
          Comment _ s t -> pure $ Comment cur s t
          Spacer _ s -> pure $ Spacer cur s
          Base _ path -> pure $ Base cur path
          Value _ k v -> do
            let klen = len k
            tell ([klen + 4] :: [Int])
            pure $ Value cur k (goChildren (valueIndent - klen) v)
        goChildren i = \case
          Txt _ v -> Txt i v
          Obj _ cs -> Obj i (goSimple (cur + 2) cs)
    len txt = Text.length txt + 2
    goAcross cur (KVObjSeq items) = KVObjSeq out
      where
        indentPairs :: [(Int, Int)]
        (out, indentPairs) = runWriter $ traverse (go 0) items
        indentMap = foldr (uncurry $ IntMap.insertWith max) IntMap.empty indentPairs
        go depth = \case
          Comment _ s t -> pure $ Comment cur s t
          Spacer _ s -> pure $ Spacer cur s
          Base _ path -> pure $ Base cur path
          Value _ k v -> do
            let klen = len k
            newV <- goChildren klen depth v
            tell [(depth, klen)]
            pure $ Value cur k newV
        goChildren klen depth = \case
          Txt _ v -> pure $ Txt ((indentMap ! depth) - klen) v
          Obj _ cs -> do
            newChildren <- traverse (go (depth + 1)) $ getKVItems cs
            pure $ Obj ((indentMap ! depth) - klen) (KVObjSeq newChildren)

-- | Loosen a concrete Void to a forall x. x
unforgetBase :: KVFile KVObjSeq Void syn loc -> KVFile KVObjSeq void syn loc
unforgetBase = mapContents (mapItems go)
  where
    go = \case
      Comment l s t -> Comment l s t
      Spacer l s -> Spacer l s
      Value l k (Txt d v) -> Value l k (Txt d v)
      Value l k (Obj d o) -> Value l k (Obj d $ mapItems go o)
