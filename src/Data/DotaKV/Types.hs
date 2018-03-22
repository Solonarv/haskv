{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.DotaKV.Types where

import           Prelude                hiding (fail)

import           Control.Monad          hiding (fail)
import           Control.Monad.Fail
import           Data.Coerce
import           Data.Foldable
import           Data.Functor.Identity
import           Data.IORef
import           Data.Maybe
import           Data.Monoid
import           Data.Void

import           Control.Monad.IO.Class
import           Data.Hashable
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HM
import qualified Data.HashMap.Strict    as HM
import qualified Data.HashSet           as HS
import           Data.Sequence          (Seq)
import qualified Data.Sequence          as Seq
import           Data.Text              (Text)
import           System.FilePath

data KVItem root base syn loc
  = Comment !loc !syn !Text     -- ^ A comment.
  | Spacer !loc !syn            -- ^ A blank line
  | Value !loc !Text !(KVal root base syn loc) -- ^ a key-value pair
  | Base !loc !base             -- ^ A #base directive (include)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data KVal root base syn loc
  = Txt !loc !Text
  | Obj !loc !(root base syn loc)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data KVFile root base syn loc
  = KVFile
    { kvOrigFile :: !FilePath
    , kvRootName :: !Text
    , kvContent  :: !(root base syn loc)
    , kvIncludes :: !(Seq (KVFile root base syn loc))
    }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type Raw ty = ty KVObjSeq FilePath () Int

type RawSoup = KVObjSeq FilePath () Int

data MergeStrategy = Overwrite | Combine

mergeWith :: MergeStrategy -> KVal KVObjMap base syn loc -> KVal KVObjMap base syn loc -> KVal KVObjMap base syn loc
mergeWith Overwrite = const id
mergeWith Combine   = combineObjs

combineObjs :: KVal KVObjMap base syn loc -> KVal KVObjMap base syn loc -> KVal KVObjMap base syn loc
combineObjs (Obj ol (KVObjMap ml)) (Obj or (KVObjMap mr)) = Obj or $ KVObjMap (HM.unionWith combineObjs ml mr)
combineObjs _ r = r

newtype KVObjSeq base syn loc
  = KVObjSeq { getKVItems :: Seq (KVItem KVObjSeq base syn loc) }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

mapItems :: (KVItem KVObjSeq base syn loc -> KVItem KVObjSeq base' syn' loc')
         -> KVObjSeq base syn loc
         -> KVObjSeq base' syn' loc'
mapItems f (KVObjSeq items) = KVObjSeq (f <$> items)

newtype KVObjMap base syn loc
  = KVObjMap { getKVMappings :: HashMap Text (KVal KVObjMap base syn loc) }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

mapContentsM :: Monad m
             => (root base syn loc -> m (root' base' syn' loc'))
             -> KVFile root base syn loc
             -> m (KVFile root' base' syn' loc')
mapContentsM k KVFile{..} = do
  newContent <- k kvContent
  newIncludes <- traverse (mapContentsM k) kvIncludes
  pure KVFile
    { kvContent = newContent
    , kvIncludes = newIncludes
    , ..
    }

mapContents :: (root base syn loc -> root' base' syn' loc')
            -> KVFile root base syn loc
            -> KVFile root' base' syn' loc'
mapContents f = runIdentity . mapContentsM (Identity . f)

-- | Discard key ordering info. Requires #base directives to be resolved,
-- and comments/whitespace to have been forgotten.
forgetOrdering :: MergeStrategy
               -> KVFile KVObjSeq Void Void ()
               -> (forall void syn. KVFile KVObjMap void syn ())
forgetOrdering strat = mapContents toMap
  where
    -- toMap :: KVObjSeq Void syn loc -> (forall void syn'. KVObjMap void syn' ())
    toMap = KVObjMap . foldl'
      (\o -> \case
        Value _ k v -> HM.insertWith (mergeWith strat) k (toVal v) o
      )
      HM.empty
      . getKVItems
    toVal = \case
      Txt _ v -> Txt () v
      Obj _ bag -> Obj () (toMap bag)

forgetCommentsAndWhitespace :: KVFile KVObjSeq path syn loc -> KVFile KVObjSeq path void loc
forgetCommentsAndWhitespace = mapContents go
  where
    go (KVObjSeq items) = KVObjSeq $ items >>= \case
      Value l k v -> pure $ Value l k (mapVal v)
      Base l b -> pure $ Base l b
      Comment{} -> []
      Spacer{} -> []
    mapVal = \case
      Txt l v -> Txt l v
      Obj l o -> Obj l (go o)


-- | Discard #base directives without resolving them.
-- Returns Nothing if the argument contains any #Base directives, Just the argument otherwise.
discardBase :: KVFile KVObjSeq FilePath syn off -> Maybe (KVFile KVObjSeq Void syn off)
discardBase = resolveBaseWith (const Nothing)

-- | Resolve #base directives (recursively), using a custom loading function
resolveBaseWith :: MonadFail m
                => (FilePath -> m (KVFile KVObjSeq FilePath syn off))
                -> KVFile KVObjSeq FilePath syn off
                -> m (KVFile KVObjSeq void syn off)
resolveBaseWith loadKVFile orig = go (HS.singleton (kvOrigFile orig)) orig
  where
    -- go :: HS.HashSet FilePath -> KVFile KVObjSeq FilePath syn loc -> m (KVFile KVObjSeq void syn loc)
    go seen KVFile{kvRootName, kvContent, kvOrigFile} = do
      let -- res :: Seq (KVItem KVObjSeq FilePath syn loc) -> m (Seq (KVItem KVObjSeq void syn loc), Seq (KVFile KVObjSeq void syn loc))
          res = (fmap fold .) . traverse $ \case
            Base loc path -> do
              let fullPath = replaceFileName kvOrigFile path
              when (fullPath `HS.member` seen) $ fail "#base directives form a loop!"
              child@KVFile{kvContent = items} <- loadKVFile fullPath >>= go (HS.insert fullPath seen)
              pure (getKVItems items, [child])
            Comment loc syn txt -> pure (Seq.singleton $ Comment loc syn txt, [])
            Spacer loc syn -> pure (Seq.singleton $ Spacer loc syn, [])
            Value loc k (Txt off t) -> pure (Seq.singleton $ Value loc k $ Txt off t, [])
            Value loc k (Obj off els) -> do
              (resolved, cs) <- res (getKVItems els)
              pure (Seq.singleton $ Value loc k $ Obj off $ KVObjSeq resolved, cs)
      (newContent, children) <- res (getKVItems kvContent)
      pure KVFile
        { kvRootName
        , kvOrigFile
        , kvContent = KVObjSeq newContent
        , kvIncludes = children}

determineRootName :: KVObjSeq base syn loc -> Text
determineRootName (KVObjSeq items) = fromMaybe "root" . getLast $ foldMap (Last . extractKey) items
  where
    extractKey = \case
      Value _ k _ -> Just k
      _ -> Nothing
