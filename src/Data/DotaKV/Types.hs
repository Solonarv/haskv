{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module Data.DotaKV.Types where

import           Control.Monad
import           Data.Coerce
import           Data.Foldable
import           Data.IORef
import           Data.Void

import           Control.Monad.IO.Class
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HM
import qualified Data.HashMap.Strict    as HM
import qualified Data.HashSet           as HS
import           Data.Sequence          (Seq)
import qualified Data.Sequence          as Seq
import           Data.Text              (Text)
import           System.FilePath

data KVItem root base syn loc
  = Comment_ !loc !syn !Text     -- ^ A comment.
  | Spacer_ !loc !syn !Int       -- ^ One or more blank lines
  | Value !loc !Text !(KVal root base syn loc) -- ^ a key-value pair
  | Base !loc !base             -- ^ A #base directive (include)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

pattern Comment loc txt = Comment_ loc () txt
pattern Spacer loc len   = Spacer_ loc () len

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

data MergeStrategy = Overwrite | Combine

mergeWith :: MergeStrategy -> KVal KVObjMap base syn loc -> KVal KVObjMap base syn loc -> KVal KVObjMap base syn loc
mergeWith Overwrite = const id
mergeWith Combine   = combineObjs

combineObjs :: KVal KVObjMap base syn loc -> KVal KVObjMap base syn loc -> KVal KVObjMap base syn loc
combineObjs (Obj ol (KVObjMap ml)) (Obj or (KVObjMap mr)) = Obj or $ KVObjMap (HM.unionWith combineObjs ml mr)
combineObjs _ r             = r

newtype KVObjSeq base syn loc
  = KVObjSeq { getKVItems :: Seq (KVItem KVObjSeq base syn loc) }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype KVObjMap base syn loc
  = KVObjMap { getKVMappings :: HashMap Text (KVal KVObjMap base syn loc) }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Discard syntax and location info, as well as ordering
forgetLocation :: MergeStrategy -> KVFile KVObjSeq Void syn loc -> KVFile KVObjMap void syn' ()
forgetLocation strat KVFile{..}
  = KVFile
    { kvRootName = kvRootName
    , kvContent = obj
    , kvIncludes = fmap (forgetLocation strat) kvIncludes
    , ..
    }
  where
    obj = toMap kvContent
    toMap :: KVObjSeq Void syn loc -> KVObjMap void syn' ()
    toMap = KVObjMap . foldl'
      (\o -> \case
        Value _ k v -> HM.insertWith (mergeWith strat) k (toVal v) o
        _ -> o
      )
      HM.empty
      . getKVItems
    toVal = \case
      Txt _ v -> Txt () v
      Obj _ bag -> Obj () (toMap bag)

-- | Resolve #base directives (recursively)
resolveBase :: KVFile KVObjSeq FilePath () Int -> IO (KVFile KVObjSeq Void () Int)
resolveBase orig = go (HS.singleton (kvOrigFile orig)) orig
  where
    go :: HS.HashSet FilePath -> KVFile KVObjSeq FilePath () Int -> IO (KVFile KVObjSeq Void () Int)
    go seen KVFile{kvRootName, kvContent, kvOrigFile} = do
      childrenRef <- newIORef []
      let res :: Seq (KVItem KVObjSeq FilePath () Int) -> IO (Seq (KVItem KVObjSeq Void () Int))
          res = (fmap join .) . traverse $ \case
            Base loc (path :: FilePath) -> do
              let fullPath = replaceFileName kvOrigFile path
              when (fullPath `HS.member` seen) $ error "#base directives form a loop!"
              child@KVFile{kvContent = items} <- loadKVFile fullPath >>= go (HS.insert fullPath seen)
              modifyIORef childrenRef (child :)
              pure $ getKVItems $ items
            Comment loc txt -> pure $ Seq.singleton $ Comment loc txt
            Spacer loc len -> pure $ Seq.singleton $ Spacer loc len
            Value loc k (Txt off t) -> pure $ Seq.singleton $ Value loc k $ Txt off t
            Value loc k (Obj off els) -> do
              resolved <- res (getKVItems els)
              pure $ Seq.singleton $ Value loc k $ Obj off $ KVObjSeq resolved
      newContent <- res (getKVItems kvContent)
      children <- readIORef childrenRef
      pure KVFile
        { kvRootName
        , kvOrigFile
        , kvContent = KVObjSeq newContent
        , kvIncludes = Seq.fromList children}

loadKVFile :: FilePath -> IO (KVFile KVObjSeq FilePath () Int)
loadKVFile = error "Not implemented"
