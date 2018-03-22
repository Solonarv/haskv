{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.DotaKV.Writer (
  dump, hPutBuilder, toLazyByteString
) where

import           Data.Foldable
import           Data.Monoid

import           Data.ByteString.Builder
import           Data.Text               (Text)
import           Data.Text.Encoding      (encodeUtf8Builder)

import           Data.DotaKV.Types

dump :: Raw KVFile -> Builder
dump KVFile{..} = dumpRawSoup (getKVItems kvContent)

dumpRawSoup :: Foldable f => f (Raw KVItem) -> Builder
dumpRawSoup = foldMap dumpRawItem

dumpRawItem :: Raw KVItem -> Builder
dumpRawItem = \case
  Comment spc _ txt -> spaces spc <> "//" <> encodeUtf8Builder txt <> eol
  Spacer spc _ -> spaces spc <> eol
  Base spc path -> spaces spc <> "#base \"" <> stringUtf8 path <> "\"" <> eol
  Value spc k v -> spaces spc <> stringLit k <> dumpRawVal v <> eol

dumpRawVal :: Raw KVal -> Builder
dumpRawVal = \case
  Txt spc v -> spaces spc <> stringLit v
  Obj spc o -> spaces spc <> "{\n" <> dumpRawSoup (getKVItems o) <> "}"

eol :: Builder
eol = "\n"

-- TODO perform escaping
stringLit :: Text -> Builder
stringLit txt = "\"" <> encodeUtf8Builder txt <> "\""

spaces :: Int -> Builder
spaces n = replicateMonoid n " "

-- This equivalent to fold . replicate n, but more efficient
-- - it only calls (<>) approx. log2 n times, since the intermediate
-- structure is a binary tree instead of a linked list.
replicateMonoid :: Monoid a => Int -> a -> a
replicateMonoid 0 x = mempty
replicateMonoid 1 x = x
replicateMonoid n x
  = let (h, r) = n `divMod` 2
        half = replicateMonoid h x
        extra = replicateMonoid r x
    in half <> half <> extra
