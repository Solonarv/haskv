{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.DotaKV.Parser (loadKVFile) where

import           Data.Char
import           Data.Void

import           Data.Sequence              (Seq)
import qualified Data.Sequence              as Seq
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (charLiteral)

import           Data.DotaKV.Types

type Parser = Parsec Void Text


soup :: Parser (KVObjSeq FilePath () Int)
soup = KVObjSeq . Seq.fromList <$> some soupElem

soupElem :: Parser (Raw KVItem)
soupElem = comment <|> spacer <|> base <|> kvPair
  where
    comment = Comment <$> spaces <*> pure () <*> (string "//" *> restOfLine)
    spacer = Spacer <$> spaces <*> pure () <* eol
    base = Base <$> spaces <*> (string "#base" *> (Text.unpack <$> l stringLit))
    kvPair = Value <$> spaces <*> stringLit <*> value

value :: Parser (Raw KVal)
value = Txt <$> spaces <*> stringLit <|> Obj <$> spaces <*> obj
  where
    obj = between (l $ string "{") (l $ string "}") soup

spaces :: Parser Int
spaces = Text.length <$> takeWhileP (Just "spaces") isSpace

restOfLine :: Parser Text
restOfLine = takeWhile1P Nothing (\c -> c == '\n' || c == '\r') <* eol

l :: Parser a -> Parser a
l p = takeWhileP Nothing isSpace *> p

stringLit :: Parser Text
stringLit = Text.pack <$> (quotedLit <|> unquotedLit)
  where
    quotedLit = between (char '"') (char '"') (many charLiteral)
    unquotedLit = manyTill charLiteral spaceChar

loadKVFile :: FilePath -> IO (Raw KVFile)
loadKVFile fp = do
  src <- Text.readFile fp
  case parse soup fp src of
    Left err -> fail (show err)
    Right items -> pure KVFile
      { kvOrigFile = fp
      , kvRootName = determineRootName items
      , kvContent = items
      , kvIncludes = []
      }
