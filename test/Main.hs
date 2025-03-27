{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Data.Bytes (Bytes)
import Data.Primitive (ByteArray, MutableByteArray)
import Data.Word (Word8)
import Data.Int (Int8, Int16, Int64)
import Flatbuffers.Parser (Parser,TableParser,UnionParser)
import Data.Text (Text)
import Control.Monad (when)
import Numeric (showHex)
import Data.Primitive (SmallArray, PrimArray)
import Data.Int (Int32)
import Data.Word (Word16)

import qualified Flatbuffers.Parser as P
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Data.Primitive as PM
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Parser as Parser


-- This example is from flatcc:
-- namespace Eclectic;
-- enum Fruit : byte { Banana = -1, Orange = 42 }
-- table FooBar {
--   meal      : Fruit = Banana;
--   density   : long (deprecated);
--   say       : string;
--   height    : short;
-- }
-- file_identifier "NOOB";
-- root_type FooBar;

main :: IO ()
main = do
  ex001 <- loadAndDecode "examples/001.txt"
  case P.run tableParserFooBar ex001 of
    Left err -> fail ("Test 001: " ++ show err)
    Right x -> do
      when (x.meal /= 42) (fail "Test 001: Wrong meal")
      when (x.density /= 0) $ fail $ "Test 001: Wrong density. Got: " ++ show x.density ++ " (0x" ++ showHex x.density "" ++ ")"
      when (x.height /= (-8000)) (fail "Test 001: Wrong height")
      when (x.say /= "hello") (fail "Test 001: Wrong say")
  putStrLn "Test 001 passed"
  ex002 <- loadAndDecode "examples/002.txt"
  case P.run footerParser ex002 of
    Left err -> fail ("Test 002: " ++ show err)
    Right x -> do
      when (length (x.schema.fields) /= 3) (fail "Test 002: Wrong number of fields")
  putStrLn "Test 002 passed"
  putStrLn "All tests passed"
  
data FooBar = FooBar
  { meal :: !Int8
  , density :: !Int64
  , say :: !Text
  , height :: !Int16
  }

tableParserFooBar :: TableParser FooBar
tableParserFooBar =
  FooBar <$> P.int8 <*> P.int64 <*> P.string <*> P.int16


loadAndDecode :: String -> IO ByteArray
loadAndDecode path = do
  contents <- Bytes.readFile path
  case decodeInput contents of
    Nothing -> fail ("Could not decode " ++ path ++ " as hex input")
    Just x -> pure x

decodeInput :: Bytes -> Maybe ByteArray
decodeInput =
    decodeSpacedHex
  . Bytes.intercalate (Bytes.singleton 0x20)
  . fmap (Bytes.takeWhile (/= 0x23))
  . Bytes.split 0x0A
  . Bytes.dropWhileEnd (==0x20)
  . Bytes.dropWhile (==0x20)

-- | Decode a byte sequence that looks like this:
--
-- > cd 0a bf ea 09 ...
--
-- There must be one or more space between each two-character representation
-- of an octet.
decodeSpacedHex :: Bytes -> Maybe ByteArray
decodeSpacedHex !b = Parser.parseBytesMaybe
  ( do let len = Bytes.length b
       dst <- Parser.effect (PM.newByteArray (len + 1))
       Parser.effect (PM.setByteArray dst 0 len (0 :: Word8))
       Latin.skipChar ' '
       parserSpacedHex dst 0
  ) b

parserSpacedHex :: MutableByteArray s -> Int -> Parser.Parser () s ByteArray
parserSpacedHex !dst !ix = do
  w <- Latin.hexFixedWord8 ()
  Parser.effect (PM.writeByteArray dst ix w)
  Parser.isEndOfInput >>= \case
    False -> do
      Latin.skipChar1 () ' '
      Parser.isEndOfInput >>= \case
        True -> Parser.effect $ do
          PM.shrinkMutableByteArray dst (ix + 1)
          PM.unsafeFreezeByteArray dst
        False -> parserSpacedHex dst (ix + 1)
    True -> Parser.effect $ do
      PM.shrinkMutableByteArray dst (ix + 1)
      PM.unsafeFreezeByteArray dst

data Footer = Footer
  { schema :: !Schema
  , dictionaries :: !(PrimArray Block)
  , recordBatches :: !(PrimArray Block)
  }

instance PM.Prim Block where
  sizeOf# _ = 24#
  alignment# _ = 8#

-- We ignore custom_metadata for the Footer
footerParser :: TableParser Footer
footerParser = Footer
  <$  P.word16Eq 4
  <*> P.table schemaParser
  <*> P.structs
  <*> P.structs

schemaParser :: TableParser Schema
schemaParser = Schema
  <$  P.word16Eq 0
  <*> P.array fieldParser

fieldParser :: TableParser Field
fieldParser = Field
  <$> P.string
  <*  P.ignore
  <*> P.union typeParser

typeParser :: UnionParser Type
typeParser = P.constructUnion2
  (pure Null)
  (Int <$> (TableInt <$> P.int32 <*> P.boolean))

data Schema = Schema
  { fields :: !(SmallArray Field)
  }

data Field = Field
  { name :: !Text
  , type_ :: Type
  }

data Type
  = None
  | Null
  | Int TableInt

data TableInt = TableInt
  { bitWidth :: !Int32
  , isSigned :: !Bool
  }

data Block = Block
  { offset :: !Int64
  , metaDataLength :: !Int32 -- we pad this when we encode it
  , bodyLength :: !Int64
  }
