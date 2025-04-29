{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DuplicateRecordFields #-}
{-# language NamedFieldPuns #-}
{-# language MultiWayIf #-}

module Flatbuffers.Parser
  ( Parser
  , TableParser
  , UnionParser
  , Error(..)
  , run
  , tableParserThrow
    -- * Table Fields
  , boolean
  , int8
  , int16
  , word8Eq
  , word16
  , word16Eq
  , int32
  , int64
  , ignore
  , string
  , union
  , table
  , optTable
  , array
  , structs
    -- * Unions
  , constructUnion2
  , constructUnion3
  , constructUnionFromList
  ) where

import Prelude hiding (length)

import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Bytes.Types (Bytes(Bytes))
import Data.Word (Word16, Word32)
import Data.Text (Text)
import Data.Int (Int8,Int16,Int32,Int64)
import Data.Word (Word8)
import Data.Kind (Type)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT, except)

import Data.Primitive (Prim, sizeOfType, alignmentOfType)
import Data.Primitive (sizeofByteArray,indexSmallArray,sizeofSmallArray)
import Data.Primitive (newByteArray,unsafeFreezeByteArray)
import Data.Primitive (unsafeFreezeSmallArray,newSmallArray)
import Data.Primitive (PrimArray(PrimArray),ByteArray(ByteArray),SmallArray)
import Data.Primitive.Contiguous (Slice,Sliced,slice)
import System.ByteOrder (ByteOrder(LittleEndian), Fixed(Fixed))

import qualified Data.Primitive as PM
import qualified Data.Primitive.Contiguous as Contiguous
import qualified Data.Bytes.Types
import qualified Data.Bytes.Text.Utf8 as Utf8
import qualified Data.Primitive.ByteArray.LittleEndian as LE
import qualified GHC.Exts as Exts

-- We do not include the vtable length in the slice since that number
-- is already captured by the Sliced data constructor itself. We begin
-- with the table length at index 0. The table length has to be included
-- in the vtable because we need to validate that it does not imply OOB
-- outside of the vtable decoding function.
--
-- Invariant: Length of slice >= 1 (because element 0 is table length)
-- Invariant: Table length >= 4 (table must include VTable offset)
-- Invariant: All offsets are < table length (this simplifies bounds
--   checking later).
newtype VTable = VTable (Sliced PrimArray (Fixed 'LittleEndian Word16))

data Error
  = VTableAlignmentError
  | VTableLengthError
  | UnionTagOutOfBounds
  | TableOffsetAlignmentError
  | StringOffsetAlignmentError
  | FieldStringAlignmentError
  | VTableLengthOutOfBoundsError
  | VTableLengthAlignmentError
  | VTableIndexOutOfBoundsError !Int !Int
  | NegativeVTableIndexError !Int
  | TableVTableEndOfInputError
  | TableAlignmentError
  | StringNotUtf8
  | StringOffsetOutOfBounds
  | TagImpliesUnionNone
  | VTableImpliesOutOfBoundsField
  | VTableLengthLessThanFour
      !Int -- offset of the vtable (where it starts)
      !Int
  | FieldBooleanNotInRange
  | FieldInt16AlignmentError
  | FieldInt32AlignmentError
  | FieldInt64AlignmentError
  | DefaultingStringNotSupportedYet
  | DefaultingTableNotSupported
  | VTableSizeImpliesTableOutOfBounds
  | TooSmallForRootTableOffset
  | RootTableOffsetOutOfBounds
  | RootTableOffsetLessThanFourError
  | ArrayOffsetOutOfBounds
  | ArrayOfStructOffsetOutOfBounds
  | ArrayOfStructPayloadOutOfBounds !Int !Int !Int
  | ArrayOffsetAlignmentError
  | ArrayOfStructOffsetAlignmentError
  | FieldArrayAlignmentError
  | ExpectedWord8EqButGot !Word8 !Word8
  | ExpectedWord16EqButGot !Word16 !Word16
  | MissingFieldWithIndex !Int
  | UnsupportedUnionTag !Word8
  deriving (Show)

type Parser :: Type -> Type
newtype Parser a = Parser
  (    ByteArray  -- The entire encoded document
    -> Int -- position
    -> Int -- end position (the length of the bytearray, a constant)
    -> Either Error a
  )

deriving instance Functor Parser

type TableParser :: Type -> Type
newtype TableParser a = TableParser
  (    ByteArray  -- The entire encoded document
    -> Int -- offset to the beginning of the table (a constant)
    -> Int -- end position (the length of the bytearray, a constant)
    -> VTable -- VTable for the table currently being decoded
    -> Int    -- current field position
    -> Either Error (R a)
  )

deriving instance Functor TableParser
instance Applicative TableParser where
  pure x = TableParser
    (\_ _ _ _ field -> Right (R field x))
  TableParser f <*> TableParser g = TableParser $
    \doc off end vt field -> case f doc off end vt field of
      Left e -> Left e
      Right (R field' h) -> case g doc off end vt field' of
        Left e -> Left e
        Right (R field'' a) -> Right (R field'' (h a))
instance Monad TableParser where
  TableParser f >>= g = TableParser $
    \doc off end vt field -> case f doc off end vt field of
      Left e -> Left e
      Right (R field' a) -> case g a of
        TableParser h -> h doc off end vt field'

data R a = R !Int !a

deriving instance Functor R

-- | Unions can only be unions of tables. In 2017, flatbuffers changed
-- this to support more types, but this library sticks with the original
-- restriction to make things more simple.
newtype UnionParser a = UnionParser (SmallArray (TableParser a))

tableParserThrow :: Error -> TableParser a
tableParserThrow e = TableParser (\_ _ _ _ _ -> Left e)

constructUnion2 :: TableParser a -> TableParser a -> UnionParser a
constructUnion2 a b = UnionParser (Contiguous.construct2 a b)

constructUnion3 :: TableParser a -> TableParser a -> TableParser a -> UnionParser a
constructUnion3 a b c = UnionParser (Contiguous.construct3 a b c)

constructUnionFromList :: [TableParser a] -> UnionParser a
constructUnionFromList xs = UnionParser (Exts.fromList xs)

-- | Reads the first four bytes to determine the root table.
run :: TableParser a -> ByteArray -> Either Error a
run p x = do
  when (sizeofByteArray x < 4) (Left TooSmallForRootTableOffset)
  let !(rootTableOffset :: Word32) = LE.indexByteArray x 0
  let !rootTableOffsetI = fromIntegral @Word32 @Int rootTableOffset
  -- These two checks are probably redundant, but they will result
  -- in a better error message if a user tries to decode a malformed
  -- document.
  when (rootTableOffsetI < 4) (Left RootTableOffsetLessThanFourError)
  when (rootTableOffsetI + 4 > sizeofByteArray x) (Left RootTableOffsetOutOfBounds)
  case lowerTableParser p of
    Parser f -> f x rootTableOffsetI (sizeofByteArray x)

lowerTableParser :: TableParser a -> Parser a
lowerTableParser (TableParser f) = Parser $ \theArray offset end ->
  if | end - offset < 4 -> Left TableVTableEndOfInputError
     | rem offset 4 /= 0 -> Left TableAlignmentError
     | (vtableInvertedRelOffsetI32 :: Int32) <- LE.indexByteArray theArray (quot offset 4)
     , vtableInvertedRelOffset <- fromIntegral @Int32 @Int vtableInvertedRelOffsetI32
     , vtableIndex <- offset - vtableInvertedRelOffset ->
       if | vtableIndex < 0 -> Left (NegativeVTableIndexError vtableIndex)
          | vtableIndex >= end -> Left (VTableIndexOutOfBoundsError vtableIndex end)
          | otherwise -> do
              vtable <- bytesToVTable theArray vtableIndex end
              when (offset + tableSizeFromVTable vtable > end) (Left VTableSizeImpliesTableOutOfBounds)
              R _ a <- f theArray offset end vtable 0
              Right a

boolean :: TableParser Bool
boolean = TableParser $ \content tableOffset _ vtable fieldIx -> do
  !fieldOffset <- indexVTableField vtable fieldIx
  case fieldOffset of
    0 -> do
      let !fieldIx' = fieldIx + 1
      Right (R fieldIx' False)
    _ -> do
      let !effectiveOffset = (fromIntegral @Word16 @Int fieldOffset) + tableOffset
      let !v = LE.indexByteArray content effectiveOffset :: Word8
      let !fieldIx' = fieldIx + 1
      case v of
        0 -> Right (R fieldIx' False)
        1 -> Right (R fieldIx' True)
        _ -> Left FieldBooleanNotInRange

word8Eq :: Word8 -> TableParser ()
word8Eq !expecting = TableParser $ \content tableOffset _ vtable fieldIx -> do
  !fieldOffset <- indexVTableField vtable fieldIx
  case fieldOffset of
    0 -> do
      let !fieldIx' = fieldIx + 1
      if expecting == 0
        then Right (R fieldIx' ())
        else Left (ExpectedWord8EqButGot expecting 0)
    _ -> do
      let !effectiveOffset = (fromIntegral @Word16 @Int fieldOffset) + tableOffset
      let !v = LE.indexByteArray content effectiveOffset
      let !fieldIx' = fieldIx + 1
      if expecting == v
        then Right (R fieldIx' ())
        else Left (ExpectedWord8EqButGot expecting v)

word8 :: TableParser Word8
word8 = TableParser $ \content tableOffset _ vtable fieldIx -> do
  !fieldOffset <- indexVTableField vtable fieldIx
  case fieldOffset of
    0 -> do
      let !fieldIx' = fieldIx + 1
      Right (R fieldIx' 0)
    _ -> do
      let !effectiveOffset = (fromIntegral @Word16 @Int fieldOffset) + tableOffset
      let !v = LE.indexByteArray content effectiveOffset
      let !fieldIx' = fieldIx + 1
      Right (R fieldIx' v)

int8 :: TableParser Int8
int8 = TableParser $ \content tableOffset _ vtable fieldIx -> do
  !fieldOffset <- indexVTableField vtable fieldIx
  case fieldOffset of
    0 -> do
      let !fieldIx' = fieldIx + 1
      Right (R fieldIx' 0)
    _ -> do
      let !effectiveOffset = (fromIntegral @Word16 @Int fieldOffset) + tableOffset
      let !v = LE.indexByteArray content effectiveOffset
      let !fieldIx' = fieldIx + 1
      Right (R fieldIx' v)

int16 :: TableParser Int16
int16 = TableParser $ \content tableOffset _ vtable fieldIx -> do
  !fieldOffset <- indexVTableField vtable fieldIx
  case fieldOffset of
    0 -> do
      let !fieldIx' = fieldIx + 1
      Right (R fieldIx' 0)
    _ -> do
      let !effectiveOffset = (fromIntegral @Word16 @Int fieldOffset) + tableOffset
      when (rem effectiveOffset 2 /= 0) (Left FieldInt16AlignmentError)
      let !v = LE.indexByteArray content (quot effectiveOffset 2)
      let !fieldIx' = fieldIx + 1
      Right (R fieldIx' v)

word16 :: TableParser Word16
word16 = TableParser $ \content tableOffset _ vtable fieldIx -> do
  !fieldOffset <- indexVTableField vtable fieldIx
  case fieldOffset of
    0 -> do
      let !fieldIx' = fieldIx + 1
      Right (R fieldIx' 0)
    _ -> do
      let !effectiveOffset = (fromIntegral @Word16 @Int fieldOffset) + tableOffset
      when (rem effectiveOffset 2 /= 0) (Left FieldInt16AlignmentError)
      let !v = LE.indexByteArray content (quot effectiveOffset 2)
      let !fieldIx' = fieldIx + 1
      Right (R fieldIx' v)

word16Eq :: Word16 -> TableParser ()
word16Eq !expecting = TableParser $ \content tableOffset _ vtable fieldIx -> do
  !fieldOffset <- indexVTableField vtable fieldIx
  case fieldOffset of
    0 -> do
      let !fieldIx' = fieldIx + 1
      if expecting == 0
        then Right (R fieldIx' ())
        else Left (ExpectedWord16EqButGot expecting 0)
    _ -> do
      let !effectiveOffset = (fromIntegral @Word16 @Int fieldOffset) + tableOffset
      when (rem effectiveOffset 2 /= 0) (Left FieldInt16AlignmentError)
      let !v = LE.indexByteArray content (quot effectiveOffset 2)
      let !fieldIx' = fieldIx + 1
      if expecting == v
        then Right (R fieldIx' ())
        else Left (ExpectedWord16EqButGot expecting v)

int32 :: TableParser Int32
int32 = TableParser $ \content tableOffset _ vtable fieldIx -> do
  !fieldOffset <- indexVTableField vtable fieldIx
  case fieldOffset of
    0 -> do
      let !fieldIx' = fieldIx + 1
      Right (R fieldIx' 0)
    _ -> do
      let !effectiveOffset = (fromIntegral @Word16 @Int fieldOffset) + tableOffset
      when (rem effectiveOffset 4 /= 0) (Left FieldInt32AlignmentError)
      let !v = LE.indexByteArray content (quot effectiveOffset 4)
      let !fieldIx' = fieldIx + 1
      Right (R fieldIx' v)

optTable :: TableParser a -> TableParser (Maybe a)
optTable arg = TableParser $ \content tableOffset end vtable fieldIx -> do
  !fieldOffset <- indexVTableField vtable fieldIx
  case fieldOffset of
    0 -> do
      let !fieldIx' = fieldIx + 1
      Right (R fieldIx' Nothing)
    _ -> do
      let !effectiveOffset = (fromIntegral @Word16 @Int fieldOffset) + tableOffset
      when (rem effectiveOffset 4 /= 0) (Left FieldStringAlignmentError)
      let !(tableFieldOffset :: Word32) = LE.indexByteArray content (quot effectiveOffset 4)
      let !effectiveTableOffset = (fromIntegral @Word32 @Int tableFieldOffset) + effectiveOffset
      when (rem effectiveTableOffset 4 /= 0) (Left TableOffsetAlignmentError)
      case lowerTableParser arg of
        Parser f -> do
          result <- f content effectiveTableOffset end
          let !fieldIx' = fieldIx + 1
          pure (R fieldIx' (Just result))

-- | Use a table as a field in another table.
table :: TableParser a -> TableParser a
table arg = TableParser $ \content tableOffset end vtable fieldIx -> do
  !fieldOffset <- indexVTableField vtable fieldIx
  case fieldOffset of
    0 -> Left DefaultingTableNotSupported
    _ -> pure ()
  let !effectiveOffset = (fromIntegral @Word16 @Int fieldOffset) + tableOffset
  when (rem effectiveOffset 4 /= 0) (Left FieldStringAlignmentError)
  let !(tableFieldOffset :: Word32) = LE.indexByteArray content (quot effectiveOffset 4)
  let !effectiveTableOffset = (fromIntegral @Word32 @Int tableFieldOffset) + effectiveOffset
  when (rem effectiveTableOffset 4 /= 0) (Left TableOffsetAlignmentError)
  case lowerTableParser arg of
    Parser f -> do
      result <- f content effectiveTableOffset end
      let !fieldIx' = fieldIx + 1
      pure (R fieldIx' result)

-- | Use a string as a field in a table.
string :: TableParser Text
string = TableParser $ \content tableOffset end vtable fieldIx -> do
  !fieldOffset <- indexVTableField vtable fieldIx
  case fieldOffset of
    0 -> Left DefaultingStringNotSupportedYet
    _ -> pure ()
  let !effectiveOffset = (fromIntegral @Word16 @Int fieldOffset) + tableOffset
  when (rem effectiveOffset 4 /= 0) (Left FieldStringAlignmentError)
  let !(stringOffset :: Word32) = LE.indexByteArray content (quot effectiveOffset 4)
  let !effectiveStringOffset = (fromIntegral @Word32 @Int stringOffset) + effectiveOffset
  when (rem effectiveStringOffset 4 /= 0) (Left StringOffsetAlignmentError)
  when (effectiveStringOffset + 4 > end) (Left StringOffsetOutOfBounds)
  let !(stringLength :: Word32) = LE.indexByteArray content (quot effectiveStringOffset 4)
  let stringLengthI = fromIntegral @Word32 @Int stringLength
  when (effectiveStringOffset + 5 + stringLengthI > end) (Left StringOffsetOutOfBounds)
  let !payload = Bytes content (effectiveStringOffset + 4) stringLengthI
  !t <- case Utf8.toText payload of
    Nothing -> Left StringNotUtf8
    Just t -> Right t
  let !fieldIx' = fieldIx + 1
  Right (R fieldIx' t)

structs :: forall a. Prim a => TableParser (PrimArray a)
structs = structsInternal (alignmentOfType @a) (sizeOfType @a)

structsInternal :: Int -> Int -> TableParser (PrimArray a)
structsInternal !alignment !sz = TableParser $ \content tableOffset end vtable fieldIx -> do
  !fieldOffset <- indexVTableField vtable fieldIx
  case fieldOffset of
    0 -> Left DefaultingStringNotSupportedYet
    _ -> pure ()
  let !effectiveOffset = (fromIntegral @Word16 @Int fieldOffset) + tableOffset
  when (rem effectiveOffset 4 /= 0) (Left FieldArrayAlignmentError)
  let !(arrayOffset :: Word32) = LE.indexByteArray content (quot effectiveOffset 4)
  let !effectiveArrayOffset = (fromIntegral @Word32 @Int arrayOffset) + effectiveOffset
  when (rem effectiveArrayOffset 4 /= 0) (Left ArrayOfStructOffsetAlignmentError)
  when (effectiveArrayOffset + 4 > end) (Left ArrayOfStructOffsetOutOfBounds)
  let !(arrayLength :: Word32) = LE.indexByteArray content (quot effectiveArrayOffset 4)
  let arrayLengthI = fromIntegral @Word32 @Int arrayLength
  -- We do not check the alignment of the start element when the length
  -- is zero because there is no start element.
  when (arrayLengthI > 0) $ do
    when (rem (effectiveArrayOffset + 4) alignment /= 0) (Left ArrayOfStructOffsetAlignmentError)
  let !payloadSz = arrayLengthI * sz
  when (effectiveArrayOffset + 4 + payloadSz > end) (Left (ArrayOfStructPayloadOutOfBounds effectiveArrayOffset payloadSz end))
  let !(ByteArray arr) = runST $ do
        dst <- newByteArray (sz * arrayLengthI)
        PM.copyByteArray dst 0 content (effectiveArrayOffset + 4) (sz * arrayLengthI)
        unsafeFreezeByteArray dst
  let !fieldIx' = fieldIx + 1
  Right (R fieldIx' (PrimArray arr))

-- | Use an array of tables as a field in a table.
array :: TableParser a -> TableParser (SmallArray a)
array tp = TableParser $ \content tableOffset end vtable fieldIx -> do
  !fieldOffset <- indexVTableField vtable fieldIx
  case fieldOffset of
    0 -> Left DefaultingStringNotSupportedYet
    _ -> pure ()
  let !effectiveOffset = (fromIntegral @Word16 @Int fieldOffset) + tableOffset
  when (rem effectiveOffset 4 /= 0) (Left FieldArrayAlignmentError)
  let !(arrayOffset :: Word32) = LE.indexByteArray content (quot effectiveOffset 4)
  let !effectiveArrayOffset = (fromIntegral @Word32 @Int arrayOffset) + effectiveOffset
  when (rem effectiveArrayOffset 4 /= 0) (Left ArrayOffsetAlignmentError)
  when (effectiveArrayOffset + 4 > end) (Left ArrayOffsetOutOfBounds)
  let !(arrayLength :: Word32) = LE.indexByteArray content (quot effectiveArrayOffset 4)
  let arrayLengthI = fromIntegral @Word32 @Int arrayLength
  when (effectiveArrayOffset + 4 + (arrayLengthI * 4) > end) (Left ArrayOffsetOutOfBounds)
  let !offsetsArray = slice (reinterpret32 content) (quot (effectiveArrayOffset + 4) 4) arrayLengthI
  arr <- runST $ runExceptT $ do
    dst <- lift (newSmallArray arrayLengthI undefined)
    Contiguous.itraverse_
      ( \ix (Fixed off) -> do
        let offI = fromIntegral off :: Int
        x <- except $ case lowerTableParser tp of
          Parser f -> f content (effectiveArrayOffset + 4 + (ix * 4) + offI) end
        lift (PM.writeSmallArray dst ix x)
      ) offsetsArray
    unsafeFreezeSmallArray dst
  let !fieldIx' = fieldIx + 1
  Right (R fieldIx' arr)

-- | If the tag is set to zero for the None option, this fails. 
union :: UnionParser a -> TableParser a
union (UnionParser options) = do
  !tagW <- word8
  when (tagW == 0) (tableParserThrow TagImpliesUnionNone)
  let !tag = (fromIntegral tagW :: Int) - 1
  when (tag >= sizeofSmallArray options) (tableParserThrow UnionTagOutOfBounds)
  let !tp = indexSmallArray options tag
  table tp

ignore :: TableParser ()
ignore = TableParser $ \_ _ _ vtable fieldIx -> do
  !_ <- indexVTableField vtable fieldIx
  let !fieldIx' = fieldIx + 1
  Right (R fieldIx' ())

int64 :: TableParser Int64
int64 = TableParser $ \content tableOffset _ vtable fieldIx -> do
  !fieldOffset <- indexVTableField vtable fieldIx
  case fieldOffset of
    0 -> do
      let !fieldIx' = fieldIx + 1
      Right (R fieldIx' 0)
    _ -> do
      let !effectiveOffset = (fromIntegral @Word16 @Int fieldOffset) + tableOffset
      when (rem effectiveOffset 8 /= 0) (Left FieldInt64AlignmentError)
      let !v = LE.indexByteArray content (quot effectiveOffset 8)
      let !fieldIx' = fieldIx + 1
      Right (R fieldIx' v)

-- string :: TableParser Text
-- string = _

bytesToVTable :: ByteArray -> Int -> Int -> Either Error VTable
bytesToVTable !theArray !offset !end
  | end - offset < 4 = Left VTableLengthError
  | rem offset 2 /= 0 = Left VTableAlignmentError
  | !(offsetW16 :: Int) <- quot offset 2
  , !(vtableLen :: Word16) <- LE.indexByteArray theArray offsetW16
  , !(vtableLenI :: Int) <- fromIntegral @Word16 @Int vtableLen =
    if | vtableLenI + offset > end -> Left VTableLengthOutOfBoundsError
       | rem vtableLenI 2 /= 0 -> Left VTableLengthAlignmentError
       | vtableLenI < 4 -> Left (VTableLengthLessThanFour offset vtableLenI)
       | !sliceLen <- quot vtableLenI 2 - 1
       , !arrW16 <- reinterpret16 theArray
       , !theSlice <- slice arrW16 (offsetW16 + 1) sliceLen
       , !theSliceWithoutTableLen <- slice arrW16 (offsetW16 + 2) (sliceLen - 1)
       , !(Fixed tableSize) <- Contiguous.index theSlice 0 ->
         if | not (Contiguous.all (\(Fixed x) -> x < tableSize) theSliceWithoutTableLen) -> Left VTableImpliesOutOfBoundsField
            | otherwise -> Right $! VTable theSlice

reinterpret16 :: ByteArray -> PrimArray (Fixed 'LittleEndian Word16)
{-# inline reinterpret16 #-}
reinterpret16 (ByteArray x) = PrimArray x

reinterpret32 :: ByteArray -> PrimArray (Fixed 'LittleEndian Word32)
{-# inline reinterpret32 #-}
reinterpret32 (ByteArray x) = PrimArray x

indexVTableField :: VTable -> Int -> Either Error Word16
indexVTableField (VTable xs) !ix
    -- everything missing from the end is treated like Absent
  | effectiveIx >= Contiguous.size xs = Right 0
  | otherwise = let Fixed w = Contiguous.index xs effectiveIx in Right w
  where
  !effectiveIx = ix + 1

tableSizeFromVTable :: VTable -> Int
tableSizeFromVTable (VTable xs) =
  let Fixed w = Contiguous.index xs 0 in fromIntegral w
