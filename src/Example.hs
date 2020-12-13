{-# language OverloadedStrings #-}
{-# language LambdaCase        #-}
{-# language ScopedTypeVariables #-}
{-# language DeriveGeneric #-}
{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language BangPatterns #-}
module Example where

import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Char8 (ByteString)

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as Mutable
import Control.Monad.ST
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import qualified VectorBuilder.Builder as Builder
import qualified VectorBuilder.Vector  as Builder
import Control.Arrow ((&&&))
import Text.Builder as TextBuilder
import Data.Foldable(traverse_)
import Data.Word (Word8)
import Data.Coerce

import qualified Data.Massiv.Array as Massiv
import qualified Data.Massiv.Array as Massiv
import Data.Massiv.Array (Ix2(..), Sz(..), U(..), DW, Value(..))
import Control.Applicative (liftA2)
import Control.Monad (filterM)

import Unsafe.Coerce

data Position =
    Empty
  | Occupied
  | Floor
  deriving (Eq, Generic, Show)



{-# inline fromW8 #-}
fromW8 :: Word8 -> Position
fromW8 n | n == 0 = Empty
         | n == 1 = Occupied
         | n == 2 = Floor
         | otherwise = errorWithoutStackTrace
             $ "Enum.fromW8: bad argument " <> (show n)

{-# inline toW8 #-}
toW8 :: Position -> Word8
toW8 = \case
    Empty    -> 0
    Occupied -> 1
    Floor    -> 2

derivingUnbox "Position"
    [t|  Position -> Word8 |]
    [| toW8     |]
    [| fromW8   |]

example :: IO ()
example = do
  input <- parseInput
  print $ s1 input

parseInput :: IO (Massiv.Array U Ix2 Position)
parseInput = do
  bs <- ByteString.readFile "input/input.dat"
  let
    (rowS, rowB) = (ByteString.length . head) &&& (foldMap buildRow) $ (ByteString.lines bs)
    mArr =
        (\ ~(v,l) -> Massiv.resize' (Massiv.Sz2 (l `div` rowS) rowS) v)
      . (Massiv.fromUnboxedVector &&& Vector.length)
      . Builder.build
      $ rowB
  pure $ mArr

buildRow :: ByteString -> Builder.Builder Position
buildRow bs = ByteString.foldl' fromEntry mempty bs
  where
    fromEntry :: Builder.Builder Position -> Char -> Builder.Builder Position
    fromEntry acc = \case
      'L' -> acc <> Builder.singleton Empty
      '.' -> acc <> Builder.singleton Floor
      '#' -> acc <> Builder.singleton Occupied
      c   -> error $ "buildRow: unexpected character: " <> [c] 

s1 :: Massiv.Array U Ix2 Position -> Int
s1 = countOccupied . (updateUntilEq update)


countOccupied ::
  Massiv.Array U Ix2 Position -> Int
countOccupied =
  Massiv.foldlS
  (\acc pos -> acc + (fromEnum (pos == Occupied)))
  0

updateUntilEq
  :: (Massiv.Array U Ix2 Position -> Massiv.Array U Ix2 Position)
  -> Massiv.Array U Ix2 Position
  -> Massiv.Array U Ix2 Position
updateUntilEq upFun =
  Massiv.iterateUntil
  (const (==))
  (const upFun)
 
update :: Massiv.Array U Ix2 Position -> Massiv.Array U Ix2 Position
update =
  Massiv.compute . Massiv.mapStencil border updateStencil
  
border :: Massiv.Border Position
border = Massiv.Fill Floor
--{-# inline border #-}


updateStencil :: Massiv.Stencil Ix2 Position Position
updateStencil =
  Massiv.makeStencilDef Floor (Sz (3 :. 3)) (1 :. 1) $ \ get ->
    let
      chair =  get (0 :. 0)
      isOcc c = fromEnum . (== Occupied) <$> (get c)
      occ :: Massiv.Value Int
      !occ = 
        (  isOcc (-1 :. -1) + isOcc (-1 :. 0) + isOcc (-1 :. 1) +
           isOcc ( 0 :. -1)                   + isOcc ( 0 :. 1) +
           isOcc ( 1 :. -1) + isOcc ( 1 :. 0) + isOcc ( 1 :. 1)
        )
    in
      if (unsafeCoerce chair) == Floor
        then pure Floor
        else liftA2 threshold chair occ
{-# inline updateStencil #-}

threshold :: Position -> Int -> Position
threshold !p !n | n == 0    = Occupied
               | n >= 4    = Empty
               | otherwise = p

{-# inline threshold #-}
