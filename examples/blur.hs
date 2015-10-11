{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.Shaped
import Data.Shaped.Base
import Data.Shaped.Pixels
import Control.Lens

import qualified Data.Shaped.Unboxed as U

import Codec.Picture
import Data.Word

import Linear
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as UV
import System.Environment
import System.FilePath.Lens

juicyRGB :: Iso' (Image PixelRGB8) (V3 (Delayed V2 Word8))
juicyRGB = juicyBase . delayed . splitRGB8

-- Not particularly efficient.
splitRGB8 :: Iso' (Delayed V2 Word8) (V3 (Delayed V2 Word8))
splitRGB8 = iso spl unspl where

  spl (Delayed l ixF) =
    V3 (Delayed l' (ixF . (*3)))
       (Delayed l' (ixF . (+1) . (*3)))
       (Delayed l' (ixF . (+2) . (*3)))
         where l' = fmap (`quot` 3) l

  unspl
    (V3 (Delayed l (ixF0))
        (Delayed _ (ixF1))
        (Delayed _ (ixF2)))
      = Delayed (fmap (*3) l) $ \i ->
          case i `quotRem` 3 of
            (a, 0) -> ixF0 a
            (a, 1) -> ixF1 a
            (a, _) -> ixF2 a

------------------------------------------------------------------------

-- | How to index a element, even if it's out of bounds.
newtype BoundaryIndex f a =
  BoundaryIndex {
    runBoundary :: Layout f     -- total size
                -> (f Int -> a) -- standard index function
                -> f Int        -- index
                -> a            -- value
  }

-- | Periodic boundary conditions. Assumes the index is no more than one
--   away from the edge.
periodic :: Shape f => BoundaryIndex f a
periodic = BoundaryIndex $ \l ixF x ->
  let c n i | i < 0     = n + i
            | i >= n    = i - n
            | otherwise = i
  in  ixF (liftI2 c l x)
{-# INLINE periodic #-}

-- | Any values not 'inRange' take a constant value.
constant :: Shape f => a -> BoundaryIndex f a
constant a = BoundaryIndex $ \l ixF x ->
  if | inRange l x -> ixF x
     | otherwise   -> a
{-# INLINE constant #-}

-- | Any values not in range are clamped.
clamped :: Shape f => BoundaryIndex f a
clamped = BoundaryIndex $ \l ixF x ->
  let c n i | i < 0     = 0
            | i >= n    = n - 1
            | otherwise = i
  in  ixF (liftI2 c l x)
{-# INLINE clamped #-}

boundaryPeek
  :: Shape f
  => BoundaryIndex f a
  -> Focused f a
  -> f Int
  -> a
boundaryPeek bs f i = runBoundary bs (extent f) (flip peek f) (pos f ^+^ i)
{-# INLINE boundaryPeek #-}

-- | Sum a weighted neighbours to a 'Focused' using the provided
--   boundary condition.
sumNeighbours
  :: (Shape f, G.Vector v (f Int, a), Num a)
  => BoundaryIndex f a -- ^ boundary condition
  -> v (f Int, a)      -- ^ relative indexes and weights
  -> Focused f a       -- ^ focus to compute around
  -> a
sumNeighbours bi is w = G.foldl' (\s (i, a) -> s + boundaryPeek bi w i * a) 0 is
{-# INLINE sumNeighbours #-}

mkIndexes :: (Shape f, G.Vector v (f Int, a), G.Vector v a) => Array v f a -> v (f Int, a)
mkIndexes a = mkIndexesWithCentre (extent a <&> (`div` 2)) a
{-# INLINE mkIndexes #-}

mkIndexesWithCentre :: (Shape f, G.Vector v (f Int, a), G.Vector v a) => f Int -> Array v f a -> v (f Int, a)
mkIndexesWithCentre dx a = G.fromList $ a ^@.. reindexed (^-^ dx) values
{-# INLINE mkIndexesWithCentre #-}

-- Blur ----------------------------------------------------------------

blurStencil :: (U.Unbox a, Num a) => UArray V2 a
blurStencil = U.fromListInto_ (V2 5 5)
  [ 2,  4,  5,  4,  2
  , 4,  9, 12,  9,  4
  , 5, 12, 15, 12,  5
  , 4,  9, 12,  9,  4
  , 2,  4,  5,  4,  2
  ]

blurWords :: UV.Vector (V2 Int, Word)
blurWords = mkIndexes blurStencil
{-# NOINLINE blurWords #-}

wordExtract :: Focused V2 Word -> Word
wordExtract = sumNeighbours clamped blurWords

blurWord8 :: Delayed V2 Word8 -> Delayed V2 Word8
blurWord8 = extendFocus (fromIntegral . (`quot` 159) . wordExtract)
          . fmap fromIntegral
{-# INLINE blurWord8 #-}

blurImage :: Image PixelRGB8 -> Image PixelRGB8
blurImage = over (juicyRGB . _x . from U.delayed) ((!! 5) . iterate (manifest . blurWord8 . delay))

-- Read a png file from the first argument and make a blured file
-- (file_blured.png). JuicyPixels conversion is kinda messy, mainly
-- wrote to test 'sumNeighbours' prototype.
main :: IO ()
main = do
  [path] <- getArgs
  Right (ImageRGB8 img) <- readPng path
  writePng (path & basename <>~ "_blured") (blurImage img)

