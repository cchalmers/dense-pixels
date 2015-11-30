{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -fno-warn-orphans #-} -- juicy storable instances

module Data.Shaped.Pixels
  ( -- * JuicyPixels
    juicy
  , juicyFlipped
  , juicyBase
  , juicyPacked

    -- * friday
  , fried
  )where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed as U
-- import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Primitive as P

import Control.Monad
import Control.Applicative
import Codec.Picture.Types
import qualified Vision.Image.Type as F
import Vision.Primitive.Shape (Z (..), (:.) (..))
import Data.Shaped.Generic
import Data.Shaped.Base
-- import Data.Shaped.Delayed.Generic
-- import Data.Shaped.Delayed
import Foreign
import Data.Vector.Storable (unsafeCast)
import Control.Lens
import Linear
import Prelude

-- | Images use storable vectors. The 'zero' pixel corresponds to the
--   top left pixel.
type Img = SArray V2

------------------------------------------------------------------------
-- JuicyPixels
------------------------------------------------------------------------

-- | Isomorphism between a "JuicyPixels" 'Image' and a "shaped"
--   'Delayed'. Note the image is transposed internally to because 'Image'
--   is column-major and 'V2' is row-major.
juicy :: (Storable a, Storable (PixelBaseComponent a),
          Storable b, Storable (PixelBaseComponent b) )
      => Iso (Image a) (Image b) (Delayed V2 a) (Delayed V2 b)
juicy = juicyFlipped . delayed . transposed
{-# INLINE juicy #-}

transposed :: Iso (Delayed V2 a) (Delayed V2 b) (Delayed V2 a) (Delayed V2 b)
transposed = iso trans trans
  where trans (Delayed l ixF) = Delayed (view _yx l) (ixF . view _yx)
{-# INLINE transposed #-}

-- transposed :: Iso (Delayed V2 a) (Delayed V2 b) (Delayed V2 a) (Delayed V2 b)
-- transposed = iso trans trans
--   -- where trans (Delayed l ixF) = Delayed (view _yx l) $ ixF . view _yx
--   where trans (Delayed l ixF)
--           = Delayed (view _yx l) $ ixF . shapeToIndex l . view _yx . shapeFromIndex l
-- {-# INLINE transposed #-}

-- | O(1) isomorphism between a "JuicyPixels" 'Image' and a "shaped" 'Array'.
--
--   Note that "shaped" uses row-major indexing and "JuicyPixels" uses
--   column-major, so O(1) conversion means the image gets flipped.
juicyFlipped :: (Storable a, Storable (PixelBaseComponent a),
                 Storable b, Storable (PixelBaseComponent b) )
      => Iso (Image a) (Image b) (Img a) (Img b)
juicyFlipped = iso (\(Image w h v)      -> Array (V2 h w) (unsafeCast v))
                   (\(Array (V2 h w) v) -> Image w h (unsafeCast v))
{-# INLINE juicyFlipped #-}

-- | Isomorphism between a "JuicyPixels" 'Image' and a "shaped" 'Array'
--   with the 'Pixel''s base component.
juicyBase :: forall a b. (Pixel a, Pixel b)
          => Iso (Image a) (Image b) (Img (PixelBaseComponent a)) (Img (PixelBaseComponent b))
juicyBase = iso (\(Image w h v)      -> Array (V2 h (n*w)) v)
                (\(Array (V2 h w) v) -> Image (w `quot` n) h v)
  where n = componentCount (undefined :: a)
{-# INLINE juicyBase #-}

-- | Isomorphism between a "JuicyPixels" 'Image' and a "shaped" 'Array'
--   with the 'Pixel''s packed representation.
juicyPacked :: (Storable a, Storable (PixelBaseComponent a),
                PackeablePixel a, Storable (PackedRepresentation a),
                Storable b, Storable (PixelBaseComponent b),
                PackeablePixel b, Storable (PackedRepresentation b))
            => Iso (Image a) (Image b) (Img (PackedRepresentation a)) (Img (PackedRepresentation b))
juicyPacked = iso (\(Image w h v)      -> Array (V2 w h) (unsafeCast v))
                  (\(Array (V2 w h) v) -> Image w h (unsafeCast v))
{-# INLINE juicyPacked #-}

newtype instance U.Vector    PixelRGB8 = V_RGB (P.Vector    Word8)
newtype instance U.MVector s PixelRGB8 = M_RGB (P.MVector s Word8)
instance U.Unbox PixelRGB8

instance M.MVector U.MVector PixelRGB8 where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  basicLength (M_RGB v)              = M.length v `div` 3
  basicUnsafeSlice m n (M_RGB v)     = M_RGB (M.basicUnsafeSlice (3*m) (3*n) v)
  basicOverlaps (M_RGB v) (M_RGB u) = M.basicOverlaps v u
  basicUnsafeNew n                  = liftM M_RGB (M.basicUnsafeNew (3*n))
  basicUnsafeRead (M_RGB v) i        =
    do let o = 3*i
       x <- M.basicUnsafeRead v o
       y <- M.basicUnsafeRead v (o+1)
       z <- M.basicUnsafeRead v (o+2)
       return (PixelRGB8 x y z)
  basicUnsafeWrite (M_RGB v) i (PixelRGB8 x y z) =
    do let o = 3*i
       M.basicUnsafeWrite v o     x
       M.basicUnsafeWrite v (o+1) y
       M.basicUnsafeWrite v (o+2) z
-- #if MIN_VERSION_vector(0,11,0)
  basicInitialize (M_RGB v) = M.basicInitialize v
  {-# INLINE basicInitialize #-}
-- #endif

instance G.Vector U.Vector PixelRGB8 where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw   #-}
  {-# INLINE basicLength       #-}
  {-# INLINE basicUnsafeSlice  #-}
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeFreeze (M_RGB v)     = liftM  V_RGB (G.basicUnsafeFreeze v)
  basicUnsafeThaw   ( V_RGB v)     = liftM M_RGB (G.basicUnsafeThaw   v)
  basicLength       ( V_RGB v)     = G.basicLength v `div` 3
  basicUnsafeSlice m n (V_RGB v) = V_RGB (G.basicUnsafeSlice (3*m) (3*n) v)
  basicUnsafeIndexM (V_RGB v) i    =
    do let o = 3*i
       x <- G.basicUnsafeIndexM v o
       y <- G.basicUnsafeIndexM v (o+1)
       z <- G.basicUnsafeIndexM v (o+2)
       return (PixelRGB8 x y z)

instance Storable PixelRGBA8 where
  sizeOf ~(PixelRGBA8 a _ _ _) = 4 * sizeOf a
  {-# INLINE sizeOf #-}
  alignment ~(PixelRGBA8 a _ _ _) = alignment a
  {-# INLINE alignment #-}
  poke ptr (PixelRGBA8 r g b a) =
    poke        ptr'   r *>
    pokeElemOff ptr' 1 g *>
    pokeElemOff ptr' 2 b *>
    pokeElemOff ptr' 3 a
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = PixelRGBA8 <$>
    peek        ptr'   <*>
    peekElemOff ptr' 1 <*>
    peekElemOff ptr' 2 <*>
    peekElemOff ptr' 3
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

instance Storable PixelYA8 where
  sizeOf ~(PixelYA8 a _) = 2 * sizeOf a
  {-# INLINE sizeOf #-}
  alignment ~(PixelYA8 a _) = alignment a
  {-# INLINE alignment #-}
  poke ptr (PixelYA8 y a) =
    poke        ptr'   y *>
    pokeElemOff ptr' 1 a
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = PixelYA8 <$>
    peek        ptr'   <*>
    peekElemOff ptr' 1
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

instance Storable PixelYA16 where
  sizeOf ~(PixelYA16 a _) = 2 * sizeOf a
  {-# INLINE sizeOf #-}
  alignment ~(PixelYA16 a _) = alignment a
  {-# INLINE alignment #-}
  poke ptr (PixelYA16 y a) =
    poke        ptr'   y *>
    pokeElemOff ptr' 1 a
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = PixelYA16 <$>
    peek        ptr'   <*>
    peekElemOff ptr' 1
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

instance Storable PixelRGBF where
  sizeOf ~(PixelRGBF a _ _) = 2 * sizeOf a
  {-# INLINE sizeOf #-}
  alignment ~(PixelRGBF a _ _) = alignment a
  {-# INLINE alignment #-}
  poke ptr (PixelRGBF r g b) =
    poke        ptr'   r *>
    pokeElemOff ptr' 1 g *>
    pokeElemOff ptr' 2 b
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = PixelRGBF <$>
    peek        ptr'   <*>
    peekElemOff ptr' 1 <*>
    peekElemOff ptr' 2
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

instance Storable PixelRGB16 where
  sizeOf ~(PixelRGB16 a _ _) = 3 * sizeOf a
  {-# INLINE sizeOf #-}
  alignment ~(PixelRGB16 a _ _) = alignment a
  {-# INLINE alignment #-}
  poke ptr (PixelRGB16 r g b) =
    poke        ptr'   r *>
    pokeElemOff ptr' 1 g *>
    pokeElemOff ptr' 1 b
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = PixelRGB16 <$>
    peek        ptr'   <*>
    peekElemOff ptr' 1 <*>
    peekElemOff ptr' 2
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

instance Storable PixelRGBA16 where
  sizeOf ~(PixelRGBA16 a _ _ _) = 4 * sizeOf a
  {-# INLINE sizeOf #-}
  alignment ~(PixelRGBA16 a _ _ _) = alignment a
  {-# INLINE alignment #-}
  poke ptr (PixelRGBA16 r g b a) =
    poke        ptr'   r *>
    pokeElemOff ptr' 1 g *>
    pokeElemOff ptr' 2 b *>
    pokeElemOff ptr' 3 a
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = PixelRGBA16 <$>
    peek        ptr'   <*>
    peekElemOff ptr' 1 <*>
    peekElemOff ptr' 2 <*>
    peekElemOff ptr' 3
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

instance Storable PixelRGB8 where
  sizeOf ~(PixelRGB8 a _ _) = 3 * sizeOf a
  {-# INLINE sizeOf #-}
  alignment ~(PixelRGB8 a _ _) = alignment a
  {-# INLINE alignment #-}
  poke ptr (PixelRGB8 r g b) =
    poke        ptr'   r *>
    pokeElemOff ptr' 1 g *>
    pokeElemOff ptr' 2 b
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = PixelRGB8 <$>
    peek        ptr'   <*>
    peekElemOff ptr' 1 <*>
    peekElemOff ptr' 2
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

------------------------------------------------------------------------
-- Friday
------------------------------------------------------------------------

-- | Isomorphism between a "JuicyPixels" 'Image' and a "shaped" 'Array'.
fried :: Iso (F.Manifest a) (F.Manifest b) (Img a) (Img b)
fried = iso (\(F.Manifest (Z :. w :. h) v) -> Array (V2 w h) v)
            (\(Array (V2 w h) v)           -> F.Manifest (Z :. w :. h) v)
{-# INLINE fried #-}

-- juicyFri :: Iso (Image (Juicy a)) (Image (Juicy b)) (Manifest a) (Manifest b)
-- juicyFri = iso (\(Image w h v) -> Manifest (Z :. w :. h) v)
--                (\(Manifest (Z :. w :. h) v) -> Image w h v)
-- {-# INLINE juicyFri #-}

