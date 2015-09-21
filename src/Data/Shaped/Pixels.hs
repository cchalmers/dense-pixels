{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -fno-warn-orphans #-}
module Data.Shaped.Pixels where

import Control.Applicative
import Codec.Picture.Types
import Vision.Image.Type
import Vision.Primitive.Shape (Z (..), (:.) (..))
-- import Data.Shaped.Base
import Data.Shaped
import Data.Shaped.Base
import Foreign
import Data.Vector.Storable (unsafeCast)
import Control.Lens
import Linear

-- | Images use storable vectors. The 'zero' pixel corresponds to the
--   top left pixel.
type Img = SArray V2

------------------------------------------------------------------------
-- JuicyPixels
------------------------------------------------------------------------

-- XXX Sort out row-minor and row-major forms

-- | Isomorphism between a "JuicyPixels" 'Image' and a "shaped" 'Array'.
juicy :: (Storable a, Storable (PixelBaseComponent a),
          Storable b, Storable (PixelBaseComponent b) )
      => Iso (Image a) (Image b) (Img a) (Img b)
juicy = iso (\(Image w h v)      -> Array (V2 w h) (unsafeCast v))
            (\(Array (V2 w h) v) -> Image w h (unsafeCast v))
{-# INLINE juicy #-}

-- | Isomorphism between a "JuicyPixels" 'Image' and a "shaped" 'Array'
--   with the 'Pixel''s base component.
juicyBase :: forall a b. (Pixel a, Pixel b)
          => Iso (Image a) (Image b) (Img (PixelBaseComponent a)) (Img (PixelBaseComponent b))
juicyBase = iso (\(Image w h v)      -> Array (n *^ V2 w h) v)
                (\(Array (V2 w h) v) -> Image (w `div` n) (h `quot` n) v)
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

testImage :: (Pixel a, Storable a) => Image a -> [V2 Int]
testImage image = filter (\v@(V2 x y) -> pixelAt image x y /= img ^?! ix v) (l ^.. enumShape)
  where
   img@(Array l _) = image ^. juicy

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
fried :: Iso (Manifest a) (Manifest b) (Img a) (Img b)
fried = iso (\(Manifest (Z :. w :. h) v) -> Array (V2 w h) v)
            (\(Array (V2 w h) v)         -> Manifest (Z :. w :. h) v)
{-# INLINE fried #-}

-- juicyFri :: Iso (Image (Juicy a)) (Image (Juicy b)) (Manifest a) (Manifest b)
-- juicyFri = iso (\(Image w h v) -> Manifest (Z :. w :. h) v)
--                (\(Manifest (Z :. w :. h) v) -> Image w h v)
-- {-# INLINE juicyFri #-}

