{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils.CryptoRandomGen
  ( Ref,
    newRef,
    withRandomGen,
    getRandomByteString,
  )
where

import qualified Basement.Block as Block
import qualified Basement.PrimType as Prim
import Basement.Types.OffsetSize (CountOf (..), Offset (..))
import qualified Crypto.Random as Crypto
import Data.Bifunctor (second)
import qualified Data.ByteString as B
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Proxy (Proxy (..))
import Data.Tuple (swap)
import System.Random (RandomGen (..))

-- | A thread-safe, atomic reference to keep the state of a
-- cryptographic random number generator. Use it when it is crucial to
-- make it practically impossible to predict random numbers, having
-- got some of them, e.g. to generate one-time passwords, tokens, etc.
data Ref = forall g. Crypto.DRG g => Ref (IORef g)

-- | Creates a new random state reference, initialized with a random seed.
newRef :: IO Ref
newRef = do
  chaChaDRG <- Crypto.drgNew
  Ref <$> newIORef chaChaDRG

getRandomByteString :: Ref -> Int -> IO B.ByteString
getRandomByteString ref numBytes = withRef ref $ Crypto.randomBytesGenerate numBytes

-- | Performs a pure computation over the random number generator,
-- managing its state within the IO monad. Example:
--
-- @
--     randomDigit <- withRandomGen randomRef $ randomR (0, 9)
-- @
withRandomGen :: Ref -> (forall gen. RandomGen gen => gen -> (a, gen)) -> IO a
withRandomGen ref f = withRef ref (second unDRGRandomGen . f . DRGRandomGen)

withRef :: Ref -> (forall g. Crypto.DRG g => g -> (a, g)) -> IO a
withRef (Ref ref) f = atomicModifyIORef' ref (swap . f)

-- | This adapts whatever instance of 'Crypto.DRG' class to 'RandomGen'
-- class.
newtype DRGRandomGen g = DRGRandomGen {unDRGRandomGen :: g}

instance Crypto.DRG g => RandomGen (DRGRandomGen g) where
  next = generateInt
  split = error "DRGRandomGen: cannot split, the generator is not splittable"

generateInt :: Crypto.DRG g => DRGRandomGen g -> (Int, DRGRandomGen g)
generateInt (DRGRandomGen drg) = (newInt, DRGRandomGen drg')
  where
    (blockOfInt, drg') = generateRandomBlock 1 drg
    newInt = blockOfInt `Block.index` Offset 0

generateRandomBlock :: forall gen ty. (Crypto.DRG gen, Prim.PrimType ty, Ord ty) => Int -> gen -> (Block.Block ty, gen)
generateRandomBlock numElements = Crypto.randomBytesGenerate (elementSize * numElements)
  where
    (CountOf elementSize) = Prim.primSizeInBytes (Proxy :: Proxy ty)
