--------------------------------------------------------------------------------
-- |
-- Module      :  filler.hs
-- Copyright   :  None 2017, Jose Daniel Duran Toro
-- License     :  None
--
-- Maintainer  :  Jose Daniel Duran Toro <jose_daniel_d@hotmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This program create files with random data.
--
--------------------------------------------------------------------------------

import qualified System.Random.TF as TF
import System.Random
import qualified Data.ByteString as BS
import Data.ByteString.Internal
import Control.Monad
import Data.Word
import Control.Monad.Random
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Control.Concurrent.Async
import Control.DeepSeq
import Foreign.ForeignPtr
import Control.Monad.ST
import Control.Monad.Primitive

hi = 10000000
lo = 1000000
tam = 10000000

files = map ((++ ".random") . show) [0..]

type RandST g s a = RandT g (ST s) a

modifyM :: (RandomGen g, VSM.Storable a) => g -> VSM.MVector s a -> (g -> ST s (a,g)) -> ST s g
modifyM gen vect m = modifyMAux gen max 0 vect m
  where max = VSM.length vect

modifyMAux :: (RandomGen g, VSM.Storable a) => g -> Int -> Int -> VSM.MVector s a -> (g -> ST s (a,g)) -> ST s g 
modifyMAux gen max current vect m
  | max == current = return gen
  | otherwise = do
    (a,newGen) <- m gen
    VSM.unsafeWrite vect current a
    modifyMAux newGen max (current+1) vect m

newChar32 :: RandomGen g => g -> ST s (Word32,g)
newChar32 = return . random

newChar64 :: RandomGen g => g -> ST s (Word64,g)
newChar64 = return . random

randomizeVect :: RandomGen g => g -> VSM.MVector s Word64 -> ST s g
randomizeVect gen vect = modifyM gen vect newChar64

makeVect :: RandomGen g => VS.Vector Word64 -> g -> IO (VS.Vector Word64,g)
makeVect vect gen = return $ runST $ do
  vectM <- VS.unsafeThaw vect
  g <- randomizeVect gen vectM
  newvect <- VS.unsafeFreeze vectM
  return (newvect,g)

vect2Bs vect = fromForeignPtr (castForeignPtr ptr) 0 (len*8)
  where (ptr,len) = VS.unsafeToForeignPtr0 vect

myio gen (f:files) vect = do
  (newvect,newGen) <- makeVect vect gen
  BS.writeFile f $ vect2Bs newvect
  myio newGen files newvect

main = do
  gen <- TF.newTFGen
  let newVect = VS.replicate tam 0
  myio gen files newVect





