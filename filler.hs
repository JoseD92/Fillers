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
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Control.Concurrent.Async
import Control.DeepSeq
import Foreign.ForeignPtr
import Control.Monad.ST
import Control.Monad.Primitive
import Control.Monad.Random.Class
import Control.Monad.Trans.Random.Strict

hi = 10000000
lo = 1000000
tam = 10000000

#ifdef x86_64_HOST_ARCH
newChar :: RandomGen g => RandST g s Word64
newChar = getRandom
factor = 8
type WordType = Word64
#else
newChar :: RandomGen g => RandST g s Word32
newChar = getRandom
factor = 4
type WordType = Word32
#endif

files = map ((++ ".random") . show) [0..5]

type RandST g s a = RandT g (ST s) a

modifyM :: (RandomGen g, VSM.Storable a) => VSM.MVector s a -> RandST g s a -> RandST g s ()
modifyM vect m = modifyMAux 0
  where
    max = VSM.length vect
    modifyMAux current
      | max == current = return ()
      | otherwise = m >>= VSM.unsafeWrite vect current >> modifyMAux (current+1)

randomizeVect :: RandomGen g => VSM.MVector s WordType -> g -> ST s g
randomizeVect vect = fmap snd . runRandT (modifyM vect newChar)

makeVect :: RandomGen g => VS.Vector WordType -> g -> IO (VS.Vector WordType,g)
makeVect vect gen = return $ runST $ do
  vectM <- VS.unsafeThaw vect
  g <- randomizeVect vectM gen
  newvect <- VS.unsafeFreeze vectM
  return (newvect,g)

vect2Bs vect = fromForeignPtr (castForeignPtr ptr) 0 (len*factor)
  where (ptr,len) = VS.unsafeToForeignPtr0 vect

myio _ [] _ = return []
myio gen (f:files) vect = do
  (newvect,newGen) <- makeVect vect gen
  BS.writeFile f $ vect2Bs newvect
  putStrLn f
  myio newGen files newvect

main = do
  gen <- TF.newTFGen
  let newVect = VS.replicate tam 0
  myio gen files newVect
