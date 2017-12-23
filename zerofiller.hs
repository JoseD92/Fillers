--------------------------------------------------------------------------------
-- |
-- Module      :  zerofiller.hs
-- Copyright   :  None 2017, Jose Daniel Duran Toro
-- License     :  None
--
-- Maintainer  :  Jose Daniel Duran Toro <jose_daniel_d@hotmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This program create files with zeros as data.
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
import Control.Concurrent.Async
import Control.DeepSeq
import Foreign.ForeignPtr

hi = 10000000
lo = 1000000
tam = 10000000

files = map ((++ ".random") . show) [0..]

newVect :: VS.Vector Word64
newVect = VS.replicate tam 0

makeBs :: VS.Vector Word64 -> ByteString
makeBs vect = fromForeignPtr (castForeignPtr ptr) 0 (len*8)
  where
    (ptr,len) = VS.unsafeToForeignPtr0 vect

myio (f:files) bs = do
  BS.writeFile f bs
  myio files bs

main = myio files $ makeBs newVect