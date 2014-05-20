-- Functions for creating a lookup table than can be used for repeated
-- fast multinomial samples given input uniformly distributed doubles.
-- Algorithm taken from Kronmal, R. A. & Peterson Jr, A. V. On the alias method for generating random variables from a discrete distribution The American Statistician, Taylor & Francis, 1979, 33, 214-218

module BigMultiNom where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.List (partition)
import Control.Monad.ST
import Debug.Trace

--traceThis = trace =<< show
--
type Table = (U.Vector Double, U.Vector Int)

setUpTableMulti :: U.Vector Double -> Table
setUpTableMulti v = runST $ do
   let vlen = U.length v
       inflated = U.map (* fromIntegral vlen) . nudge $ v
       (bigger_than_1, smaller_than_1) = partition ((>1) . fst) $ zip (U.toList inflated) [0..]
   f <- UM.new vlen
   U.unsafeCopy f inflated
   l <- UM.new vlen
   let go _ [] = return () 
       go [] (j:[]) = UM.unsafeWrite l j (-1) -- should never hit this
       go [] _ = error (trace (show v) "Something went deeply wrong")
       go (k:ks) (j:js) = do
                UM.unsafeWrite l j k
                fk <- UM.unsafeRead f k
                fj <- UM.unsafeRead f j
                let newfk = (fk - (1 - fj))
                UM.unsafeWrite f k newfk
                if newfk < 1 then go (ks) ( (k:js)) else go (k:ks) (js)
   go (map snd bigger_than_1) (map snd smaller_than_1)
   outf <- U.freeze f
   outl <- U.freeze l
   return (outf, outl)

lookupMulti :: Table -> Double -> Int
lookupMulti (f,l) d = if u > (f U.! k) then (l U.! k) else k
                       where (k, u) = properFraction $ fromIntegral (U.length f) * d

-- a kludge: if the weights are equal (which can occasionally happen
-- if there aren't many particles) the sampling thing blows up, 
-- so we jiggle the first one
nudge :: U.Vector Double -> U.Vector Double
{-# INLINE nudge #-}
nudge v = if U.maximum v - U.minimum v < 0.00000001 
              then (U.head v * 1.001) `U.cons` (U.tail v)
              else v
