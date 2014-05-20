{-# LANGUAGE GeneralizedNewtypeDeriving, ViewPatterns #-}
module Sim (basicFilter, conditionedFilter, extractTrajectory,
           extractTrajectoryAncestors, printTrajectory) where 

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List (intercalate, transpose, sortBy, mapAccumL, group)
import Data.List.Zipper as LZ
import Control.Arrow ((&&&),(***))
import Control.Monad.Reader
import Control.Parallel.Strategies
import Control.Monad.State
import Control.Lens
import System.Random.TF.Gen
import qualified Data.Foldable as DF
import qualified Data.Traversable as DT
import Data.Function (on)
import Data.Word (Word32)
import GHC.Conc



import Common
import BigMultiNom

-- NOODLING 
-- 

-- REAL
--
basicFilter :: V.Vector ModelNode -> ModelSimPR (V.Vector (U.Vector Int, V.Vector Particle, WeightVec))
basicFilter nodes = do
        (pk, n) <- asks (popKinMI &&& nHaps)
        bigB <- asks nParticles
        starting <- replicateM bigB (chineseRestaurantProcess n pk)
        startingshuff <- traverse (shuffle . U.fromList) starting
        evalStateT (traverse doOne nodes) (V.fromList . map (particleify) $ startingshuff)

conditionedFilter :: V.Vector ModelNode -> V.Vector Particle -> ModelSimPR (V.Vector (U.Vector Int, V.Vector Particle, WeightVec))
conditionedFilter nodes cond = do
        (pk, n) <- asks (popKinMI &&& nHaps)
        bigB <- asks nParticles
        starting <- replicateM bigB (chineseRestaurantProcess n pk)
        startingshuff <- traverse (shuffle . U.fromList) starting
        evalStateT (traverse doOneConditional (V.zip nodes cond)) (V.fromList . map (particleify) $ startingshuff)


doOne :: ModelNode -> StateT (V.Vector Particle) ModelSimPR (U.Vector Int, V.Vector Particle, WeightVec)
doOne mn = do
        ps <- get
        (new_particles, ws) <- lift $ stepPoolPar mn ps
        let the_table = setUpTableMulti . getProbVec . normalize  $ ws
        ancestors <- lift $ (U.map (lookupMulti the_table) . U.fromList )`fmap` replicateM (U.length ws) unif 
        let res_particles = V.map (new_particles V.!) (V.convert ancestors)
        put (res_particles)
        return (ancestors, res_particles, ws)


doOneConditional :: (ModelNode, Particle) -> StateT (V.Vector Particle) ModelSimPR (U.Vector Int, V.Vector Particle, WeightVec)
doOneConditional (mn, conditioned) = do
        ps <- V.tail `fmap` get -- chop the old conditioned one
        (new_particles1, ws1) <- lift $ stepPoolPar mn ps -- add back in new conditioned
        condweight <- lift . lift $ calcWeight mn conditioned
        let new_particles = V.cons conditioned new_particles1
            ws = U.cons condweight ws1
            the_table = setUpTableMulti . getProbVec . normalize $ ws
        ancestors <- lift $ (U.cons 0 . U.map (lookupMulti the_table) . U.fromList) `fmap` replicateM (U.length ws - 1) unif 
        let res_particles = V.map (new_particles V.!) (V.convert ancestors)
        put (res_particles)
        return (ancestors, res_particles, ws)


-- We always take the 2nd particle at the end locus (index 1).  This is
-- arbitrary, except it can't be the 1st b/c that one is conditioned to be
-- 0.  
extractTrajectory :: V.Vector (U.Vector Int, V.Vector Particle, WeightVec) -> V.Vector Particle
extractTrajectory v = V.zipWith ((V.!) . view _2) v $ extractTrajectoryAncestors v

extractTrajectoryAncestors :: V.Vector (U.Vector Int, V.Vector Particle, WeightVec) -> V.Vector Int
extractTrajectoryAncestors v = V.prescanr ((U.!) . view _1) 1 v

-- OUTPUT
--
printTrajectory :: [Indiv] -> V.Vector Particle -> String
printTrajectory inds v = printMorgGraph . zip nms . map fglEncode . transpose . V.toList $ V.map (U.toList . fgls) v
        where nms = rep 2 $ map name inds

printMorgGraph :: [(String, FglList)] -> String
printMorgGraph = intercalate "\n" . map ( \ (nm, xs) -> unwords $ nm : "0" : (show . fst . head) xs : show (length xs - 1) : (tail . init . map show . concatMap detup) xs)
    where detup (x, y) = [x, y]

fglEncode ::  [Int] -> FglList
fglEncode = snd . mapAccumL f 0 . group
    where f loc = (snd &&& id) . (head &&& ((+) loc . length))

stepOne :: PosDifference -> Particle -> ModelSimPR Particle
stepOne Start p = return p
stepOne (Diff t) p = go p 0
      where go p' acc = do 
             pk <- asks popKinMI
             dt <- getDwellingTime pk p'
             newp <- jumpPartition pk p'
             if dt + acc > t then return p' else go newp (dt + acc)

stepPool :: ModelNode -> V.Vector Particle -> ModelSimPR (V.Vector Particle, WeightVec)
stepPool mn v = do new_particles <- V.mapM (stepOne (cmDist mn)) v
                   weights <- lift $ calcWeights mn new_particles
                   return (new_particles, weights)

stepPoolPar :: ModelNode -> V.Vector Particle -> ModelSimPR (V.Vector Particle, WeightVec)
stepPoolPar mn v = do
        gen <- get
        let cs = chunkSize numCapabilities (V.length v) -- 
            mychunks = map (V.fromList) $ chunk cs (V.toList v)
            nchunks = length (mychunks)
            ngens = twoPower (nchunks + 1) -- we need one extra gen to pass forward
            splitted = splitn gen ngens
            helper :: (V.Vector Particle, Int) -> ModelSimPR (V.Vector Particle, WeightVec)
            helper (ps, i) = lift $ evalStateT (stepPool mn ps) (splitted (fromIntegral i))
        computed <- mapM helper $ zip mychunks [1..(nchunks + 1)]
        put $ splitted 0 -- this is the extra gen
        return $ ((V.concat *** U.concat) . unzip)  (withStrategy (parTraversable rdeepseq) $ computed)


getDwellingTime :: Double -> Particle -> ModelSimPR Double
getDwellingTime beta p = do
        cr <- asks changeRateMI
        let gs = LZ.fromList . IM.elems $ groupSizes p
            rate = cr * foldlz' (outrates beta) 0 gs
        runif <- unif
        return $ (-1 * log(runif)) / rate

outrates :: Double -> Double -> LZ.Zipper Int -> Double
outrates beta tot (Zip xs (this:ys)) | this == 1 = tot + to_groups
                             | otherwise = tot + (fromIntegral this) * (to_groups + (1 - beta))
        where to_groups = beta * fromIntegral (sum xs + sum ys)


-- WEIGHTING
calcWeight :: ModelNode -> Particle -> ModelPR Double
calcWeight mn ps = do 
                 errprob <- asks genoErrorMI
                 let p = oneFreq mn
                 return . DF.product . IM.map (classProb errprob p) . (`groupOnInt` theData mn) . fgls $ ps

calcWeights :: ModelNode -> V.Vector Particle -> ModelPR WeightVec
calcWeights mn ps = do 
                 errprob <- asks genoErrorMI
                 let p = oneFreq mn
                 return . V.convert . V.map (DF.product . IM.map (classProb errprob p) . (`groupOnInt` theData mn) . fgls) $ ps

-- given e error and p0 prob the true class allele is 0, prob of a string of 1's and 2's
classProb :: Double -> Double -> String -> Double
classProb e p0 xs = p0 * pr (1-e) + (1-p0) * pr e
    where pr p = (p ^ k) * ((1 - p) ^ (j))
          k = (length . filter (=='1'))  xs
          j = (length . filter (=='2')) xs


jumpPartition :: Double -> Particle -> ModelSimPR Particle
jumpPartition beta (Particle fglvec sizes unused) = do
        n <- asks nHaps
        i <- rrange (U.length fglvec) 
        let old_fgl = fglvec U.! i
            (Just old_size, new_sizes') = IM.updateLookupWithKey minusOne old_fgl sizes
            k = n - old_size
            single = old_size == 1
        k' <- rrange k
        u <- unif
        let novel_group = not single && u < (1 - beta)/(1 + (fromIntegral k - 1) * beta)
            new_fgl = if novel_group
                          then head unused
                          else findGroupI k' old_fgl sizes
            new_fglvec = U.unsafeUpd fglvec [(i, new_fgl)]
            new_sizes = IM.alter plusOne new_fgl $ new_sizes'
            new_unused = if single 
                             then  old_fgl:unused
                             else if novel_group 
                                      then tail unused
                                      else unused
        return $ Particle new_fglvec new_sizes new_unused

groupOnInt :: U.Unbox a => U.Vector Int -> U.Vector a -> IM.IntMap [a]
groupOnInt labels values = U.foldr ins IM.empty $ U.zip labels values
    where ins (k,a) = IM.insertWith (++) k [a] 


plusOne :: Maybe Int -> Maybe Int
plusOne (Just x) = Just (x + 1)
plusOne Nothing = Just 1

-- dummy argument for passing to updateLookupWithKey
minusOne :: Int -> Int -> Maybe Int
minusOne _ x | x == 1 = Nothing
             | otherwise = Just (x - 1)

findGroupI :: Int -> Int -> IM.IntMap Int -> Int
findGroupI i ignore = go 0 . IM.toList
        where go j ((fgl,k):ys) | fgl == ignore = go j ys
                                | j + k > i = fgl
                                | otherwise = go (j + k) ys


-- RANDOM UTILITIES
--
--

unif :: ModelSimPR Double
unif = do 
          gen <- get
          let (randw32, newgen) = next gen
              rand :: Double
              rand = fromIntegral randw32 / (1 + fromIntegral (maxBound::Word32))
          put newgen
          return rand

rrange :: Int -> ModelSimPR Int
rrange n = (floor . (* (fromIntegral n))) `fmap` unif 

normalize :: WeightVec -> ProbVec
normalize x = ProbVec $ U.map (/ U.sum x) x

shuffle :: U.Vector Int -> ModelSimPR (U.Vector Int)
shuffle x = do
        rands <- replicateM (U.length x) unif
        let y = map fst . sortBy (compare `on` snd) $ zip (U.toList x) rands
        return $ U.fromList y

rep :: Int -> [a] -> [a]
rep n x = concat $ map (replicate n) x

-- INITIAL SAMPLING FROM PRIOR
chineseRestaurantProcess :: Int -> Double -> ModelSimPR [Int]
chineseRestaurantProcess n beta = go 1 [1]
        where theta = (1 / beta) - 1
              go k xs | k == n = return xs
                      | otherwise = do
                          u <- unif
                          k' <- rrange k
                          let new_fgl = if u < theta/(theta + (fromIntegral k ) ) 
                                            then k + 1
                                            else xs !! k'
                          go (k + 1) (new_fgl:xs)

-- assumes only fgls 1..n used
particleify :: U.Vector Int -> Particle
particleify x = Particle x (IM.map length $ groupOnInt x (U.replicate n ())) (IS.toList diff)
        where n = U.length x
              diff = IS.difference (IS.fromList [1..n]) (IS.fromList $ U.toList x) 




