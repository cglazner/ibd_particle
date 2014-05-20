module Main where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Data.List (intercalate, transpose)
import Control.Monad.Reader
import Control.Monad.State
import System.Random.TF.Gen
import System.IO
import Data.Time
import Text.Show.Pretty (ppShow)
import Text.Printf
import GHC.Conc

import System.Environment

import Common
import ParseMorg
import Sim


main :: IO ()
main = do 
          --putStrLn (show fred)
          parFileName <- fmap cmdLineCheck getArgs
          config <- getConfig `fmap` readFile parFileName
          (mi1, theIndivs) <- getGenoData config `fmap` readFile (markerFile config)
--          putStrLn $ show mi1
          let datamat = V.fromList $ map (U.concat) $ transpose $ map (V.toList . genos) theIndivs
          let nubst = V.zipWith3 ModelNode (markerPos mi1) (freqs mi1) datamat
          putStrLn . show $ length $ transpose $ map (V.toList . genos) theIndivs
          let stub = fileStub parFileName
              logfile = stub ++ ".log"
              outfile = stub ++ ".out"
              mixfile = stub ++ ".mix"
          putStrLn $ "Logging to " ++ logfile
          starttime <- getCurrentTime
          appendFile logfile "IBD Particle\n"
          appendFile logfile $ printf "Run starting at %s :\n %s\n" (show starttime) (ppShow config)
          appendFile logfile $ printf "Using %d cores" numCapabilities
          let si = fromIntegral (seed config)
              the_gen = seedTFGen (si, si, si, si)
          putStrLn . show $ V.head datamat
          let torque = (id) `fmap` basicFilter nubst
              borque = take (iterations config) `fmap` iterateM (conditionedFilter nubst . extractTrajectory) torque
              results = flip runReader mi1 $ flip evalStateT the_gen $ borque
 --             outputgraphs = map (printTrajectory theIndivs . extractTrajectory) results
--              outputmix = map extractTrajectoryAncestors results

--          withFile (stub ++ ".out") WriteMode $ \h -> mapM (hPutStrLn h) outputgraphs
 --         withFile (stub ++ ".mixing") WriteMode $ \h -> mapM (hPutStrLn h . intercalate "\t" . map show . V.toList) outputmix
 --
          writeFile outfile ""
          writeFile mixfile ""
          forM_ (zip results [1..]) $ \ (res, i) -> do
              withFile outfile AppendMode $ \h -> (hPutStrLn h . printTrajectory theIndivs . extractTrajectory ) res
              withFile mixfile AppendMode $ \h -> (hPutStrLn h . intercalate "\t" . map show . V.toList . extractTrajectoryAncestors ) res
              putStrLn $ printf "Iteration %d" (i::Int)
          endtime <- getCurrentTime
          let tottime = (diffUTCTime endtime starttime)
          appendFile logfile $ printf "Running took %s, or %s/iteration/particle\n" (show tottime) (show (tottime/( fromIntegral $ iterations config * nParts config)))

ifdigit :: Bool -> Int
ifdigit x = if x then 1 else 0

cmdLineCheck :: [a] -> a
cmdLineCheck x = case x of
                     [] -> error "No .par file specified on command line."
                     (x:_) -> x


runModelSimPR :: ModelInfo -> TFGen -> ModelSimPR a -> a
runModelSimPR m x = flip runReader m . flip evalStateT x 

fileStub :: String -> String
fileStub = takeWhile (/= '.') . reverse . takeWhile (/= '/') . reverse 

