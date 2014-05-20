{-# LANGUAGE TemplateHaskell #-}    
module Types where

import Control.Lens 
import Control.Monad.Reader
import Control.Monad.State (StateT)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.Random.TF.Gen
import qualified Data.IntMap as IM
import Control.DeepSeq (NFData(..))

--- TYPES
type WeightVec = U.Vector Double
data ProbVec = ProbVec { getProbVec :: WeightVec } deriving Show

data Particle = Particle { fgls :: U.Vector Int
                         , groupSizes :: IM.IntMap Int
                         , unusedFgls :: [Int]
                         } deriving (Read)
instance Show Particle where
        show x = "Particle" ++ (show $ fgls x)
instance NFData Particle where
        rnf x = rnf (fgls x, groupSizes x, unusedFgls x)


data PosDifference = Diff {getDiff::Double} | Start deriving Show




data Indiv = Indiv {name::String
                  , genos::V.Vector (U.Vector Char) 
                  , _intIds::(Int, Int) } deriving Show
makeLenses ''Indiv


type PhaseInput = (String, [Int]) 

data ModelNode = ModelNode {cmDist :: PosDifference
                          , oneFreq ::Double
                          , theData :: U.Vector Char } deriving Show

-- for particle filtering
data ModelInfo = ModelInfo { changeRateMI :: Double 
                             , popKinMI :: Double
                             , genoErrorMI :: Double
                             , freqs :: V.Vector Double
                             , markerPos :: V.Vector PosDifference
                             --, hapIntMap :: BMap.Bimap HapId Int
                             , nParticles :: Int
                             , nHaps :: Int } deriving (Show)



type ModelPR = Reader ModelInfo
type ModelSimPR = StateT TFGen (Reader ModelInfo)

type FglList = [(Int, Int)] -- (fgl, location)
type StateList = [(Int, Int)]
--type IbdResults' = (IbdResults, V.Vector (DP.Partition HapId))
type IbdState = [Int]


data Config = Config { markerFile :: FilePath
                     , iterations :: Int
                     , seed :: Int
                     , popKin :: Double
                     , changeRate :: Double
                     , nullFrac :: Double
                     , nParts :: Int
                     , genoError :: Double }

                       deriving Show
