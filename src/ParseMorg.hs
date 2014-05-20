{-# LANGUAGE TupleSections #-}
module ParseMorg (getConfig, getGenoData ) where

import Common

import Text.Parsec 
import Text.Parsec.String (Parser) -- type Parser = Parsec String ()
import Text.Parsec.Language (javaStyle)
import Text.Parsec.Token as P
import Control.Applicative ((<*),(<*>),(*>),pure,(<$>))
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Control.Lens (set)


getGenoData ::  Config -> String -> (ModelInfo, [Indiv])
getGenoData cfg x = checkGenoData cfg $ parse parseGenoData "marker file" x

getConfig ::  String -> Config
getConfig x = checkConfig $  parse parseConfig ".par file" x



checkConfig :: Either ParseError [(String,String)] -> Config
checkConfig assoc = case assoc of
    Left x -> error $ show x
    Right x -> Config { markerFile = gv "input marker data file"
                      , iterations = gv "iterations"
                      , popKin = gv "population kinship"
                      , changeRate = gv "kinship change rate"
                      , nullFrac = gv "transition matrix null fraction"
                      , seed = gv "seed"
                      , genoError = gv "genotyping error rate"
                      , nParts = gv "particles"
    }
                 where gv :: (Read a) => String -> a
                       gv = getConfigValue x

getConfigValue :: (Read a) => [(String,String)] -> String -> a
getConfigValue assoc name1 = fromMaybe err $ lookup name1 assoc >>= readMaybe 
    where err = error $ "Error reading parameter '" ++ name1 ++ "'"

getOptionalValue :: (Read a) => [(String,String)] -> String -> Maybe a
getOptionalValue assoc name1 = (fromMaybe err . readMaybe) `fmap` lookup name1 assoc-- >>= return $ fromMaybe err readMaybe 
    where err = error $ "Error reading parameter '" ++ name1 ++ "'"


parseGenoData :: Parser (V.Vector PosDifference, V.Vector Double, [Indiv])
parseGenoData = do 
    myRes "map marker positions" 
    pos <- many myFloat
    freq <- many alFreqLine
    _ <- myRes "set marker data" >> myInt
    theIndivs <- many (myIndiv (length pos)) <* eof
    let diffs = Start : fmap Diff (diff pos)
        tup (x:y:_) = (x,y)
        idIndivs = zipWith (set intIds) (map tup $ pairOff [1..]) theIndivs
    return (V.fromList diffs, V.fromList freq, idIndivs)

checkGenoData :: Config -> Either ParseError (V.Vector PosDifference, V.Vector Double, [Indiv]) -> (ModelInfo, [Indiv])
checkGenoData cfg x = case x of 
    Right (pos, frqs, inds) -> 
        if allEqual $ V.length frqs : V.length pos : map (V.length . genos) inds
        then (makeModelInfo cfg pos frqs inds, inds)
        else error "Different numbers of markers and data supplied"
    Left err -> error $ show err

makeModelInfo :: Config -> V.Vector PosDifference -> V.Vector Double -> [Indiv] -> ModelInfo 
makeModelInfo cfg pos frqs inds = ModelInfo { changeRateMI = changeRate cfg
                     , popKinMI = popKin cfg
                     , genoErrorMI = genoError cfg
                     , markerPos = pos
                     , freqs = frqs
                     , nParticles = nParts cfg
                     , nHaps = 2 * length inds 
        }

{-
makeHapIntMap :: [Indiv] -> BMap.Bimap HapId Int 
makeHapIntMap xs = BMap.fromList $ (zip ) (go (matPat . name)) (go (view intIds) )
        where go f = toListOf (folded.both) $ map f xs
-}

-- config parsing
parseConfig :: Parser [(String, String)]
parseConfig = myWs >> endBy (selectMarkersLine <|> line) myWs <* eof

line :: Parser (String, String)
line = selLine <|> priorLine <|> regularLine 

lineBody :: Parser String
lineBody = many1 $ noneOf "\n\r"

regularLine :: Parser (String, String)
regularLine = let snip xs = (unwords $ init $ words xs, last $ words xs)
                  in snip <$> (optional (myRes "set") *> lineBody)
selLine :: Parser (String, String)
selLine = (,"") <$> (myRes "select" *> lineBody )

priorLine :: Parser (String, String)
priorLine = (,) <$> (myRes "set priors" *> return "priors") <*> lineBody 

selectMarkersLine :: Parser (String, String)
selectMarkersLine = (,) <$> (myRes "select markers" *> return "select markers") <*> (show `fmap` many myInt )

-- marker parsing
myWord :: Parser String
myWord = many1 ( noneOf " \t\n\r") <* myWs <?> "word"

myIndiv :: Int -> Parser Indiv
myIndiv n = Indiv <$>  myWord <*> (V.fromList <$> count n myGeno) <*> pure (-999999,-999999) <?> "indiv"

myAllele :: Parser Char
myAllele = oneOf "012" <* myWs  <?> "allele"

myGeno :: Parser (U.Vector Char)
myGeno = listify <$> myAllele <*> myAllele <?> "geno"
    where listify x y = U.fromList [x,y]

alFreqLine :: Parser Double
alFreqLine = try (myRes "set marker" >> myInt) >> count 2 myWord >>
    myFloat <* myFloat <?> "allele freq"

-- parsing setup
def = javaStyle {reservedNames = ["select"
                                 , "map marker positions"
                                 , "set marker"
                                 , "set marker data"
                                 , "set priors"]
                , commentLine = "#" }
TokenParser {reserved = myRes
            , whiteSpace = myWs 
            , float = myFloat
            , integer = myInt}  = makeTokenParser def
-- helpers
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

diff :: [Double] -> [Double]
diff = zipWith (-) =<< tail


