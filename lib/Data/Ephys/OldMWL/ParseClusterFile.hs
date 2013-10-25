module Data.Ephys.OldMWL.ParseClusterFile where

import Data.Ephys.Cluster

import Data.Map
import Text.Parsec
import Control.Monad (liftM, replicateM)
import Control.Applicative ((<$),(<*),(*>),(<*>), liftA)
import Data.Char           (chr)

parseClusters :: String -> Either ParseError (Map Int ClusterMethod)
parseClusters = parse pClusterFile "Cluster file"
aoeu 
pClusterFile :: Parsec String () (Map Int ClusterMethod)
pClusterFile = do
  many pHeader
  assoc <- many pProjectionPoly
  eof
  return $ foldList  assoc

foldList :: [(Int, CartBound)] -> Map Int ClusterMethod
foldList bs = Prelude.foldl appendByKey empty bs

appendByKey :: Map Int ClusterMethod -> (Int, CartBound) -> Map Int ClusterMethod
appendByKey m (k,v) = case Data.Map.lookup k m of
  Nothing                     -> insert k (ClustIntersection [ClustCartBound v]) m
  Just (ClustIntersection vs) -> insert k (ClustIntersection ((ClustCartBound v:vs))) m

{-
pHeader :: Parsec String () (Map Int ClusterMethod) 
pHeader = char '%' *> many (noneOf "\n") *> newline
-}

pHeader :: Parsec String st ()
pHeader = do
  char '%'
  many (noneOf "\n")
  newline
  return ()

pProjectionPoly :: Parsec String () (Int, CartBound)
pProjectionPoly = do
  clustId <- read `liftM` many digit
  newline
  proj1 <- read `liftM` many digit
  spaces
  proj2 <- read `liftM` many digit
  _ <- many (noneOf " \n\t")
  newline
  _ <- many (noneOf " \n\t")
  newline
  nPoint <- read `liftM` many digit
  many (noneOf "\n") >> newline
  pts <- replicateM nPoint $ do
--    x <- read `liftM` many digit <*> char '.' <*> many digit
    x <- pDouble
    spaces
    y <- pDouble
    newline
    return (x,y)
  return $ (clustId, CartBound proj1 proj2 pts)
  
pNumber :: Parsec String () String
pNumber = many1 digit
  
pInt :: Parsec String () String
pInt = pPlus <|> pMinus <|> pNumber
  where pPlus  = char '+' *> pNumber
        pMinus = (:) <$> char '-' <*> pNumber 

pDouble :: Parsec String () Double
pDouble = rd <$> ((++) <$> pInt <*> pDecimal)
  where rd = read :: String -> Double
        pDecimal = option "" ((:) <$> char '.' <*> pNumber)
