module Data.Ephys.OldMWL.ParseClusterFile where

import Data.Ephys.Cluster

import Data.Map
import Text.Parsec
import Control.Monad (liftM, replicateM)
import Control.Applicative ((<$),(<*),(*>),(<*>),(<$>), liftA)
import Data.Char           (chr)

parseClusters :: String -> Either ParseError (Map Int ClusterMethod)
parseClusters = parse pClusterFile "Cluster file"

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

pClusterFile :: Parsec String () (Map Int ClusterMethod)
pClusterFile = do
  many pHeader
  assoc <- many pProjectionPoly
  eof
  return $ foldList  assoc

pHeader :: Parsec String st ()
pHeader = do
  char '%'
  many (noneOf "\n")
  newline
  return ()

pProjectionPoly :: Parsec String () (Int, CartBound)
pProjectionPoly = do
  newline
  clustId <- read `liftM` many1 digit
  newline
  proj1 <- read `liftM` many1 digit
  char '\t'
  proj2 <- read `liftM` many1 digit
  newline
  many (noneOf "\n\t")
  newline
  many (noneOf "\n\t")
  newline
  nPoint <- read `liftM` many1 digit
  many (noneOf "\n")
  newline
  pts <- replicateM nPoint $ do
    x <- pDouble
    char '\t'
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
