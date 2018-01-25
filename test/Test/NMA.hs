module Test.NMA where

import Data.Maybe

import           Data.Aeson
import qualified Data.ByteString.Lazy  as B
import qualified Data.Map.Strict as Map
import Data.Graph.AdjacencyList
import Data.Graph.AdjacencyList.Network

import TestHS

import Data.NMA
import Data.NMA.Contribution

{-fastTests :: [Test]-}
{-fastTests = [ -}
              {-test1-}
            {-]-}

ioTests :: [IO Test]
ioTests = [ test1
          , test2
          , test3
          , test4
          , test5
          , test6
          ]

test1 :: IO Test
test1 = do
  let name = "read hatmatrix json"
  let hmat = "test/big_widehat.json"
  let getJSON = B.readFile hmat
  ehmrow <- (eitherDecode <$> getJSON) :: IO (Either String HatMatrixRaw)
  case ehmrow of
    Left err -> return $ testFailed name $ ("error json parse",err)
    Right hmrow -> do
      let hatmatrix = hatMatrixFromList hmrow
      putStrLn "row names"
      return $ testPassed name $ "passed!"

test2 :: IO Test
test2 = do
  let name = "hatmatrix row into HMGraph"
  {-let hmat = "test/Donghat.json"-}
  let hmat = "test/big_widehat.json"
  let getJSON = B.readFile hmat
  ehmraw <- (eitherDecode <$> getJSON) :: IO (Either String HatMatrixRaw)
  case ehmraw of
    Left err -> return $ testFailed name $ ("error json parse",err)
    Right hmraw -> do
      let hatmatrix = hatMatrixFromList hmraw
      {-let comp = ComparisonId (IntId 1) (IntId 2)-}
      {-let comp = ComparisonId (StringId "ICS") (StringId "LABA") -}
      let comp = ComparisonId (StringId "agom") (StringId "amit") 
      let row = fromJust $ Map.lookup comp hatmatrix
      let mhmgraph = hmGraph' hatmatrix comp
      case mhmgraph of
        Nothing -> return $ testFailed name $ ("didn't find row", show comp)
        Just hmgraph -> do
            putStrLn "HMGraph"
            {-putStrLn $ show hmgraph-}
            return $ testPassed name $ "passed!"

test3 :: IO Test
test3 = do
  let name = "findAStream"
  let hmat = "test/diabetes_indrhat.json"
  let getJSON = B.readFile hmat
  ehmraw <- (eitherDecode <$> getJSON) :: IO (Either String HatMatrixRaw)
  case ehmraw of
    Left err -> return $ testFailed name $ ("error json parse",err)
    Right hmraw -> do
      let hatmatrix = hatMatrixFromList hmraw
      let comp = ComparisonId (IntId 5) (IntId 6)
      let row = fromJust $ Map.lookup comp hatmatrix
      let mhmgraph = hmGraph' hatmatrix comp
      case mhmgraph of
        Nothing -> return $ testFailed name $ ("didn't find row", show comp)
        Just hmgraph -> do
          let str = findAStream hmgraph
          putStrLn $ show str
          return $ testPassed name $ "passed!"

test4 :: IO Test
test4 = do
  let name = "contributionrow with findAStream"
  let hmat = "test/diabetes_indrhat.json"
  let getJSON = B.readFile hmat
  ehmraw <- (eitherDecode <$> getJSON) :: IO (Either String HatMatrixRaw)
  case ehmraw of
    Left err -> return $ testFailed name $ ("error json parse",err)
    Right hmraw -> do
      let hatmatrix = hatMatrixFromList hmraw
      let comp = ComparisonId (IntId 2) (IntId 3)
      let row = fromJust $ Map.lookup comp hatmatrix
      let mhmgraph = hmGraph' hatmatrix comp
      case mhmgraph of
        Nothing -> return $ testFailed name $ ("didn't find row", show comp)
        Just hmgraph -> do
          let cr = contributionRow findAStream hmgraph
          let contrsum = fromRational $ sum $ map snd $ Map.toList (contribution cr) :: Double
          putStrLn $ show cr
          putStrLn $ show contrsum
          return $ testPassed name $ "passed!"

test5 :: IO Test
test5 = do
  let name = "Contributions should sum to 1 (grizelda)"
  let hmat = "test/big_widehat.json"
  let getJSON = B.readFile hmat
  ehmraw <- (eitherDecode <$> getJSON) :: IO (Either String HatMatrixRaw)
  case ehmraw of
    Left err -> return $ testFailed name $ ("error json parse",err)
    Right hmraw -> do
      let hatmatrix = hatMatrixFromList hmraw
      let conmat = contributionMatrix findAStream hatmatrix
      let contsums = Map.map sumContributionRow conmat
      putStrLn "contribution sum"
      {-putStrLn $ show contsums-}
      case all (\c -> abs (c-1) < 0.001) contsums of
        False -> return $ testFailed name $ ("all 1s", show contsums)
        True -> return $ testPassed name $ "passed!"

test6 :: IO Test
test6 = do
  let threshold = 0.00081
  let name = "Flows should be less than " ++ (show $ (fromRational threshold :: Double)) ++ " (Leucht)"
  let hmat = "test/Leuchthat.json"
  let getJSON = B.readFile hmat
  ehmraw <- (eitherDecode <$> getJSON) :: IO (Either String HatMatrixRaw)
  case ehmraw of
    Left err -> return $ testFailed name $ ("error json parse",err)
    Right hmraw -> do
      let hatmatrix = hatMatrixFromList hmraw
      let firsthmgraph = fromJust $ hmGraph' hatmatrix (fst ( fromJust ( Map.lookupMin hatmatrix)))
      let hmgraph = contributionRow findAStream firsthmgraph
      let getflows hgr = Map.map (\f->fromRational f::Double) (flow $ network $ hgr)
      let allFlowsAreSmall fls = all (\f -> f < threshold) fls
      let conmat = map (\hgr -> contributionRow findAStream hgr) (mapHMGraph' hatmatrix)
      let checkzeroflow = all id $ map (\hgr -> allFlowsAreSmall (flow $ network $ hgr)) conmat
      case checkzeroflow of
        True -> return $ testPassed name $ "passed!"
        False -> return $ testFailed name $ ("not all small", show checkzeroflow)
