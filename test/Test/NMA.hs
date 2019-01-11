module Test.NMA where

import           Data.Maybe

import           Data.Aeson
import qualified Data.ByteString.Lazy             as B
import           Data.Graph.AdjacencyList
import           Data.Graph.AdjacencyList.Network
import           Data.Graph.AdjacencyList.DFS as DFS
import qualified Data.Map.Strict                  as Map

import           TestHS

import           Data.NMA
import           Data.NMA.Contribution

{-fastTests :: [Test]-}
{-fastTests = [ -}
              {-test1-}
            {-]-}

ioTests :: [IO Test]
ioTests = [ test1
          , test2
          , test3
          , testdiabetes4
          , test5
          , test6
          , test7
          , test8
          , testlongest
          , testlongest2
          , testlongest3
          , testlongest4
          , teststreams
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
      --putStrLn "row names"
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
            --putStrLn "HMGraph"
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
          --putStrLn $ show str
          return $ testPassed name $ "passed!"

testdiabetes4 :: IO Test
testdiabetes4 = do
  let name = "contributionrow with shortestStream"
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
          let cr = contributionRow shortestStream hmgraph
          let contrsum = fromRational $ sum $ map snd $ Map.toList (contribution cr) :: Double
          --putStrLn $ show cr
          --putStrLn $ show contrsum
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
      let ContributionMatrix conmat = contributionMatrix findAStream hatmatrix
      let contsums = Map.map sumContributionRow conmat
      putStrLn "contribution sum"
      {-putStrLn $ show contsums-}
      case all (\c -> abs (c-1) < 0.001) contsums of
        False -> return $ testFailed name $ ("all 1s", show contsums)
        True  -> return $ testPassed name $ "passed!"

test6 :: IO Test
test6 = do
  let threshold = 0.0008
  let name = "Flows should be less than " ++ (show $ (fromRational threshold :: Double)) ++ " (Leucht)"
  let hmat = "test/Leuchthat.json"
  let getJSON = B.readFile hmat
  ehmraw <- (eitherDecode <$> getJSON) :: IO (Either String HatMatrixRaw)
  case ehmraw of
    Left err -> return $ testFailed name $ ("error json parse",err)
    Right hmraw -> do
      let hatmatrix = hatMatrixFromList hmraw
      let allFlowsAreSmall fls = all (\f -> f < threshold) fls
      let conmat = map (\hgr -> contributionRow findAStream hgr) (mapHMGraph hatmatrix)
      let checkzeroflow = all id $ map (\hgr -> allFlowsAreSmall (flow $ network $ hgr)) conmat
      case checkzeroflow of
        True -> return $ testPassed name $ "passed!"
        False -> return $ testFailed name $ ("not all small", show checkzeroflow)

test7 :: IO Test
test7 = do
  let threshold = 0.0006
  let name = "Checking first path streams. Flows should be less than " ++ (show $ (fromRational threshold :: Double)) ++ " (Leucht)"
  let hmat = "test/Leuchthat.json"
  let getJSON = B.readFile hmat
  ehmraw <- (eitherDecode <$> getJSON) :: IO (Either String HatMatrixRaw)
  case ehmraw of
    Left err -> return $ testFailed name $ ("error json parse",err)
    Right hmraw -> do
      let hatmatrix = hatMatrixFromList hmraw
      let allFlowsAreSmall fls = all (\f -> f < threshold) fls
      let conmat = map (\hgr -> contributionRow findAStream hgr) (mapHMGraph hatmatrix)
      let allflows = map (\hgr -> Map.map (\f -> fromRational f :: Double) (flow $ network $ hgr)) conmat
      let checkzeroflow = all id $ map (\hgr -> allFlowsAreSmall (flow $ network $ hgr)) conmat
      let firstrow = head conmat
      case checkzeroflow of
        True -> return $ testPassed name $ "passed!"
        --True -> return $ testPassed name $ "passed!" <> (show $ streams firstrow)
        False -> return $ testFailed name $ ("not all small", show $ firstrow)

test8 :: IO Test
test8 = do
  let threshold = 0.0006
  let name = "Checking shortest path streams. Flows should be less than " ++ (show $ (fromRational threshold :: Double)) ++ " (Leucht)"
  let hmat = "test/Leuchthat.json"
  let getJSON = B.readFile hmat
  ehmraw <- (eitherDecode <$> getJSON) :: IO (Either String HatMatrixRaw)
  case ehmraw of
    Left err -> return $ testFailed name $ ("error json parse",err)
    Right hmraw -> do
      let hatmatrix = hatMatrixFromList hmraw
      let allFlowsAreSmall fls = all (\f -> f < threshold) fls
      let conmat = map (\hgr -> contributionRow shortestStream hgr) (mapHMGraph hatmatrix)
      let allflows = map (\hgr -> Map.map (\f -> fromRational f :: Double) (flow $ network $ hgr)) conmat
      let checkzeroflow = all id $ map (\hgr -> allFlowsAreSmall (flow $ network $ hgr)) conmat
      let firstrow = head conmat
      let contrsums = show $ map (\r -> (row r,fromRational $ sum (contribution r))) conmat
      let numstreams = show $ map (\r -> (row r,length (streams r))) conmat
      let probrow = head $ filter (\r -> row r == (ComparisonId (IntId 1) (IntId 5))) conmat
      case checkzeroflow of
        True -> return $ testPassed name $ "passed!"
        --True -> return $ testPassed name $ "passed!" <> (show $ streams firstrow)
        --True -> return $ testPassed name $ ((show contrsums) <>"\n" <> (show probrow)
                  -- <>"\n" <> (show $ probrow)
                                           --)
        False -> return $ testFailed name $ ("not all small", show $ firstrow)


testlongest :: IO Test
testlongest = do
  let threshold = 0.0002
  let name = "Checking longest path streams. Flows should be less than " ++ (show $ (fromRational threshold :: Double)) ++ " (diabetes)"
  let hmat = "test/diabetes_indrhat.json"
  let getJSON = B.readFile hmat
  ehmraw <- (eitherDecode <$> getJSON) :: IO (Either String HatMatrixRaw)
  case ehmraw of
    Left err -> return $ testFailed name $ ("error json parse",err)
    Right hmraw -> do
      let hatmatrix = hatMatrixFromList hmraw
      let allFlowsAreSmall fls = all (\f -> f < threshold) fls
      let conmat = map (\hgr -> contributionRow longestStream hgr) (mapHMGraph hatmatrix)
      let allflows = map (\hgr -> Map.map (\f -> fromRational f :: Double) (flow $ network $ hgr)) conmat
      let checkzeroflow = all id $ map (\hgr -> allFlowsAreSmall (flow $ network $ hgr)) conmat
      let firstrow = head conmat
      let wrongRow = filter (\row -> not $ allFlowsAreSmall (flow $ network row) ) conmat
      let contrsums = show $ map (\r -> (row r,fromRational $ sum (contribution r))) conmat
      let flowsums = show $ map (\r -> (row r,fromRational $ sum (flow $ network r))) conmat
      let numstreams = show $ map (\r -> (row r,length (streams r))) conmat
      let probrow = head $ filter (\r -> row r == (ComparisonId (IntId 1) (IntId 5))) conmat
      case checkzeroflow of
        True -> return $ testPassed name $ "passed!"
        --True -> return $ testPassed name $ "passed!" <> (show $ streams firstrow)
        False -> return $ testFailed name $ ("not all small", (show contrsums)
                   <>"\n" <> (show $ probrow)
                                            )

testlongest2 :: IO Test
testlongest2 = do
  let threshold = 0.0006
  let name = "Checking longest path streams. Flows should be less than " ++ (show $ (fromRational threshold :: Double)) ++ " (Leucht)"
  let hmat = "test/Leuchthat.json"
  let getJSON = B.readFile hmat
  ehmraw <- (eitherDecode <$> getJSON) :: IO (Either String HatMatrixRaw)
  case ehmraw of
    Left err -> return $ testFailed name $ ("error json parse",err)
    Right hmraw -> do
      let hatmatrix = hatMatrixFromList hmraw
      let allFlowsAreSmall fls = all (\f -> f < threshold) fls
      let conmat = map (\hgr -> contributionRow longestStream hgr) (mapHMGraph hatmatrix)
      let allflows = map (\hgr -> Map.map (\f -> fromRational f :: Double) (flow $ network $ hgr)) conmat
      let checkzeroflow = all id $ map (\hgr -> allFlowsAreSmall (flow $ network $ hgr)) conmat
      let firstrow = head conmat
      let wrongRow = filter (\row -> not $ allFlowsAreSmall (flow $ network row) ) conmat
      let contrsums = show $ map (\r -> (row r,fromRational $ sum (contribution r))) conmat
      let flowsums = show $ map (\r -> (row r,fromRational $ sum (flow $ network r))) conmat
      let numstreams = show $ map (\r -> (row r,length (streams r))) conmat
      let probrow = head $ filter (\r -> row r == (ComparisonId (IntId 1) (IntId 5))) conmat
      case checkzeroflow of
        True -> return $ testPassed name $ "passed!"
        --True -> return $ testPassed name $ "passed!" <> (show $ streams firstrow)
        False -> return $ testFailed name $ ("not all small", (show contrsums)
                   <>"\n" <> (show $ probrow)
                                            )

testlongest3 :: IO Test
testlongest3 = do
  let threshold = 0.0008
  let name = "Checking longest path streams. Flows should be less than " ++ (show $ (fromRational threshold :: Double)) ++ " (grizelda)"
  let hmat = "test/big_widehat.json"
  let getJSON = B.readFile hmat
  ehmraw <- (eitherDecode <$> getJSON) :: IO (Either String HatMatrixRaw)
  case ehmraw of
    Left err -> return $ testFailed name $ ("error json parse",err)
    Right hmraw -> do
      let hatmatrix = hatMatrixFromList hmraw
      let allFlowsAreSmall fls = all (\f -> f < threshold) fls
      let conmat = map (\hgr -> contributionRow longestStream hgr) (mapHMGraph hatmatrix)
      let allflows = map (\hgr -> Map.map (\f -> fromRational f :: Double) (flow $ network $ hgr)) conmat
      let checkzeroflow = all id $ map (\hgr -> allFlowsAreSmall (flow $ network $ hgr)) conmat
      let firstrow = head conmat
      let wrongRow = filter (\row -> not $ allFlowsAreSmall (flow $ network row) ) conmat
      let contrsums = show $ map (\r -> (row r,fromRational $ sum (contribution r))) conmat
      let flowsums = show $ map (\r -> (row r,fromRational $ sum (flow $ network r))) conmat
      let numstreams = show $ map (\r -> (row r,length (streams r))) conmat
      let probrow = head $ filter (\r -> row r == (ComparisonId (IntId 1) (IntId 5))) conmat
      case checkzeroflow of
        True -> return $ testPassed name $ "passed!"
        --True -> return $ testPassed name $ "passed!" <> (show $ streams firstrow)
        False -> return $ testFailed name $ ("not all small", (show contrsums)
                   <>"\n" <> (show $ probrow)
                                            )

testlongest4 :: IO Test
testlongest4 = do
  let threshold = 0.0008 :: Double
  let name = "Checking longest path streams. contributions should be above " ++ (show $1.0-threshold) ++ " (grizelda)"
  let hmat = "test/big_widehat.json"
  let getJSON = B.readFile hmat
  ehmraw <- (eitherDecode <$> getJSON) :: IO (Either String HatMatrixRaw)
  case ehmraw of
    Left err -> return $ testFailed name $ ("error json parse",err)
    Right hmraw -> do
      let hatmatrix = hatMatrixFromList hmraw
      let ContributionMatrix conmat = contributionMatrix findAStream hatmatrix
      let contsums = Map.map sumContributionRow conmat
      let isOne :: Double -> Bool
          isOne = (\c -> c > (1.0 - threshold))
      let checkallOne = all isOne contsums
      let problematics = Map.filter (not . isOne) contsums
      case checkallOne of
        True -> return $ testPassed name $ "passed!"
        --True -> return $ testPassed name $ "passed!" <> (show $ streams firstrow)
        False -> return $ testFailed name $ ("not all small", (show problematics)
                                            )

teststreams :: IO Test
teststreams = do
  let name = "Streams"
  let hmat = "test/diabetes_indrhat.json"
  let getJSON = B.readFile hmat
  ehmraw <- (eitherDecode <$> getJSON) :: IO (Either String HatMatrixRaw)
  case ehmraw of
    Left err -> return $ testFailed name $ ("error json parse",err)
    Right hmraw -> do
      let hatmatrix = hatMatrixFromList hmraw
      let hatmatrix = hatMatrixFromList hmraw
      let conrows = map (\hgr -> contributionRow findAStream hgr) (mapHMGraph hatmatrix)
      let StreamMatrix streammat = streamMatrixFromConRows conrows
      case Map.null streammat of
        True -> return $ testFailed name $ ("bad streams", (show $ toJSON (StreamMatrix streammat)))
        False -> return $ testPassed name "streams ok"
