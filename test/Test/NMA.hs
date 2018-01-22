module Test.NMA where

import Data.Maybe

import           Data.Aeson
import qualified Data.ByteString.Lazy  as B

import TestHS

import Data.NMA

{-fastTests :: [Test]-}
{-fastTests = [ -}
              {-test1-}
            {-]-}

ioTests :: [IO Test]
ioTests = [ 
              test1
          ]

test1 :: IO Test
test1 = do
  let hmat = "test/Leucht.json"
  let getJSON = B.readFile hmat
  mhatmatrix <- (eitherDecode <$> getJSON) :: IO (Either String HatMatrix)
  putStrLn "showing test.json"
  putStrLn $ show mhatmatrix
  let name = "Hatmatrix to graph"
      out = "μάγκας"
      expe = "magas"
   in case  out == expe of
        True -> return $ testPassed name "passed!"
        False -> return $ testFailed name $ (,) (show expe) (show out)
