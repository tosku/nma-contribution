{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}

module Data.NMA
  ( HatMatrix
  , ComparisonId (..)
  , TreatmentId (..)
  ) where

import GHC.Generics
import Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe

data TreatmentId = IntId Int
                 | StringId String 
  deriving (Show,Generic,Read,Ord,Eq)
instance ToJSON TreatmentId
instance FromJSON TreatmentId 
  where
    parseJSON = do
      let outint = withScientific "TreatmentId" 
                   $ \tid -> return (IntId (floor tid))
          outstr = withText "TreatmentId" 
                   $ \tid -> return (StringId (T.unpack tid))
       in (\v -> outint v <|> outstr v)

data ComparisonId = ComparisonId TreatmentId TreatmentId
  deriving (Show,Generic)
instance Eq ComparisonId where
  (==) (ComparisonId a b)
       (ComparisonId c d) =
    (a == c && b == d) ||
    (a == d && b == c)
instance Ord ComparisonId where
  compare (ComparisonId a b) (ComparisonId c d) =
    let mina = (min a b)
        minb = (min c d)
        maxa = (max a b)
        maxb = (max c d)
     in if mina /= minb 
           then compare mina minb 
           else compare maxa maxb 
instance ToJSON ComparisonId
instance FromJSON ComparisonId 
  where
    parseJSON = do
      let compstr = withText "ComparisonId" 
                   $ \cid -> do
                     let textToTid tx = 
                           let etx = TR.decimal (T.pack tx)
                            in case etx of
                                 Left ert -> StringId tx
                                 Right (nid,rst) -> case (T.unpack rst) of
                                                      "" -> IntId nid
                                                      _ -> StringId tx
                         comps = splitOn ":" (T.unpack cid)
                      in return $ ComparisonId (textToTid (head comps)) (textToTid (last comps))
       in (\v -> compstr v)


data HElement = HElement { row :: ComparisonId
                         , comparison :: ComparisonId
                         , value :: Double 
                         }
  deriving (Show,Generic,Eq)
instance FromJSON HElement
instance ToJSON HElement

{-type HatMatrix = Map.Map ComparisonId (Map.Map ComparisonId HElement)-}
type HatMatrix = [HElement]
