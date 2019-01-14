{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.NMA
  ( HatMatrix (..)
  , HatMatrixRaw (..)
  , HatMatrixRow (..)
  , hatMatrixFromList
  , readHatMatrix
  , rowNames
  , ComparisonId (..)
  , this
  , that
  , TreatmentId (..)
  ) where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import           Data.List.Split
import qualified Data.Map.Strict     as Map
import           Data.Maybe
import qualified Data.Text           as T
import qualified Data.Text.Read      as TR
import           GHC.Generics
import           Data.Aeson
import qualified Data.ByteString.Lazy             as B

data TreatmentId = IntId Int
                 | StringId String
  deriving (Generic,Read,Ord,Eq)
instance Show TreatmentId where
  show (IntId tid)    = show tid
  show (StringId tid) = tid
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
  deriving (Generic,Eq,Ord)
instance Show ComparisonId where
  show (ComparisonId a b) =
     show a ++ ":" ++ show b
instance ToJSONKey ComparisonId
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
                                                      _  -> StringId tx
                         comps = splitOn ":" (T.unpack cid)
                      in return $ ComparisonId (textToTid (head comps)) (textToTid (last comps))
       in (\v -> compstr v)

this :: ComparisonId -> TreatmentId
this (ComparisonId a b) = a

that :: ComparisonId -> TreatmentId
that (ComparisonId a b) = b

data HElement = HElement { row        :: ComparisonId
                         , comparison :: ComparisonId
                         , value      :: Double
                         }
  deriving (Show,Generic,Eq)
instance FromJSON HElement
instance ToJSON HElement

type HatMatrixRaw = [HElement]
type HatMatrixRow = Map.Map ComparisonId Double
type HatMatrix = Map.Map ComparisonId HatMatrixRow

hatMatrixFromList :: HatMatrixRaw -> HatMatrix
hatMatrixFromList hmr =
  let hml =
        map (\he ->
          (row he, Map.singleton (comparison he) (value he))) hmr
   in Map.fromListWith Map.union hml

rowNames :: HatMatrix -> [String]
rowNames hm =
  map show $ Map.keys hm

-- | Read hatmatrix from json file
readHatMatrix :: String -> IO (Either String HatMatrix)
readHatMatrix filename = do
  let getJSON = B.readFile filename
  ehmraw <- (eitherDecode <$> getJSON) :: IO (Either String HatMatrixRaw)
  case ehmraw of
      Left err -> return $ Left ("error json parsing: " <> err)
      Right hmraw -> return $ Right $ hatMatrixFromList hmraw
