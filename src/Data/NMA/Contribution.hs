{-# LANGUAGE BangPatterns, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}

module Data.NMA.Contribution
  ( hmGraph'
  , mapHMGraph'
  , findAStream
  , contributionRow
  , sumContributionRow
  , contributionMatrix
  , HMGraph (..)
  , ContributionRow
  , ContributionMatrix
  ) where

import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.IntMap.Strict as IM
import Data.NMA
import Data.Graph.AdjacencyList
import Data.Graph.AdjacencyList.Network
import Data.Graph.AdjacencyList.BFS

type Contribution = Flow

type ContributionRow = Map.Map ComparisonId Contribution

type ContributionMatrix = Map.Map ComparisonId ContributionRow

data HMGraph = 
  HMGraph { row :: ComparisonId
          , network :: Network
          , vsts :: IM.IntMap TreatmentId
          , tsvs :: Map.Map TreatmentId Vertex
          , contribution :: ContributionRow
          , streams :: [Stream]
          }
          deriving (Show,Eq)

-- | Turn hatmatrix into a Network (graph with flow).
-- |Edges respect the sign of the h matrix elements
-- | with capacity and flow equal to its absolute 
hmGraph' :: HatMatrix -> ComparisonId -> Maybe HMGraph
hmGraph' hm cid =
  let mhmr = Map.lookup cid hm
   in case mhmr of
        Nothing -> Nothing 
        Just hmr -> 
          let directs = Map.keys hmr
              treatments = foldl' (\ac (ComparisonId a b) 
                -> Set.insert a (Set.insert b ac)) Set.empty directs
              vsts = IM.fromList $ zip [1..] (Set.toList treatments)
              tsvs = Map.fromList $ zip (Set.toList treatments) [1..] 
              getVertex tid = fromJust (Map.lookup tid tsvs)
              flows = Map.fromList $ 
                map (\((ComparisonId a b), h) -> 
                  let e = if h > 0 
                             then fromTuple (getVertex a, getVertex b)
                             else fromTuple (getVertex b, getVertex a)
                  in (e, toRational (abs h))) $ Map.toList hmr
              es = Map.keys flows
              tgr = graphFromEdges es
              ntw = Network { graph = tgr
                            , source = getVertex $ this cid
                            , sink = getVertex $ that cid
                            , capacities = flows
                            , flow = flows
                            }
           in Just ( HMGraph { row = cid
                             , network = ntw
                             , vsts = vsts
                             , tsvs = tsvs
                             , contribution = Map.fromList $ zip directs (repeat 0)
                             , streams = []
                             })

-- | Reduces a hatmatrix to the corresponding list of the reduces networks
mapHMGraph' :: HatMatrix -> [HMGraph]
mapHMGraph' hm =
  let rows = Map.keys hm
   in map (\r -> fromJust $ hmGraph' hm r) rows


edgeToComparisonId :: HMGraph -> Edge -> ComparisonId
edgeToComparisonId hmgr (Edge u v) =
  let treat x = fromJust $ IM.lookup x (vsts hmgr)
   in ComparisonId (treat u) (treat v)

data Stream = Stream { path :: [Edge]
                     , φ :: Flow
                     }
                     deriving (Show,Eq)

-- | Removes edges from HMGraph
removeEdges :: HMGraph -> [Edge] -> HMGraph
removeEdges hmgr res = 
  let nes = es \\ res
   in hmgr { network = ((network hmgr) { graph = graphFromEdges nes }) }
  where ntw = network hmgr
        g = graph ntw
        es = edges g

minimumFlow :: HMGraph -> [Edge] -> Flow
minimumFlow hgr path =
  let fls = map (\e -> fromJust $ Map.lookup e flows) path
   in minimum fls
  where ntw = network hgr
        flows = flow ntw

-- | Finds a stream by following the first neighbor of a vertex starting
-- | from the source. The algorithm removes dead end edges arising from
-- | inconcistency introduced by the precision of Hat matrix.
findAStream :: HMGraph -> Maybe Stream
findAStream hgr =
  let ntw = network hgr
      g = graph ntw
      s = source ntw
      t = sink ntw
   in if null (neighbors g s)
         then Nothing
         else 
            let emptyStream = Stream { path = []
                                     , φ = 0
                                     }
                getStream :: Stream -> Vertex -> Maybe Stream
                getStream strm v = 
                  if v == t 
                     then Just strm
                     else
                       let neis = neighbors g v
                        in if null neis 
                            then 
                              let newHgr = removeEdges hgr [(last $ path strm)]
                               in findAStream newHgr
                            else 
                              let nextVertex = head neis
                                  newpath = path strm ++ [(Edge v nextVertex)]
                                  newphi = minimumFlow hgr newpath
                                  newStream = Stream { path = newpath
                                                     , φ = newphi
                                                     }
                               in getStream newStream nextVertex
             in getStream emptyStream s

updateFlow :: HMGraph -> Stream -> Map.Map Edge Flow
updateFlow hgr strm =
  let ntw = network hgr
      g = graph ntw
      s = source ntw
      pth = path strm
      phi = φ strm
      oldflow = flow ntw
      newflow = foldl' (\ac e -> 
        Map.adjust (\ofl -> (-) ofl phi) e ac) oldflow pth
   in newflow

updateContribution :: HMGraph -> Stream -> ContributionRow
updateContribution hgr strm =
  let oldContr= contribution hgr
      newContr = foldl' (\ac e -> 
        Map.adjust ((+) (phi / l)) (edgeToComparisonId hgr e) 
        (Map.adjust ((+) (phi / l)) (edgeToComparisonId hgr (reverseEdge e)) ac)) oldContr pth
   in newContr
  where ntw = network hgr
        g = graph ntw
        s = source ntw
        pth = path strm
        l = fromIntegral $ length pth
        phi = φ strm


-- | Main algorithm for computing the contribution of the row by 
-- |iteratively removing Streams given an algorithm to locate streams and a
-- |HMGraph
contributionRow :: (HMGraph -> Maybe Stream) -> HMGraph -> HMGraph
contributionRow getStream hmgraph =
  let reduceGraph :: HMGraph -> HMGraph
      reduceGraph !hgr =
        let ntw = network hgr
            g = graph ntw
            s = source ntw
            t = sink ntw
            mstream = getStream hgr
         in case mstream of
              Nothing -> hgr
              Just newStream -> 
                let newFlow = updateFlow hgr newStream
                    newContribution = updateContribution hgr newStream
                    newHMGraph = 
                      let emptyEdges = map fst $
                            filter (\(e,f) -> f == 0) $
                                map (\e -> 
                                  (e, fromJust $ Map.lookup e (newFlow))) (path newStream)
                          !rhgr = removeEdges hgr emptyEdges
                       in rhgr { network = (network rhgr) {flow = newFlow}
                               , contribution = newContribution
                               , streams = (streams rhgr) ++ [newStream]
                               }
                 in reduceGraph newHMGraph 
                {-in newHMGraph -}
  in reduceGraph hmgraph

sumContributionRow :: ContributionRow -> Double
sumContributionRow cr = fromRational $ sum $ map snd $ Map.toList cr

contributionMatrix :: (HMGraph -> Maybe Stream) -> HatMatrix -> ContributionMatrix
contributionMatrix streamAlgo hatmatrix =
  let rows = Map.keys hatmatrix
      hmgraphs = map (\r -> fromJust $ hmGraph' hatmatrix r) rows
      contributionRows = map (contributionRow streamAlgo) hmgraphs
   in Map.fromList $
     map (\cr -> (row cr, contribution cr)) contributionRows
