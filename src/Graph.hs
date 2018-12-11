{-# LANGUAGE ScopedTypeVariables #-}
module Graph where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Arrow
import Control.Monad
import System.IO (Handle, stdin, hGetLine)
import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (sp, spLength)
import Data.Graph.Inductive.Query.MST (msTree)

someFunc :: IO ()
someFunc = do
  print ns
  print ess
  print $ genGraph (ns, ess)
  solveShortest (ns, ess) 1 6
  solveMST (ns, ess)

ns = 6
ess =
  [ EdgeSpec {fromNode = 1, toNode = 2, distance = 3}
  , EdgeSpec {fromNode = 1, toNode = 3, distance = 4}
  , EdgeSpec {fromNode = 2, toNode = 3, distance = 5}
  , EdgeSpec {fromNode = 2, toNode = 4, distance = 2}
  , EdgeSpec {fromNode = 2, toNode = 5, distance = 6}
  , EdgeSpec {fromNode = 3, toNode = 5, distance = 5}
  , EdgeSpec {fromNode = 4, toNode = 6, distance = 9}
  , EdgeSpec {fromNode = 5, toNode = 4, distance = 1}
  , EdgeSpec {fromNode = 5, toNode = 6, distance = 10}
  ]

data EdgeSpec = EdgeSpec
  { fromNode :: Int
  , toNode :: Int
  , distance :: Int
  } deriving (Eq, Ord, Show)

readInputs :: Handle -> IO (Int, [EdgeSpec])
readInputs hdl = do
  numNodes <- read <$> hGetLine hdl
  numEdges <- read <$> hGetLine hdl
  edges <- replicateM numEdges (readEdge hdl)
  pure (numNodes, edges)

readEdge :: Handle -> IO EdgeSpec
readEdge hdl = do
  inp :: String <- hGetLine hdl
  let [fs, ts, ds] = Prelude.words inp
  pure $ EdgeSpec (read fs) (read ts) (read ds)

newtype NodeLabel= NodeLabel Text
  deriving (Eq, Ord, Show)
type Distance = Int

genGraph :: (Int, [EdgeSpec]) -> Gr NodeLabel Distance
genGraph (numNodes, edgeSpecs) = mkGraph nodes edges
  where
    nodes = (id &&& (NodeLabel . T.pack . show)) <$> [1..numNodes]
    edges = (\(EdgeSpec f t d) -> (f, t, d)) <$> edgeSpecs

solveShortest :: (Int, [EdgeSpec]) -> Int -> Int -> IO ()
solveShortest spec start end = do
  let gr = genGraph spec
  print $ sp start end gr
  print $ spLength start end gr

genUndirectedGraph :: (Int, [EdgeSpec]) -> Gr NodeLabel Distance
genUndirectedGraph (numNodes, edgeSpecs) = mkGraph nodes edges
  where
    nodes = (id &&& (NodeLabel . T.pack . show)) <$> [1..numNodes]
    edges = Prelude.concatMap (\(EdgeSpec f t d) -> [(f, t, d), (t, f, d)]) edgeSpecs

solveMST :: (Int, [EdgeSpec]) -> IO ()
solveMST spec = print $ msTree (genUndirectedGraph spec)
