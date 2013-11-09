{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}


-- | A pedestrian implementation of a directed acyclic graph.
-- Sharing is explicitely represented by using node-level and
-- edge-level identifiers.
-- The module may be convenient to use if your data structure
-- doesn't change often.
--
-- Note: many functions work under the assumption, that the DAG
-- is stored in a topological order.


module Data.DAG
( 
-- * Types
  DAG (..)
, NodeID
, Node (..)
, EdgeID
, Edge (..)

-- * Build
, fromEdges

-- * Query
-- ** Node
, node
, nodeIDs
, nodeIDs'
-- ** Edge
, edge
, edgeIDs
, edgeIDs'

-- * Map
, mapN
, mapE
, forward
, backward
, forwardM
, backwardM

-- -- * Topological sorting
-- , topSort
) where


import           Control.Applicative ((<$))
import           Control.Monad (foldM)
import           Data.List (foldl')
import qualified Data.Map as M


-- | A node identifier.
type NodeID = Int


-- | An edge identifier.
type EdgeID = Int


-- | A node.
data Node a = Node
    { ing   :: [EdgeID]
    , out   :: [EdgeID]
    , value :: a }
    deriving (Eq, Ord, Show, Functor)


-- | Add ingoing edge.
addIng :: EdgeID -> Node a -> Node a
addIng eid x@Node{..} = x {ing = eid:ing}


-- | Add outgoing edge.
addOut :: EdgeID -> Node a -> Node a
addOut eid x@Node{..} = x {out = eid:out}


-- | An edge.
data Edge a = Edge
    { from  :: NodeID
    , to    :: NodeID
    , label :: a }
    deriving (Eq, Ord, Show, Functor)


-- | A lattice is a directed acyclic graph (DAG) with values of type
-- `a` assigned to individual nodes and values of type `b` assigned
-- to individual edges.
data DAG a b = DAG {
    -- | Map of nodes.
      nodeMap  :: M.Map NodeID (Node a)
    -- | Map of edges.
    , edgeMap  :: M.Map EdgeID (Edge b) }
    deriving (Eq, Ord, Show, Functor)
-- The structure changes rarely, so it might be a good idea to
-- store node-level and edge-level values in separate sub-structures.


-------------------------------------------------------------------
-- Build
-------------------------------------------------------------------


-- | Build DAG from a list of edges.
fromEdges :: [Edge a] -> DAG () a
fromEdges xs0 = DAG
    { nodeMap = newNodeMap
    , edgeMap = M.fromList xs }
  where
    -- Edges with identifiers.
    xs = zip [0..] xs0
    -- Resulting map of nodes.
    newNodeMap = foldl' updNodeMap M.empty xs
    -- Updeate node map with a particular edge.
    updNodeMap m (eid, Edge{..})
        = updNode (addOut eid) from
        $ updNode (addIng eid) to m
    updNode f nid m =
        let g Nothing  = Just (f $ Node [] [] ())
            g (Just x) = Just (f x)
        in  M.alter g nid m

-------------------------------------------------------------------
-- Node queries
-------------------------------------------------------------------


-- | Return node identifiers in a topological order.
-- TODO: assumes, that the graph is sorted topologically.
nodeIDs :: DAG a b -> [NodeID]
nodeIDs = map fst . M.toAscList . nodeMap


-- | Return node identifiers in a reverse topological order.
-- TODO: assumes, that the graph is sorted topologically.
nodeIDs' :: DAG a b -> [NodeID]
nodeIDs' = map fst . M.toDescList . nodeMap
-- NOTE: containers 0.5.X are needed, because older versions don't
-- provide the `I.toDescList` function.


-- | Resolve the node identifier.
node :: DAG a b -> NodeID -> Node a
node d x = case M.lookup x (nodeMap d) of
    Nothing -> error "node: invalid identifier"
    Just y  -> y


-- -- | Get the list of preceding, adjacent nodes.
-- prevN :: DAG a b -> NodeID -> [NodeID]
-- prevN = undefined


-------------------------------------------------------------------------
-- Edge queries
-------------------------------------------------------------------------


-- | Return edges in a topological order.  Similar to `nodeIDs`.
edgeIDs :: DAG a b -> [EdgeID]
edgeIDs d = concatMap (ing . node d) (nodeIDs d)


-- | Return DAG edges in a reverse topological order.
-- Similar to `nodeIDs'`.
edgeIDs' :: DAG a b -> [EdgeID]
edgeIDs' d = concatMap (out . node d) (nodeIDs' d)


-- | Resolve the edge identifier.
edge :: DAG a b -> EdgeID -> Edge b
edge d x = case M.lookup x (edgeMap d) of
    Nothing -> error "edge: invalid identifier"
    Just y  -> y


-------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------


-- | Map function over the node values.
mapN :: (a -> c) -> DAG a b -> DAG c b
mapN f dag@DAG{..} = dag { nodeMap = fmap (fmap f) nodeMap }


-- | Map function over the edge values (consider using `fmap` instead).
mapE :: (b -> c) -> DAG a b -> DAG a c
mapE f dag@DAG{..} = dag { edgeMap = fmap (fmap f) edgeMap }


-- | Map the given function over edge values giving it access
-- to the already computed values of the preceding edges.
forward :: (DAG a c -> EdgeID -> c) -> DAG a b -> DAG a c
forward f dag = mapDir f dag (edgeIDs dag)


-- | Map the given function over edge values giving it access
-- to the already computed values of the following edges.
backward :: (DAG a c -> EdgeID -> c) -> DAG a b -> DAG a c
backward f dag = mapDir f dag (edgeIDs' dag)


-- | An abstraction over the `forward` and `backward` functions.
mapDir :: (DAG a c -> EdgeID -> c) -> DAG a b -> [EdgeID] -> DAG a c
mapDir f dag xs =
    foldl' upd dag0 xs
  where
    dag0 = dag { edgeMap = M.empty }
    upd acc x =
        let y = f acc x
            e = y <$ (edgeMap dag M.! x)
            m = M.insert x e (edgeMap acc)
        in  y `seq` e `seq` m `seq` acc { edgeMap = m }


-- | Map the given monadic function over edge values giving it access
-- to the already computed values of the preceding edges.
forwardM
    :: Monad m => (DAG a c -> EdgeID -> m c)
    -> DAG a b -> m (DAG a c)
forwardM f dag = mapDirM f dag (edgeIDs dag)


-- | Map the given monadic function over edge values giving it access
-- to the already computed values of the following edges.
backwardM
    :: Monad m => (DAG a c -> EdgeID -> m c)
    -> DAG a b -> m (DAG a c)
backwardM f dag = mapDirM f dag (edgeIDs' dag)


-- | An abstraction over the `forwardM` and `backwardM` functions.
mapDirM
    :: Monad m => (DAG a c -> EdgeID -> m c)
    -> DAG a b -> [EdgeID] -> m (DAG a c)
mapDirM f dag xs =
    foldM upd dag0 xs
  where
    dag0 = dag { edgeMap = M.empty }
    upd acc x = do
        y <- f acc x
        let e = y <$ (edgeMap dag M.! x)
            m = M.insert x e (edgeMap acc)
        return $ y `seq` e `seq` m `seq` acc { edgeMap = m }


-------------------------------------------------------------------
-- Topological sorting
-------------------------------------------------------------------
-- 
-- 
-- -- | Perform topological sort of the lattice nodes.
-- topSort :: Lattice a b -> Lattice a b
-- topSort = error "topSort: not implemented"
