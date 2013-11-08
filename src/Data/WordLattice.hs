-- | The module provides word lattices.
--
-- TODO: It maybe a good idea to provide a monadic interface
-- to lattices?


-- | TODO: or maybe Data.Lattice?
module Data.WordLattice
( 
-- * Types
  Lattice
, Node (..)
, NodeID
, Edge (..)
, EdgeID

, topSort
, nodesAsc
, nodesDesc

-- -- * Query
-- -- ** Node
-- , nodes
-- , nodes'
-- , inGo
-- , outGo
-- , nodeVal
-- -- ** Edge
-- , edges
-- , edges'
-- , beg
-- , end
-- , edgeVal
-- 
-- -- * Map
-- , mapN
-- , mapE
-- , mapT
-- , mapT'
) where


import qualified Data.IntMap.Strict as I
import qualified Data.Vector.Unboxed as U


-- | A node identifier.
type NodeID = Int


-- | An edge identifier.
type EdgeID = Int


-- | A node.
data Node a = Node
    { valN  :: a
    , ing   :: U.Vector EdgeID
    , out   :: U.Vector EdgeID }


-- | An edge.
data Edge a = Edge
    { valE  :: a
    , beg   :: NodeID
    , end   :: NodeID }


-- | A lattice is a directed acyclic graph (DAG) with values of type
-- `a` assigned to individual nodes and values of type `b` assigned
-- to individual edges.
data Lattice a b = Lattice {
    -- | Map of nodes.
      nodeMap  :: I.IntMap (Node a)
    -- | Map of edges.
    , edgeMap  :: I.IntMap (Edge b) }


-- | Perform topological sort of the lattice nodes.
topSort :: Lattice a b -> Lattice a b
topSort = error "topSort: not implemented"


-- | Return nodes in an ascending order w.r.t. the node identifiers.
-- See also `topSort`.
nodesAsc :: Lattice a b -> [NodeID]
nodesAsc = map fst . I.toAscList . nodeMap


-- | Return nodes in an descending order w.r.t. the node identifiers.
-- See also `topSort`.
nodesDesc :: Lattice a b -> [NodeID]
nodesDesc = map fst . I.toDescList . nodeMap
-- NOTE: containers 0.5.X are needed, because older versions don't
-- provide the `I.toDescList` function.


-- | A traversal-like mapping function which can be used to map a given
-- function over edge values.  While mapping the elementary function over
-- a particular edge, the function has access to mapping results for all
-- preceding edges in the lattice.
-- mapT :: (Lattice a c -> b -> c) -> Lattice a b -> Lattice a c


-- -------------------------------------------------------------------
-- -- Node queries
-- -------------------------------------------------------------------
-- 
-- 
-- -- | Return DAG nodes in a topological order, starting at the beginning
-- -- of a sentence and ending where the sentence ends.
-- nodes :: Lattice a b -> [Node]
-- nodes = undefined
-- 
-- 
-- -- | Return DAG nodes in a reverse topological order.
-- nodes' :: Lattice a b -> [Node]
-- nodes' = undefined
-- 
-- 
-- -- | Get the list of ingoing edges.
-- inGo :: Lattice a b -> Node -> [Edge]
-- inGo = undefined
-- 
-- 
-- -- | Get the list of outgoing edges.
-- outGo :: Lattice a b -> Node -> [Edge]
-- outGo = undefined
-- 
-- 
-- -- | Get the value assigned to a particular node.
-- nodeVal :: Lattice a b -> Node -> a
-- nodeVal = undefined
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- Edge queries
-- ------------------------------------------------------------------------------
-- 
-- 
-- -- | Return DAG edges in a topological order.  Similar to `nodes`.
-- edges :: Lattice a b -> [Edge]
-- edges = undefined
-- 
-- 
-- -- | Return DAG edges in a reverse topological order.  Similar to `nodes'`.
-- edges' :: Lattice a b -> [Edge]
-- edges' = undefined
-- 
-- 
-- -- | Beginning node of the edge.
-- beg :: Lattice a b -> Edge -> Node
-- beg = undefined
-- 
-- 
-- -- | Ending node of the edge.
-- end :: Lattice a b -> Edge -> Node
-- end = undefined
-- 
-- 
-- -- | Get the value assigned to a particular node.
-- edgeVal :: Lattice a b -> Edge -> b
-- edgeVal = undefined
-- 
-- 
-- ------------------------------------------------------------------------------
-- -- Mapping
-- ------------------------------------------------------------------------------
-- 
-- 
-- -- | Map function over the node values.
-- mapN :: (a -> c) -> Lattice a b -> Lattice c b
-- mapN = undefined
-- 
-- 
-- -- | Map function over the edge values (consider using `fmap` instead).
-- mapE :: (b -> c) -> Lattice a b -> Lattice a c
-- mapE = undefined
-- 
-- 
-- -- | A traversal-like mapping function which can be used to map a given
-- -- function over edge values.  While mapping the elementary function over
-- -- a particular edge, the function has access to mapping results for
-- -- all preceding edges in the lattice.
-- --
-- -- WARNING: if the elementary function tries to access a node, which
-- -- doesn't precede the current node, an error will be raised.
-- mapT :: (Lattice a c -> b -> c) -> Lattice a b -> Lattice a c
-- mapT = undefined
-- 
-- 
-- -- | A traversal-like mapping function similar to `mapT`, with the difference
-- -- that the lattice is traversed in the opposite direction.
-- mapT' :: (Lattice a c -> b -> c) -> Lattice a b -> Lattice a c
-- mapT' = undefined
