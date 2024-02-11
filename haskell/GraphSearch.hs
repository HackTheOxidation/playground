module GraphSearch
  where

import qualified Data.Map as Map
import qualified Data.Set as Set

data Node a = Node a 
  deriving (Read, Show, Eq, Ord)

type Nodes a = Set.Set (Node a)
type Edges a = Map.Map (Node a) (Nodes a)
type Graph a = (Nodes a, Edges a)
type GoalTest a = Node a -> Bool


addUnidirectionalNode :: Ord a => Node a -> Nodes a -> Graph a -> Graph a
addUnidirectionalNode newNode endpoints (nodes, edges) =
  if Set.isSubsetOf endpoints nodes then
    (Set.insert newNode nodes, Map.insert newNode endpoints edges)
  else
    (nodes, edges)

addBidirectionalNode :: Ord a => Node a -> Nodes a -> Graph a -> Graph a
addBidirectionalNode newNode endpoints (nodes, edges) =
  if Set.isSubsetOf endpoints nodes then
    (Set.insert newNode nodes,
     foldl (\m n -> Map.update
             (\v -> Just (Set.insert newNode v)) n m) edges endpoints)
  else
    (nodes, edges)

backtrackSolution :: Ord a => Node a -> Graph a -> [Node a]

search :: Ord a => Node a -> Graph a -> GoalTest a -> Maybe ([Node a])
search currentNode graph test =
  if test currentNode then
    Just $ backtrackSolution currentNode graph
  else



main :: IO ()
main = do
  let a = Node "A"
  let b = Node "B"
  putStrLn ""
  
