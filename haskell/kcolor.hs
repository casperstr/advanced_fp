-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINE \/\/\/
module Kcolor where 
import Control.Monad (replicateM)
-- see https://hackage.haskell.org/package/QuickCheck for
-- documentation if you want to write your own tests

import Data.Either
import Data.List (find, findIndex, nub, sort )
import qualified Data.Set as Sets
import qualified Maybes as Data.Maybe
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Char

data Vertex a = Vertex {label :: a, adjacent :: Sets.Set a}
  deriving (Show, Eq)

data Graph a = Graph [Vertex a]
  deriving (Show, Eq)

empty :: Graph a
empty = Graph []

addVertex :: Ord a => Graph a -> a -> Graph a
addVertex (Graph ver) el = Graph (Vertex el Sets.empty : ver)

addEdgeAux :: Ord a => (a, a) -> Vertex a -> Vertex a
addEdgeAux (a, b) (Vertex l ad)
  | l == a = Vertex l (Sets.insert b ad)
  | l == b = Vertex l (Sets.insert a ad)
  | otherwise = Vertex l ad

addEdge :: Ord a => Graph a -> (a, a) -> Graph a
addEdge (Graph ver) (a, b) = Graph (map (addEdgeAux (a, b)) ver)

vertices :: Ord a => Graph a -> [a]
vertices (Graph ver) = map (\(Vertex l _) -> l) ver

neighbors :: Ord a => Graph a -> a -> Sets.Set a
neighbors (Graph ver) e =
  let elem = find (\(Vertex l _) -> l == e) ver
   in maybe Sets.empty adjacent elem

indexForLabel :: Ord a => Graph a -> a -> Maybe Int
indexForLabel (Graph ver) l = findIndex (\(Vertex a _) -> a == l) ver

neighborsIndexes :: Ord a => Graph a -> a -> [Int]
neighborsIndexes g label =
  let neighborLabels = Sets.toList $ neighbors g label
      indexes = mapM (indexForLabel g) neighborLabels
   in Data.Maybe.fromMaybe [] indexes

type Color = Int

type Colors = [Color]

isSafeNode :: Ord a => Graph a -> a -> Color -> Colors -> Bool
isSafeNode graph label color colors =
  let indexes = neighborsIndexes graph label
      colorsNotAllowed = map (colors !!) indexes
   in notElem color colorsNotAllowed

isSafeGraph :: Ord a => Graph a -> Colors -> Bool
isSafeGraph g@(Graph vertices) colors =
  let vertWithColors = zip vertices colors
   in all (\(v, c) -> isSafeNode g (Kcolor.label v) c colors) vertWithColors

allColors :: Int -> Int -> [[Int]]
allColors numColors numNodes = replicateM numNodes [1 .. numColors]

parseGraph :: [(Int, [Int])] -> Graph Int
parseGraph inp = 
  let graph = foldl addVertex empty (map fst inp)
      neighbors = concatMap (\(n,adj) -> map (\(a) -> (n,a)) adj ) inp
  in foldl addEdge graph neighbors
  
kcolor :: [(Int, [Int])] -> Int -> Maybe [(Int, Char)]
kcolor inp numColors = do 
    let g = parseGraph inp
    let vs = vertices g
    let numNodes = length $ vs
    let colors = allColors numColors numNodes
    colorCombination <- find (isSafeGraph g) colors
    let charColors = map (\c -> chr (ord 'a' + c)) colorCombination
    Just(zip vs charColors)
    

newtype GraphGenerator a = GraphGenerator [Either a (a, a)]
  deriving (Show)
    
instance (Arbitrary a, Ord a) => Arbitrary (GraphGenerator a) where
  arbitrary =
    sized arbitrarySizedGraphBuilder 

generateItems :: Ord a => [a] -> [a] -> [(a, a)] -> Gen [Either a (a, a)]
generateItems _ labels [] = return $ map Left labels
generateItems _ [] es = return $ map Right es   
generateItems addedVertecies labels es = do
  let addableEdges = filter (\(u, v) -> elem u addedVertecies && elem v addedVertecies) es
  x <- elements (map Left labels ++ map Right addableEdges)
  fmap (x :) $ case x of
    Left v -> generateItems (v : addedVertecies) (filter (/= v) labels) es
    Right e -> generateItems addedVertecies labels (filter (/= e) es)

arbitrarySizedGraphBuilder :: (Arbitrary a, Ord a) => Int -> Gen (GraphGenerator a)
arbitrarySizedGraphBuilder m = do
  n <- chooseInt (0, m `div` 2)
  e <- chooseInt (0, n * n `div` 2)
  labels <- fmap nub (vectorOf n arbitrary)
  let elements = [(u, v) | u <- labels, v <- labels, u /= v]
  es <- fmap (take e) (shuffle elements)
  xs <- generateItems [] labels es
  return $ GraphGenerator xs

addItem :: Ord a => Graph a -> Either a (a, a) -> Graph a
addItem g (Left v) = addVertex g v
addItem g (Right edge) = addEdge g edge

buildGraph :: Ord a => GraphGenerator a -> Graph a
buildGraph (GraphGenerator xs) = foldl addItem empty xs

graphBuilderVertices :: GraphGenerator a -> [a]
graphBuilderVertices (GraphGenerator xs) = lefts xs

prop_VerteciesAdded :: GraphGenerator Int -> Bool
prop_VerteciesAdded gb =
  let g = buildGraph gb
   in sort (vertices g) == sort (graphBuilderVertices gb)

prop_NColorsValid :: GraphGenerator Int -> Bool
prop_NColorsValid gb =
  let g = buildGraph gb
      c = length $ vertices g
   in isSafeGraph g [0 .. c]

prop_ColorsValid :: GraphGenerator Int -> Gen Bool
prop_ColorsValid gb = do
  let g = buildGraph gb
  let v = vertices g
  colors <- vectorOf (length v) arbitrary
  let vc = zip v colors
  let safe =
        all
          ( \(v, c) ->
              let adj = neighbors g v
               in not (any (\(v, c1) -> c1 == c && Sets.member v adj) vc)
          )
          vc
  return (isSafeGraph g colors == safe)

test = do
  putStrLn "prop_VerteciesAdded 1:"
  quickCheck prop_VerteciesAdded
  putStrLn "prop_NColorsValid 2:"
  quickCheck prop_NColorsValid
  putStrLn "prop_ColorsValid 3:"
  quickCheck prop_ColorsValid