{-# LANGUAGE TupleSections #-}

-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINE \/\/\/
module Kcolor where
import Control.Monad (replicateM)
import Data.Char
import Data.Either
import Data.List (sortBy, find, findIndex, nub, sort)
import qualified Data.Set as Sets
import qualified Maybes as Data.Maybe
import System.Process
import Test.QuickCheck
import Test.QuickCheck.Monadic
import GHC.Base (join)

data Vertex a = Vertex {label :: a, adjacent :: Sets.Set a}
  deriving (Show, Eq)

data Graph a = Graph [Vertex a]
  deriving (Show, Eq)


printVertex :: Show a => Vertex a -> String
printVertex vert = "(" ++ show (Kcolor.label vert) ++ " (" ++ join (map (\x -> show x  ++ " ") (Sets.toList (adjacent vert))) ++ "))"

printGraph :: Show a => Graph a -> String
printGraph (Graph vertexes) = join (map (\x -> printVertex(x) ++ " ") vertexes)

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
      neighbors = concatMap (\(n, adj) -> map (\(a) -> (n, a)) adj) inp
   in foldl addEdge graph neighbors

colorGraph :: Graph Int -> Int -> Maybe [(Int, Char)]
colorGraph g numColors = do
  let vs = vertices g
  let numNodes = length vs
  let colors = allColors numColors numNodes
  colorCombination <- find (isSafeGraph g) colors
  let charColors = map (\c -> chr (ord 'a' + c)) colorCombination
  return (zip vs charColors)


printColoredPair :: (Int, Char) -> String 
printColoredPair (a,b) = "(" ++ show a ++ "" ++ show b ++ ")"

printColoredGraph :: Maybe [(Int, Char)] -> String
printColoredGraph Nothing  = "#f"
printColoredGraph (Just vertexes) = join $ map printColoredPair (sortBy (\(a,_) (b,_) -> compare a b) vertexes) 



kcolor :: [(Int, [Int])] -> Int -> Maybe [(Int, Char)]
kcolor inp = colorGraph (parseGraph inp)

-- TESTS --

newtype GraphGenerator a = GraphGenerator [Either a (a, a)]
  deriving (Show)

instance (Arbitrary a, Ord a) => Arbitrary (GraphGenerator a) where
  arbitrary =
    arbitrarySizedGraphBuilder

sortInsertionOrder :: Ord a => [a] -> [a] -> [(a, a)] -> Gen [Either a (a, a)]
sortInsertionOrder _ labels [] = return $ map Left labels
sortInsertionOrder _ [] es = return $ map Right es
sortInsertionOrder addedVertecies labels edges = do
  let addableEdges = filter (\(u, v) -> elem u addedVertecies && elem v addedVertecies) edges
  x <- elements (map Left labels ++ map Right addableEdges)
  fmap (x :) $ case x of
    Left v -> sortInsertionOrder (v : addedVertecies) (filter (/= v) labels) edges
    Right e -> sortInsertionOrder addedVertecies labels (filter (/= e) edges)

-- the method of building a arbitrary graph is inspired by the functional programing 1 course
arbitrarySizedGraphBuilder :: (Arbitrary a, Ord a) => Gen (GraphGenerator a)
arbitrarySizedGraphBuilder = do
  n <- chooseInt (0, 6)
  e <- chooseInt (0, n * n)
  labels <- fmap nub (vectorOf n arbitrary)
  let edges = [(u, v) | u <- labels, v <- labels, u /= v]
  es <- fmap (take e) (shuffle edges)
  xs <- sortInsertionOrder [] labels es
  return $ GraphGenerator xs

addItem :: Ord a => Graph a -> Either a (a, a) -> Graph a
addItem g (Left v) = addVertex g v
addItem g (Right edge) = addEdge g edge

buildGraph :: Ord a => GraphGenerator a -> Graph a
buildGraph (GraphGenerator xs) = foldl addItem empty xs

graphBuilderVertices :: GraphGenerator a -> [a]
graphBuilderVertices (GraphGenerator xs) = lefts xs

graphBuilderEdges :: GraphGenerator a -> [(a, a)]
graphBuilderEdges (GraphGenerator xs) = rights xs

prop_VerteciesAdded :: GraphGenerator Int -> Bool
prop_VerteciesAdded gb =
  let g = buildGraph gb
   in sort (vertices g) == sort (graphBuilderVertices gb)

prop_EdgesAdded :: GraphGenerator Int -> Bool
prop_EdgesAdded gb =
  let g = buildGraph gb
      edges = graphBuilderEdges gb
   in all (\(l, r) -> Sets.member r (neighbors g l) && Sets.member l (neighbors g r)) edges

prop_MaxNumberColorHonoredAux :: GraphGenerator Int -> Int -> Maybe Bool
prop_MaxNumberColorHonoredAux gb numberOfColors =
  do
    coloredGraph <- colorGraph (buildGraph gb) numberOfColors
    let distinctElements = length . nub
    return $ distinctElements (map snd coloredGraph) <= numberOfColors

prop_MaxNumberColorHonored :: GraphGenerator Int -> Gen Bool
prop_MaxNumberColorHonored gb = do
  d <- chooseInt (0, 5)
  return $ Data.Maybe.fromMaybe True $ prop_MaxNumberColorHonoredAux gb d

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

isSame :: String -> IO String
isSame  = readProcess "../racket/kcolor" [] 
  
  

prop_IsSameAsRacket :: GraphGenerator Int -> Property
prop_IsSameAsRacket g = monadicIO  $ 
  do
    let graph = buildGraph g
    res <- run (isSame ("(kcolor '(" ++ printGraph graph ++ ") 4)"))
    assert ((take 2 res == "#f") == ("#f" == printColoredGraph (colorGraph graph 4)))

test = do
  putStrLn "prop_VerteciesAdded"
  quickCheck prop_VerteciesAdded
  putStrLn "prop_EdgesAdded"
  quickCheck prop_EdgesAdded
  putStrLn "prop_NColorsValid"
  quickCheck prop_NColorsValid
  putStrLn "prop_ColorsValid"
  quickCheck prop_ColorsValid
  putStrLn "prop_MaxNumberColorHonored"
  quickCheck prop_MaxNumberColorHonored
  putStrLn "as"
  quickCheck prop_IsSameAsRacket