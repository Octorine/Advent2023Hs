module Day10 (day10) where
import Game.Advent
import Paths_Advent2023Hs (getDataFileName)
import qualified Data.Map as M
import Data.Maybe

datafile filename = getDataFileName filename >>= readFile

{- | Part 1.  Short description of the problem.
>>> d10p1 "day10-ex.txt"
"2"
-}
d10p1 filename = do
  input <- datafile filename
  let grid = makeGrid . lines $ input
  let start = gridStart grid
  return  . show $ length (getPath grid) `div` 2

getPath :: Grid -> [Coords]
getPath g =
  go g
     (gridStart g)
     (head (outPaths (gridStart g) (gridGraph g)))
     []
  where go g c prev path =
          let outs = filter (/= prev) (outPaths c (gridGraph g))
          in if head outs == gridStart g then c : path
             else go g (head outs) c (c : path)
        outPaths c g = case M.lookup c g of
                         Just (Hall outs) -> outs
                         other -> error ("Tried to find outpaths from "
                                          ++ show other
                                          ++ " at "
                                          ++ show c)


data Coords = Coords{ x :: !Int, y :: !Int}
  deriving (Eq, Ord)

instance Show Coords where
  show (Coords x y) = "<" ++ show x ++ ", " ++ show y ++ ">"
data Grid  = Grid {gridStart :: Coords,
                   gridGraph :: M.Map Coords Space}
  deriving (Show)

data Space = Hall [Coords] | Empty
  deriving (Show)

spaceOuts :: Space -> [Coords]
spaceOuts (Hall outs) = outs
spaceOuts Empty = []


-- Make a grid from a list of strings.  Bottom left character is 0, 0.
-- Right is positive x.  Up is positive y
makeGrid :: [String] -> Grid
makeGrid rows = go (Coords 0 (length rows - 1)) Nothing rows M.empty
  where go c mStart [] m = Grid{ gridStart = fromJust mStart,
                                 gridGraph = addStart (fromJust mStart) m}
        go c mStart ([]: rows) m = go (lf c) mStart rows m
        go c mStart (('S' : xs) : rows) m = go (right c) (Just c) (xs : rows) m
        go c mStart (('.' : xs) : rows) m = go (right c) mStart (xs : rows) m
        go c mStart ((x : xs) : rows) m =
          go (right c)
             mStart
             (xs : rows)
             (M.insert c (Hall (getOuts  c x)) m)

        clamp :: Coords -> Bool
        clamp (Coords x y) = x >= 0 && y >= 0
                             && x < length (head rows)
                             && y < length rows
addStart :: Coords -> M.Map Coords Space -> M.Map Coords Space
addStart start m = M.insert start outs m
  where outs = Hall . M.keys
          . M.filter hasStart
          $ m
        hasStart (Hall outs) = start `elem` outs
        hasStart _ = False
right, left, up, down, lf :: Coords -> Coords
right (Coords x y) = Coords (x + 1) y
left (Coords x y) = Coords (x - 1) y
up (Coords x y) = Coords x (y + 1)
down (Coords x y) = Coords x (y - 1)
lf (Coords x y) = Coords 0 (y - 1)

getOuts c 'L' = [up c, right c]
getOuts c 'F' = [down c, right c]
getOuts c '|' = [up c, down c]
getOuts c '-' = [left c, right c]
getOuts c '7' = [left c, down c]
getOuts c 'J' = [up c, left c]
getOuts c _ = []

makeCharMap :: [String] -> M.Map Coords Char
makeCharMap rows = M.fromList $ do
  (row, rowNum) <- (rows `zip` [0..])
  (c, colNum) <- (row `zip` [0..])
  return (Coords colNum rowNum, c)


{- | Part 2.  Short description of the problem.
>>> d10p2 "day10-ex.txt"
"TODO"
-}
d10p2 filename = do
  input <- datafile filename
  return . show  . countInnerSpaces . fixStart . removeJunk $ input
{- Walk through the cells counting the number of spaces ('.') that are
 inside the loop.  We determine if a space is inside as follows:

If there are an odd number of walls between the space and the left
margin, it's inside.  Any '|' char counrs as a wall.

However, a 'L' any numbrer of '-', and a '7' also counts as a wall, as
does a 'F' adn number of '-'s adn a 'J'.
-}

data CountingState  = CS {
  csCoords :: !Coords,
  csInner :: !Bool,
  csHoriz :: !(Maybe Char),
  csRows :: [String],
  csAcc :: !Int
  } deriving (Show)

csRight :: CountingState -> CountingState
csRight(CS coords inner horiz ((first : rest) : rows) acc) = CS  (right coords) inner horiz (rest : rows) acc

csLf  :: CountingState -> CountingState
csLf (CS coords inner horiz (row : rows) acc) = CS (lf coords) False Nothing rows acc

csFlipInner :: CountingState -> CountingState
csFlipInner (CS coords inner horiz rows acc) = CS coords (not inner) Nothing rows acc

csBump  :: CountingState -> CountingState
csBump (CS coords inner horiz rows acc) = acc `seq` CS coords inner horiz rows ( 1 + acc)

csSetHoriz :: Char -> CountingState -> CountingState
csSetHoriz h (CS coords inner horiz rows acc) = CS coords inner (Just h) rows acc

csNotHoriz  :: CountingState -> CountingState
csNotHoriz (CS coords inner horiz rows acc) = CS coords inner Nothing rows acc

countInnerSpaces txt = go (CS (Coords 0 0) False Nothing rows 0)
  where rows = lines txt
        go cs | null . csRows $ cs = csAcc cs
        go cs | null . head . csRows $ cs = go (csLf cs)
        go cs = case head (head (csRows cs)) of
          '|' -> go . csRight . csFlipInner $ cs
          '.' -> go . csRight . (if csInner cs then csBump else id) $ cs
          '-' -> go . csRight $ cs
          'L' -> go . csRight . csSetHoriz 'L' $ cs
          'F' -> go . csRight . csSetHoriz 'F' $ cs
          'J' -> case csHoriz cs of
            (Just 'L') -> go . csRight . csNotHoriz $ cs
            (Just 'F') -> go . csFlipInner . csRight $ cs
            Nothing -> go . csRight $ cs
          '7' -> case csHoriz cs of
            (Just 'L') -> go . csFlipInner . csRight $ cs
            (Just 'F') -> go . csRight . csNotHoriz $ cs
            Nothing -> go . csRight $ cs

part2ex = unlines $ [".S-------7.",
                     ".|F-----7|.",
                     ".||.||..||.",
                     ".||.....||.",
                     ".|L-7.F-J|.",
                     ".|..|.|..|.",
                     ".L--J.L--J.",
                     "..........."]

coordMinus (Coords x1 y1) (Coords x2 y2) = Coords (x1 - x2) (y1 - y2)

fromOuts :: [Coords]-> Char
fromOuts outs = case outs of  [Coords ~1  0, Coords 1 0] -> '-'
                              [Coords 0 1, Coords 0 (-1)] -> '|'
                              [Coords 1 0, Coords 0 1] -> 'L'
                              [Coords (-1) 0, Coords 0 1] -> 'J'
                              [Coords 1 0, Coords 0 (-1)] -> 'F'
                              [Coords (-1) 0, Coords 0 (-1)] -> '7'
                              [c1, c2] -> fromOuts [c2, c1]

fixStart txt = map (\c -> if c == 'S' then  realStart else c) txt
  where g = makeGrid  (lines txt)
        startCoords = gridStart g
        realStart = fromOuts . map (`coordMinus` startCoords) . spaceOuts . fromJust . M.lookup (gridStart g) $ gridGraph g

removeJunk :: String -> String
removeJunk txt = pathString
  where rows = lines txt
        width = length (head rows)
        height = length rows
        g = makeGrid rows
        cg = makeCharMap rows
        path = getPath g
        isOnPath (Coords x y) b = elem (Coords x (height - y - 1)) path
        pathString  = unmap width height
          . M.filterWithKey isOnPath $ cg

unmap :: Int -> Int -> M.Map Coords Char -> String
unmap w h g = unlines $ map makeRow [0 .. h - 1]
  where makeRow r = map (makeCol r) [0 .. w - 1]
        makeCol r c = fromMaybe '.' $  M.lookup  (Coords c r)  g

day10 :: Day
day10 =
  Day
    { dayName = "10",
      dayPart1 = d10p1 "day10.txt",
      dayPart2 = d10p2 "day10.txt"
    }
