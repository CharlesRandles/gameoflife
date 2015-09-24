{- Topologically spherical game of life -}

type Cell = Char
liveCell :: Cell
liveCell = 'O'
deadCell :: Cell
deadCell = '.'
isAlive = not . isDead
isDead c = c == deadCell

data Field = Field {cells :: [[Cell]]}

width (Field f) = length $ head f
height (Field f) = length f

toString [] = ""
toString (l:ls) = show l ++ "\n" ++ toString ls

instance Show Field where
  show (Field cells) = toString cells 

makeEmpty w h = Field
                $ take h
                $ repeat
                $ take w
                $ repeat '.'

replaceNth :: [a] -> a -> Int -> [a]
replaceNth [] _ _ = []
replaceNth (x:xs) a 0 = (a:xs)
replaceNth (x:xs) a n = x:(replaceNth xs a (n-1))

getCell x y f = (cells f) !! y !! x

setCell x y f@(Field c) v = Field (replaceNth c newRow y)
  where newRow = replaceNth (c!!y) v x


neighbours x y f@(Field cells) = [getCell x1 y1 f |
                                  x1 <- map wrapX [x-1..x+1],
                                  y1 <- map wrapY [y-1..y+1],
                                  not (x1==x && y1==y)]
  where wrapX a = a `mod` (width f)
        wrapY a = a `mod` (height f)

liveNeighbours x y f = length $ filter isAlive $ neighbours x y f

field = Field [".....",
               ".AB..",
               "..CD.",
               ".E.F.",
               "..GH.."]

glider = Field [".....",
               ".oo...",
               ".o.o..",
               ".o....",
               "......"]

nextState x y f
  | (isDead cell) && (liveNeighbours x y f == 3) = liveCell
  | (isAlive cell) && (liveNeighbours x y f < 2) = deadCell
  | (isAlive cell) && (liveNeighbours x y f  `elem` [2,3]) = liveCell
  | (isAlive cell) && (liveNeighbours x y f > 3) = deadCell
  | otherwise = deadCell
  where cell = getCell x y f

split _ [] = []
split n xs = (take n xs) : (split n $ drop n xs )

nextGeneration rule f@(Field cells) = Field $ split (width f)
                     [rule x y f |
                      x <- [0..((width f) - 1)],
                      y <- [0..((height f) - 1)]]

ng=nextGeneration

{-
    A rule looks like "Bxyz/Sabc"
    where xyz are the number of neighbours that make a dead cell be born
    and abc are the number of neighbours that allow a cell to survive 
-}

splitToInt :: String -> [Int]
splitToInt chars = map read (split 1 chars)

splitRule = break (\c->c=='/')

parseRule rule = (splitToInt $ fst $ splitRule cleanRule,
                  splitToInt $ tail $snd $ splitRule cleanRule)
                 where cleanRule = filter (\c->c`elem`"012345678/") rule

generator rule = (\x y f ->
                   if (isDead (getCell x y f)) then
                     if (liveNeighbours x y f) `elem` born
                     then liveCell
                     else deadCell
                   else --Cell is live
                     if (liveNeighbours x y f) `elem` survive
                     then liveCell
                          else deadCell)
  where (born, survive) = parseRule rule 
                                 
life = ng $ generator "3/23"

large =
  setCell 2 3
  (setCell 2 2
   (makeEmpty 50 24) 'O') 'O'
