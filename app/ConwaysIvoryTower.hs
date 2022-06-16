{-# LANGUAGE ParallelListComp #-}

module ConwaysIvoryTower (main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.List
import Data.Fixed

type Cell = (Int, Int)
type Board = [Cell]

-- #TODO Optimize with some kind of hashset.
iterateBoard :: Board -> Board
iterateBoard []    = []
iterateBoard board = stepCells board []
    where stepCells :: Board -> Board -> Board
          stepCells [] accumulator          = accumulator
          stepCells (cell:rest) accumulator = if aliveNeighbors >= 2 && aliveNeighbors < 4
              then stepCells rest (cell : tryAddOffspring)
              else stepCells rest tryAddOffspring
              where aliveNeighbors :: Int
                    aliveNeighbors = length $ findAlive possibleNeighbors
                    findAlive :: [Cell] -> [Cell]
                    findAlive = filter (`elem` board)

                    tryAddOffspring :: [Cell]
                    tryAddOffspring
                      | not $ null findOffspring = findOffspring ++ accumulator
                      | otherwise                = accumulator

                    findOffspring :: [Cell]
                    findOffspring = [deadNeighbor | deadNeighbor <- possibleNeighbors
                                                  , deadNeighbor `notElem` board
                                                  , deadNeighbor `notElem` accumulator
                                                  , length (findAlive $ findPossibleNeighbors deadNeighbor) == 3]
                
                    possibleNeighbors :: [Cell]
                    possibleNeighbors = findPossibleNeighbors cell
                    findPossibleNeighbors :: Cell -> [Cell]
                    findPossibleNeighbors cell = [(fst cell + dx, snd cell + dy) | dx <- [-1, 0, 1]
                                                                                 , dy <- [-1, 0, 1]
                                                                                 , (dx, dy) /= (0, 0)]

iterateGame :: Float -> Game -> Game
iterateGame deltaTime game = game { board  = if not $ paused game then iterateBoard (board game) else board game
                                  , camera = gameCamera { x    = x gameCamera + deltaX gameCamera * deltaTime
                                                        , y    = y gameCamera + deltaY gameCamera * deltaTime
                                                        , zoom = zoom gameCamera + deltaZoom gameCamera * deltaTime}}
    where gameCamera :: Camera
          gameCamera = camera game



data Camera = Camera { x :: Float,    deltaX :: Float
                     , y :: Float,    deltaY :: Float
                     , zoom :: Float, deltaZoom :: Float}

drawCells :: Game -> Picture
drawCells (Game {board = board, camera = camera}) = scale (zoom camera) (zoom camera) $ translate (-(x camera)) (-(y camera)) $ 
    scale cellSize cellSize $ translate (cellSize / 2) (cellSize / 2) $ pictures $
        zipWith (uncurry translate) (map floatize board)
                                    (replicate (length board) (rectangleSolid 1.0 1.0))
    where floatize :: Cell -> (Float, Float)
          floatize (x, y) = (fromIntegral x, fromIntegral y)

-- #TODO factor in zoom of camera.
drawGrid :: Game -> Picture
drawGrid (Game {board = board, camera = camera}) = translate horizontalCenteringOffset verticalCenteringOffset $ 
    pictures [ translate 0.0 cameraHorizontalOffset $ scale 1.0 cellSize $ pictures 
                 [translate 0.0 offset line | line <- replicate horizontalLineCount $ Line [(0.0, 0.0), (horizontalLineSize, 0.0)]
                                            | offset <- [0.0 .. fromIntegral horizontalLineCount]]
             , translate cameraVerticalOffset 0.0 $ scale cellSize 1.0 $ pictures 
                 [translate offset 0.0 line | line <- replicate verticalLineCount $ Line [(0.0, 0.0), (0.0, verticalLineSize)]
                                            | offset <- [0.0 .. fromIntegral verticalLineCount]]]
    where horizontalLineSize :: Float
          horizontalLineSize = fromIntegral (fst screenSize)
          verticalLineSize :: Float
          verticalLineSize   = fromIntegral (snd screenSize)

          horizontalLineCount :: Int
          horizontalLineCount = ceiling $ max 200.0 $ 1.0 + fromIntegral (fst screenSize)
          verticalLineCount :: Int
          verticalLineCount   = ceiling $ max 200.0 $ 1.0 +  fromIntegral (snd screenSize)

          cameraHorizontalOffset :: Float
          cameraHorizontalOffset = y camera `mod'` cellSize
          cameraVerticalOffset :: Float
          cameraVerticalOffset   = x camera `mod'` cellSize

          verticalCenteringOffset :: Float
          verticalCenteringOffset   = fromIntegral (fst screenSize) / (-2.0)
          horizontalCenteringOffset :: Float
          horizontalCenteringOffset = fromIntegral (snd screenSize) / (-2.0)

drawGame :: Game -> Picture
drawGame game = pictures [ color white $ drawCells game
                         , if showGrid game then color white $ drawGrid game else Blank]
    where gameCamera :: Camera
          gameCamera = camera game



gameInteract :: Event -> Game -> Game
gameInteract (EventKey (Char key) keyState _ _) game =
    case key of
         'w' -> game {camera = gameCamera {deltaY    = if keyState == Down then moveSpeed  else 0.0}}
         's' -> game {camera = gameCamera {deltaY    = if keyState == Down then -moveSpeed else 0.0}}
         'a' -> game {camera = gameCamera {deltaX    = if keyState == Down then -moveSpeed else 0.0}}
         'd' -> game {camera = gameCamera {deltaX    = if keyState == Down then moveSpeed  else 0.0}}
         '1' -> game {camera = gameCamera {deltaZoom = if keyState == Down then zoomSpeed  else 0.0}}
         '2' -> game {camera = gameCamera {deltaZoom = if keyState == Down then -zoomSpeed else 0.0}}
         'g' -> if keyState == Down then game {showGrid = not $ showGrid game} else game
         _   -> game
    where gameCamera :: Camera
          gameCamera = camera game
gameInteract (EventKey (SpecialKey key) keyState _ _) game =
    case key of
         KeyUp       -> game {camera = gameCamera {deltaY    = if keyState == Down then moveSpeed  else 0.0}}
         KeyDown     -> game {camera = gameCamera {deltaY    = if keyState == Down then -moveSpeed else 0.0}}
         KeyLeft     -> game {camera = gameCamera {deltaX    = if keyState == Down then -moveSpeed else 0.0}}
         KeyRight    -> game {camera = gameCamera {deltaX    = if keyState == Down then moveSpeed  else 0.0}}
         KeyPageUp   -> game {camera = gameCamera {deltaZoom = if keyState == Down then zoomSpeed  else 0.0}}
         KeyPageDown -> game {camera = gameCamera {deltaZoom = if keyState == Down then -zoomSpeed else 0.0}}
         KeySpace    -> if keyState == Down then game {paused = not $ paused game} else game
         _           -> game
    where gameCamera :: Camera
          gameCamera = camera game
gameInteract (EventKey (MouseButton mouseButton) Down _ position) game =
    case mouseButton of
         LeftButton -> if mouseToCellCoordinates `notElem` board game
             then game {board = mouseToCellCoordinates : board game}
             else game {board = delete mouseToCellCoordinates (board game)}
         _          -> game
    where mouseToCellCoordinates :: (Int, Int)
          mouseToCellCoordinates =
              ( floor $ (fst position / zoom gameCamera + x gameCamera) / cellSize - cellSize / 2 + 0.5
              , floor $ (snd position / zoom gameCamera + y gameCamera) / cellSize - cellSize / 2 + 0.5)

          gameCamera :: Camera
          gameCamera = camera game
gameInteract _ game = game



data Game = Game { board    :: Board
                 , camera   :: Camera
                 , paused   :: Bool
                 , showGrid :: Bool}

newGame :: Game
newGame = Game { board  = []
               , camera = Camera { x = 0.0,    deltaX = 0.0
                                 , y = 0.0,    deltaY = 0.0
                                 , zoom = 1.0, deltaZoom = 0.0}
               , paused = True
               , showGrid = False}

title :: String
title = "Conway's Ivory Tower"
-- The size of the screen in pixels.
screenSize :: (Int, Int)
screenSize = (600, 600)
backgroundColor :: Color
backgroundColor = black
-- Number of times to advance the board per second.
iterationsPerSecond :: Int
iterationsPerSecond = 10

moveSpeed :: Float
moveSpeed = 40.0
zoomSpeed :: Float
zoomSpeed = 1.0
cellSize :: Float
cellSize = 8.0

main :: IO ()
main = play (InWindow title screenSize (100, 100)) backgroundColor
            iterationsPerSecond newGame
            drawGame gameInteract iterateGame