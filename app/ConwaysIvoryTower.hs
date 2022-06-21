{-# LANGUAGE ParallelListComp #-}

module ConwaysIvoryTower (main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.List as L
import Data.Fixed
import Data.HashSet as HS

type Cell = (Int, Int)
type Board = HashSet Cell

-- #TODO Switch over to using indexes with this function.
iterateBoard :: Board -> Board
iterateBoard board = stepCells (HS.toList board) HS.empty
    where stepCells :: [Cell] -> Board -> Board
          stepCells [] accumulator          = accumulator
          stepCells (cell:rest) accumulator = if aliveNeighbors >= 2 && aliveNeighbors < 4
              then stepCells rest (HS.insert cell tryAddOffspring)
              else stepCells rest tryAddOffspring
              where aliveNeighbors :: Int
                    aliveNeighbors = length $ findAlive possibleNeighbors
                    findAlive :: [Cell] -> [Cell]
                    findAlive = L.filter (`HS.member` board)

                    tryAddOffspring :: Board
                    tryAddOffspring
                      | not $ L.null findOffspring = HS.union findOffspring accumulator
                      | otherwise                   = accumulator

                    findOffspring :: Board
                    findOffspring = HS.fromList [deadNeighbor | deadNeighbor <- possibleNeighbors
                                                              , not $ deadNeighbor `HS.member` board
                                                              , not $ deadNeighbor `HS.member` accumulator
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
                                                        , zoom = min zoomMaximum $ max nextZoom zoomMinimum}}
    where gameCamera :: Camera
          gameCamera = camera game

          nextZoom :: Float
          nextZoom = zoom gameCamera + deltaZoom gameCamera * deltaTime
          zoomMinimum :: Float
          zoomMinimum = 0.05
          zoomMaximum :: Float
          zoomMaximum = 100.0



data Camera = Camera { x :: Float,    deltaX :: Float
                     , y :: Float,    deltaY :: Float
                     , zoom :: Float, deltaZoom :: Float}

drawCells :: Game -> Picture
drawCells (Game {board = board, camera = camera}) = 
    scale (zoom camera) (zoom camera) $ translate (-(x camera)) (-(y camera)) $
    scale cellSize cellSize $ pictures $ 
        zipWith (uncurry translate) (L.map floatize $ HS.toList board)
                                    (replicate (HS.size board) (rectangleSolid 1.0 1.0))
    where floatize :: Cell -> (Float, Float)
          floatize (x, y) = (fromIntegral x, fromIntegral y)

-- #TODO Generate correct number of lines for zoom.
drawGrid :: Game -> Picture
drawGrid (Game {camera = camera}) = pictures
    [ translate (-halfHorizontal) 0.0 $ scale 1.0 (zoom camera) $ translate 0.0 horizontalCameraOffset $ pictures 
        [translate 0.0 offset line | line <- replicate horizontalLineCount $ Line [ (0.0, 0.0)
                                                                                  , (horizontalLineSize, 0.0)]
                                   | offset <- horizontalLineOffsets]
    , translate 0.0 (-halfVertical) $ scale (zoom camera) 1.0 $ translate verticalCameraOffset 0.0 $ pictures 
        [translate offset 0.0 line | line <- replicate verticalLineCount $ Line [ (0.0, 0.0)
                                                                                , (0.0, verticalLineSize)]
                                   | offset <- verticalLineOffsets]]
    where horizontalLineSize :: Float
          horizontalLineSize = fromIntegral (fst screenSize)
          halfHorizontal :: Float
          halfHorizontal = horizontalLineSize / 2.0
          verticalLineSize :: Float
          verticalLineSize = fromIntegral (snd screenSize)
          halfVertical :: Float
          halfVertical = verticalLineSize / 2.0

          horizontalLineCount :: Int
          horizontalLineCount = length horizontalLineOffsets
          verticalLineCount :: Int
          verticalLineCount = length verticalLineOffsets

          horizontalLineOffsets :: [Float]
          horizontalLineOffsets = [-halfVertical, -halfVertical + cellSize .. halfVertical]
          verticalLineOffsets :: [Float]
          verticalLineOffsets = [-halfHorizontal, -halfHorizontal + cellSize .. halfHorizontal]

          horizontalCameraOffset :: Float
          horizontalCameraOffset = y camera `mod'` cellSize
          verticalCameraOffset :: Float
          verticalCameraOffset = x camera `mod'` cellSize

drawGame :: Game -> Picture
drawGame game = pictures [ color white $ drawCells game
                         , if showGrid game then color white $ drawGrid game else Blank]



gameInteract :: Event -> Game -> Game
gameInteract (EventKey (Char key) keyState _ _) game =
    case key of
         'w' -> game {camera = gameCamera {deltaY    = if keyState == Down then  scaledMoveSpeed else 0.0}}
         's' -> game {camera = gameCamera {deltaY    = if keyState == Down then -scaledMoveSpeed else 0.0}}
         'a' -> game {camera = gameCamera {deltaX    = if keyState == Down then -scaledMoveSpeed else 0.0}}
         'd' -> game {camera = gameCamera {deltaX    = if keyState == Down then  scaledMoveSpeed else 0.0}}
         '1' -> game {camera = gameCamera {deltaZoom = if keyState == Down then  scaledZoomSpeed else 0.0}}
         '2' -> game {camera = gameCamera {deltaZoom = if keyState == Down then -scaledZoomSpeed else 0.0}}
         'g' -> if keyState == Down then game {showGrid = not $ showGrid game} else game
         _   -> game
    where gameCamera :: Camera
          gameCamera = camera game

          scaledMoveSpeed :: Float
          scaledMoveSpeed = moveSpeed / zoom gameCamera

          scaledZoomSpeed :: Float
          scaledZoomSpeed = zoomSpeed ** zoom gameCamera
gameInteract (EventKey (SpecialKey key) keyState _ _) game =
    case key of
         KeyUp       -> game {camera = gameCamera {deltaY    = if keyState == Down then  scaledMoveSpeed else 0.0}}
         KeyDown     -> game {camera = gameCamera {deltaY    = if keyState == Down then -scaledMoveSpeed else 0.0}}
         KeyLeft     -> game {camera = gameCamera {deltaX    = if keyState == Down then -scaledMoveSpeed else 0.0}}
         KeyRight    -> game {camera = gameCamera {deltaX    = if keyState == Down then  scaledMoveSpeed else 0.0}}
         KeyPageUp   -> game {camera = gameCamera {deltaZoom = if keyState == Down then  scaledZoomSpeed else 0.0}}
         KeyPageDown -> game {camera = gameCamera {deltaZoom = if keyState == Down then -scaledZoomSpeed else 0.0}}
         KeySpace    -> if keyState == Down then game {paused = not $ paused game} else game
         _           -> game
    where gameCamera :: Camera
          gameCamera = camera game

          scaledMoveSpeed :: Float
          scaledMoveSpeed = moveSpeed / zoom gameCamera

          scaledZoomSpeed :: Float
          scaledZoomSpeed = zoomSpeed ** zoom gameCamera
gameInteract (EventKey (MouseButton mouseButton) Down _ position) game =
    case mouseButton of
         LeftButton -> if not $ mouseToCellCoordinates `HS.member` board game
             then game {board = HS.insert mouseToCellCoordinates (board game)}
             else game {board = HS.delete mouseToCellCoordinates (board game)}
         _          -> game
    where mouseToCellCoordinates :: Cell
          mouseToCellCoordinates =
              ( floor $ (fst position / zoom gameCamera + x gameCamera) / cellSize + 0.5
              , floor $ (snd position / zoom gameCamera + y gameCamera) / cellSize + 0.5)

          gameCamera :: Camera
          gameCamera = camera game
gameInteract _ game = game



data Game = Game { board    :: Board
                 , camera   :: Camera
                 , paused   :: Bool
                 , showGrid :: Bool}

newGame :: Game
newGame = Game { board  = HS.empty
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
moveSpeed = 120.0
zoomSpeed :: Float
zoomSpeed = 1.0
cellSize :: Float
cellSize = 8.0

main :: IO ()
main = play (InWindow title screenSize (100, 100)) backgroundColor
            iterationsPerSecond newGame
            drawGame gameInteract iterateGame