{-# LANGUAGE ParallelListComp #-}

module ConwaysIvoryTower (main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.List as L
import Data.Fixed
import Data.HashSet as HS

-- |An individual cell on a board, stored as it's (x, y) position.
type Cell = (Int, Int)
-- |Represents an instance of a board of Conway's Game of Life.
-- Any cells present in the HashSet are alive, any that are not are dead.
type Board = HashSet Cell

-- |Advances a gameboard 1 generation forward.
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
                      | otherwise                  = accumulator

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

-- |Advances a game one step forward.
-- The first argument is the amount of time in seconds that this step occupies.
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
          zoomMaximum = 10.0



-- |Represents where and how to render the game.
data Camera = Camera { x :: Float,    deltaX :: Float
                     , y :: Float,    deltaY :: Float
                     , zoom :: Float, deltaZoom :: Float}

-- |Draws the cells onto the screen.
drawCells :: Game -> Picture
drawCells (Game {board = board, camera = camera}) = 
    scale (zoom camera) (zoom camera) $ translate (-x camera) (-y camera) $
    scale cellSize cellSize $ pictures $ 
        zipWith (uncurry translate) (L.map floatize $ HS.toList board)
                                    (replicate (HS.size board) (translate (-0.5) (-0.5) $ rectangleSolid 1.0 1.0))
    where floatize :: Cell -> (Float, Float)
          floatize (x, y) = (fromIntegral x, fromIntegral y)

-- |Draws a cell grid onto the screen.
drawGrid :: Game -> Picture
drawGrid (Game {camera = camera, screenSize = screenSize}) = pictures
    [ translate (-halfHorizontal) horizontalCameraOffset $ pictures 
        [translate 0.0 offset line | line <- replicate horizontalLineCount $ Line [ (0.0, 0.0)
                                                                                  , (horizontalLineSize, 0.0)]
                                   | offset <- horizontalLineOffsets]
    , translate verticalCameraOffset (-halfVertical) $ pictures 
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
          horizontalLineOffsets = [-scaledCellSize, -scaledCellSize * 2.0 .. -halfVertical] ++ 0 : [scaledCellSize, scaledCellSize * 2.0 .. halfVertical + scaledCellSize]
          verticalLineOffsets :: [Float]
          verticalLineOffsets = [-scaledCellSize, -scaledCellSize * 2.0 .. -halfHorizontal] ++ 0 : [scaledCellSize, scaledCellSize * 2.0 .. halfHorizontal + scaledCellSize]
          scaledCellSize :: Float
          scaledCellSize = cellSize * zoom camera

          horizontalCameraOffset :: Float
          horizontalCameraOffset = -(y camera `mod'` cellSize) * zoom camera 
          verticalCameraOffset :: Float
          verticalCameraOffset = -(x camera `mod'` cellSize) * zoom camera 

-- |Draws the given instance of the game to the screen.
drawGame :: Game -> Picture
drawGame game = pictures [ color white $ drawCells game
                         , if showGrid game then color white $ drawGrid game else Blank]



-- |Enacts user input onto the game.
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
         'r' -> if keyState == Down then game {board = HS.empty, paused = True} else game
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
              ( floor $ (fst position / zoom gameCamera + x gameCamera) / cellSize + 1.0
              , floor $ (snd position / zoom gameCamera + y gameCamera) / cellSize + 1.0)

          gameCamera :: Camera
          gameCamera = camera game
gameInteract (EventResize newScreenSize) game = game {screenSize = newScreenSize}
gameInteract _ game = game



-- |Represents the state of a game.
data Game = Game { board      :: Board
                 , camera     :: Camera
                 , paused     :: Bool
                 , showGrid   :: Bool
                 , screenSize :: (Int, Int)}

-- |Generates a new, empty game.
newGame :: Game
newGame = Game { board  = HS.empty
               , camera = Camera { x = 0.0,    deltaX = 0.0
                                 , y = 0.0,    deltaY = 0.0
                                 , zoom = 1.0, deltaZoom = 0.0}
               , paused = True
               , showGrid = False
               , screenSize = defaultScreenSize}

-- |What to display as the title of the window.
title :: String
title = "Conway's Ivory Tower"
-- |The size of the screen in pixels.
defaultScreenSize :: (Int, Int)
defaultScreenSize = (600, 600)
-- |The color of the window's background.
backgroundColor :: Color
backgroundColor = black
-- |Number of times to advance the board per second.
iterationsPerSecond :: Int
iterationsPerSecond = 10

-- |How fast the camera moves.
moveSpeed :: Float
moveSpeed = 120.0
-- |How fast the camera zooms.
zoomSpeed :: Float
zoomSpeed = 1.0
-- |The size to draw the cells at.
cellSize :: Float
cellSize = 8.0

main :: IO ()
main = play (InWindow title defaultScreenSize (100, 100)) backgroundColor
            iterationsPerSecond newGame
            drawGame gameInteract iterateGame