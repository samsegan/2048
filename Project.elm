import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Window
import Keyboard
import Time exposing (..)
import Text exposing (..)
import List exposing (..)
import Random exposing (..)

type alias Keys = { x:Int, y:Int }
type alias Piece = { x:Float, y:Float, vx:Float, vy:Float, val:Int }
type alias Board = List (List Int)

board = [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,2,0]]
-- make board an arr?
-- add a type board? with positions indicating vals?
limit = 400
speed = 20
leftEdge = (-limit/2 + squareSize/2)
rightEdge = (limit/2 - squareSize/2)
gridSize = 4
score = 0

squareSize = 100


p : Piece
p = 
    { x = leftEdge,
    y = leftEdge, 
    vx = 0,
    vy = 0,
    val = 2}


checkList : List Int -> Bool
checkList xs = 
    case xs of 
        [] -> False
        hd::tl -> if hd == 0 then True else checkList tl

checkEmptyGrid : Board -> Bool
checkEmptyGrid grid = 
    case grid of
        [] -> False
        hd::tl -> (checkList hd) && checkEmptyGrid tl 

sumList : List Int -> Int -> Int
sumList xs val = 
    List.foldl (+) val xs

sumGrid : Board -> Int
sumGrid grid = 
    List.foldl sumList 0 grid

-- Inserts a 2 once
insertList : List Int -> Bool -> List Int
insertList xs insertBool =
    case xs of 
        [] -> [] --Debug.crash (This should never be a case)
        hd::tl -> if (hd == 0 && insertBool) then 2 :: (insertList tl False)
                else hd :: (insertList tl insertBool)

insertRandListHelper : List Int -> Bool -> List Int
insertRandListHelper xs insertBool =
    let (row, toInsert) = insertRandInList xs
    in if toInsert then row 
    else insertList xs insertBool

insertRandInList : List Int -> (List Int, Bool)
insertRandInList xs = 
    let 
        seed0 = initialSeed 31415
    in let randVal = fst (generate (int 0 (List.length xs)) seed0) 
    in let randPosVal = findAtPos xs randVal
    in case randPosVal of
        0 -> ((insertAtPos xs randVal 2), True)
        _ -> (xs, False)

insertRandomHelper : Board -> Bool -> Board
insertRandomHelper grid insertBool = 
    case grid of
        [] -> []
        hd::tl -> if checkList hd then (insertList hd insertBool) :: (insertRandomHelper tl False)
                else hd :: (insertRandomHelper tl insertBool)

insertRandInGrid : Board -> Board
insertRandInGrid grid = 
    insertRandomHelper grid True

-- Finds an element at a given position
findAtPos : List Int -> Int -> Int
findAtPos xs i = 
    case xs of
        [] -> -1
        hd::tl -> if i == 0 then hd else findAtPos tl (i-1)

-- Finds a tile at a position give its i,j coords in the grid
getTileAtPos : Board -> (Int, Int) -> Int
getTileAtPos grid (i,j) =
    case grid of 
        [] -> -1
        hd::tl -> if i == 0 then (findAtPos hd j) else getTileAtPos tl ((i-1),j)

-- Inserts an integer at the ith position
insertAtPos : List Int -> Int -> Int -> List Int
insertAtPos xs i num =
    case xs of 
        [] -> []
        hd::tl ->
            if i == 0 then num::tl 
            else hd :: insertAtPos tl (i-1) num

-- gets next tile for a given tile in the given direction
-- first bool: x or y (indicated by the bool, True for right, false for left)
getNext : Board -> (Int, Int) -> Bool -> Bool -> Int
getNext grid (i,j) xBool dirBool =
    if xBool then 
        if dirBool then (getTileAtPos grid (i, j+1))
        else (getTileAtPos grid (i, j-1))
    else 
        if dirBool then (getTileAtPos grid (i+1, j))
        else (getTileAtPos grid (i-1, j))

shiftX : List Int -> List Int 
shiftX xs =
    List.filter (\x -> x /= 0) xs

-- gets the ith column of a grid and turns is into a list in 
-- order to be manipulated in the x direction
getYCol : Board -> Int -> List Int
getYCol grid i = 
    case grid of 
        [] -> []
        hd::tl -> (findAtPos hd i)::(getYCol tl i)

-- Puts the ith column in the grid and returns the grid
putYColHelper : List Int -> Int -> Board -> Int -> Board
putYColHelper ys i grid pos = 
    case grid of
        [] -> []
        hd::tl-> (insertAtPos hd i (findAtPos ys pos)) :: putYColHelper ys i tl (pos+1)

putYCol : List Int -> Int -> Board -> Board
putYCol ys i grid = 
    putYColHelper ys i grid 0

--Pads up to the gridsize, dirBool moves in +x direction
padX : List Int -> Bool -> List Int
padX xs dirBool = 
    let length = List.length xs
    in case length of
        0 -> [0,0,0,0]
        1 -> if dirBool then [0,0,0] ++ xs
            else xs ++ [0,0,0]
        2 -> if dirBool then [0,0] ++ xs
            else xs ++ [0,0]
        3 -> if dirBool then [0] ++ xs
            else xs ++ [0]
        _ -> xs

combineX : List Int -> Bool -> List Int
combineX xs dirBool =
    let 
        x1 = findAtPos xs 0
        x2 = findAtPos xs 1
        x3 = findAtPos xs 2
        x4 = findAtPos xs 3
    in 
    if dirBool then --case shifting right
        if x4 == x3 then
            if x2 == x1 then 
                [0,0,(x1+x2),(x3+x4)]
            else
                [0,x1,x2,(x3+x4)]
        else if x2 == x3 then 
            [0,x1,(x2+x3),x4]
        else if x1 == x2 then
            [0,(x1+x2),x3,x4]
        else [x1,x2,x3,x4]
    else 
        if x1 == x2 then
            if x3 == x4 then 
                [(x1+x2),(x3+x4),0,0]
            else
                [(x1+x2),x3,x4,0]
        else if x2 == x3 then 
            [x1,(x2+x3),x4,0]
        else if x3 == x4 then
            [x1,x2,(x3+x4),0]
        else [x1,x2,x3,x4]

manipRow : List Int -> Bool -> List Int
manipRow xs dirBool = 
    combineX (padX (shiftX xs) dirBool) dirBool

-- manipulates the ith column, dirBool is True for down, false for up
manipCol : Board -> Int -> Bool -> List Int
manipCol grid i dirBool =
    let col = getYCol grid i 
    in
    (manipRow col dirBool)

-- Given a list of cols, it rotates the cols to normal
rotate : Board -> Board
rotate colgrid =
    let 
    row1 = getYCol colgrid 0 
    row2 = getYCol colgrid 1
    row3 = getYCol colgrid 2
    row4 = getYCol colgrid 3
    in 
    row1::row2::row3::row4::[]

manipX : Board -> Bool -> Board
manipX grid dirBool =
    case grid of
        [] -> []
        hd::tl ->
            (manipRow hd dirBool)::(manipX tl dirBool)

-- Manipulates the grid in the +y or -y directions
manipY : Board -> Bool -> Board
manipY grid dirBool = 
    let 
    col1 = manipCol grid 0 dirBool
    col2 = manipCol grid 1 dirBool
    col3 = manipCol grid 2 dirBool
    col4 = manipCol grid 3 dirBool
    in
    rotate (col1::col2::col3::col4::[])

listEquals : List Int -> List Int -> Bool
listEquals xs ys = 
    foldl (&&) True (List.map2 (==) xs ys)

gridEquals : Board -> Board -> Bool
gridEquals grid1 grid2 =
    foldl (&&) True (List.map2 listEquals grid1 grid2)

-- This is a genSquare for the Regular 2048 game
genSquare : Int -> (Float, Float) -> List Form
genSquare val (x,y) =
    let tick = round (logBase 2 (toFloat val)) in
    if val == 0 then []
    else if tick <= 4 then 
        [square (squareSize - 2) |> filled (rgb 255 (255-(tick*50)) 100) |> alpha 0.75 |> move (x,y),
        (text (bold (Text.color white (Text.height 20 (fromString (toString val)))))) |> move(x,y)]
    else if tick <= 8 then 
        [square (squareSize - 2) |> filled (rgb ((tick - 4)*50) 100 100) |> move (x,y),
        (text (bold (Text.color white (Text.height 20 (fromString (toString val)))))) |> move(x,y)]
    else if tick <= 12 then
        [square (squareSize - 2) |> filled (rgb 255 255 ((tick-8)*50)) |> move (x,y),
        (text (bold (Text.color white (Text.height 20 (fromString (toString val)))))) |> move(x,y)]
    else
        [square (squareSize - 2) |> filled white |> move (x,y),
        (text (bold (Text.color white (Text.height 20 (fromString (toString val)))))) |> move(x,y)]

-- gridToPic renders the pic on the state of the grid
-- rowsquare takes the ith row and generates a pic
rowSquare : List Int -> Int -> List Form
rowSquare row i = 
        (genSquare (findAtPos row 0) (-150, 150 - ((toFloat i)*100))) ++ 
            (genSquare (findAtPos row 1) (-50, 150 - ((toFloat i)*100))) ++ 
                (genSquare (findAtPos row 2) (50, 150 - ((toFloat i)*100))) ++ 
                    (genSquare (findAtPos row 3) (150, 150 - ((toFloat i)*100)))

gridToPic : Board -> List Form
gridToPic grid = 
    case grid of 
        [row1, row2, row3, row4] -> 
            rowSquare row1 0 ++ rowSquare row2 1 ++ rowSquare row3 2 ++ rowSquare row4 3
        _ -> if List.length grid == 0 then [square (limit + 5) |> filled green] else
            [square (limit + 5) |> filled black]

line : Float -> Path
line row = 
    path [(-(limit/2), (limit/2) - (row*100)), ((limit/2),(limit/2)-row*100)]

lineVert : Float -> Path
lineVert col = 
    path [((limit/2)-col*100,(limit/2)), ((limit/2) - (col*100),-(limit/2))]


background : (Int, Int) -> Board -> (Float, Float) -> Color -> Color -> List Form
background (w,h) board position c1 c2 =
    [rect (toFloat w) (toFloat h) |> filled black, -- big background
    square (limit + 5) |> filled white,
    square limit |> filled black,
    rect 255 105 |> filled white |> move (0,285),
    rect 250 100 |> filled black |> move (0,285),
    text (bold (Text.height 100 (Text.color white (fromString "2048")))) |> move (0, 300),
    text (bold (Text.height 25 (Text.color white (fromString "Score")))) |> move (200, 325),
    text (bold (Text.height 20 (Text.color white (fromString (toString (sumGrid board)))))) |> move (200, 300),
    --Graphics.Collage.rotate (degrees 20) (text (bold (Text.color white (fromString "The Presidential Version")))) |> move (-130, 345),
    text (bold (Text.color white (fromString "Use the arrow keys to combine the squares!"))) |> moveY -250] 
    ++ gridToPic board ++ 
        [traced (solid white) (line 1),
        traced (solid white) (line 2),
        traced (solid white) (line 3),

        traced (solid white) (lineVert 1),
        traced (solid white) (lineVert 2),
        traced (solid white) (lineVert 3)]

update : (Float, Keys) -> Board -> Board
update (dt, keys) board =
    if keys.y > 0 then 
        let new = manipY board False in --Checks if the manipulation updates the board
        if gridEquals new board then board 
        else insertRandInGrid new
    else if keys.y < 0 then 
        let new = manipY board True in
        if gridEquals new board then board
        else insertRandInGrid new
    else if keys.x > 0 then 
        let new = manipX board True in 
        if gridEquals new board then board 
        else insertRandInGrid new
    else if keys.x < 0 then 
        let new = manipX board False in
        if gridEquals new board then board
        else insertRandInGrid new
    else board

view : (Int, Int) -> Board -> Element
view (w,h) board =
    collage w h (background (w,h) board (0,0) lightBrown lightGrey)

main : Signal Element
main = 
    Signal.map2 view Window.dimensions (Signal.foldp update board input)

input : Signal (Float, Keys)
input = 
    let
        delta = Signal.map (\t -> t/20) (fps 10)
    in
        Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)

