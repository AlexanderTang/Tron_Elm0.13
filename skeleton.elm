import Color
import Keyboard
import Keyboard (KeyCode)
import Mouse
import Debug
import String (append, fromList)
import List
import Text
import Random (float)
import Char (toCode, fromCode, toUpper)
import Graphics.Input (Input, input, button, Handle, dropDown)
import Graphics.Input.Field (..)


-------------------------
-- WHAT IS IMPLEMENTED --
-------------------------

{--
author: Alexander Tang

The base game and all of the additional features are implemented, more info below:

Implemented:
------------
* Base game runs without bugs (or not any that I managed to find)
    -> diagonal movement is purposely blocked
    -> draw possible (no players win)
* 4 player mode (default 2P)
* custom name and color -> default "Player [insert number]" and light blue
* speed increase and color brightens up when close to tails and walls
    -> Note about that: your own tail is always close to you, which means you always
       get speed boosted.  It is easy to block out your own tail by removing
       the corresponding snake itself from the list in function 'speedAdjustment'.
       I find this makes the game more exciting so I left it on as a feature. Alternatively,
       the increase in speed can be adjusted in the global variable 'speedIncrease'.
       To balance this, I start with a slightly lower base speed and a 0.1% increase per second
       instead of the 1% listed in the assignment. The percentage only depends on the base
       speed, meaning the increase in speed over time is linear.
* custom keys
    -> A limitation to this feature is that the key must be pressed down WHILE the button
       is being pressed.  Holding down multiple keys at once will register the latest
       pressed in key while the button is pressed. Keys can be assigned multiple times,
       allowing one set of controls to control all 4 snakes for example. Clicking the button
       without pressing anything reverts it back to default.
    -> Another issue is that the arrow keys are not presented properly.  For player 1
       the default keys are the arrow keys (presented by the KeyCode values ranging from
       37 to 40.  However, when transformed into a string, it shows the symbols
       & ( % ' respectively.  This means that the keys may look strange in the config
       menu (start screen) when certain keys are entered, but they are fully
       functional.
* random walls are implemented
    -> players can increase their speed using these walls (and obviously crash into them)
    -> to admire all of the cleverly positioned walls, change the following in
       the function customWalls from:
           in  take amount allWalls
       to
           in  take 9 allWalls
       The maximum amount of random walls is 9.
* AI implemented for Solo mode
    -> will view in a (600+playerW)*(600+playerH) rectangular box and take an action
       accordingly.
       -> when an object within 50 pixels if found straight ahead, it will contemplate
          to turn left, right or keep going. To do this, it will take into account the
          amount of pixels to the right, the amount of pixels to the left, the distance
          to the nearest pixel directly to the left and the distance to the nearest 
          pixel directly to the right.  More info can be found above the function
          'aiStep' which holds the AI strategy.
    -> AI has it's derpy moments when driven into a corner, but plays fine
       aside from that.
    -> Its strong points are its pinpoint accuracies in going along lines without
       crashing into them.
    -> Its tendency to avoid things allows players opportunities to trap the AI;
       or if the player has decent timing, one can go through the AI's head from the side
       without the AI trying to dodge since AI assumes all the detected pixels are
       immobile.  This calls for some more advanced AI that keeps track of the player
       movement but that's outside the scope of this assignment I believe.

--}

------------
-- INPUTS --
------------

delta : Signal Time
delta = inSeconds <~ fps 40

type CustomKeys = { up:[KeyCode], down:[KeyCode], left:[KeyCode], right:[KeyCode] }
type CustomKeys' = { up:KeyCode, down:KeyCode, left:KeyCode, right:KeyCode }
type PlayerKeys = { p1:CustomKeys', p2:CustomKeys', p3:CustomKeys', p4:CustomKeys' }

playerKeys : Signal PlayerKeys
playerKeys = PlayerKeys <~ selectKey customKeys1 38 40 37 39
                         ~ selectKey customKeys2 (toCode 'W') (toCode 'S') (toCode 'A') (toCode 'D')
                         ~ selectKey customKeys3 (toCode 'O') (toCode 'L') (toCode 'K') (toCode 'M')
                         ~ selectKey customKeys4 (toCode 'Y') (toCode 'H') (toCode 'G') (toCode 'J')

customKeys1 : Signal CustomKeys
customKeys1 = CustomKeys <~ ck1up.signal 
                          ~ ck1down.signal 
                          ~ ck1left.signal 
                          ~ ck1right.signal

ck1up : Input [KeyCode]
ck1up = input [38]

ck1down : Input [KeyCode]
ck1down = input [40]

ck1left : Input [KeyCode]
ck1left = input [37]

ck1right : Input [KeyCode]
ck1right = input [39]

customKeys2 : Signal CustomKeys
customKeys2 = CustomKeys <~ ck2up.signal 
                          ~ ck2down.signal 
                          ~ ck2left.signal 
                          ~ ck2right.signal

ck2up : Input [KeyCode]
ck2up = input [toCode 'W']

ck2down : Input [KeyCode]
ck2down = input [toCode 'S']

ck2left : Input [KeyCode]
ck2left = input [toCode 'A']

ck2right : Input [KeyCode]
ck2right = input [toCode 'D']

customKeys3 : Signal CustomKeys
customKeys3 = CustomKeys <~ ck3up.signal 
                          ~ ck3down.signal 
                          ~ ck3left.signal 
                          ~ ck3right.signal

ck3up : Input [KeyCode]
ck3up = input [toCode 'O']

ck3down : Input [KeyCode]
ck3down = input [toCode 'L']

ck3left : Input [KeyCode]
ck3left = input [toCode 'K']

ck3right : Input [KeyCode]
ck3right = input [toCode 'M']

customKeys4 : Signal CustomKeys
customKeys4 = CustomKeys <~ ck4up.signal 
                          ~ ck4down.signal 
                          ~ ck4left.signal 
                          ~ ck4right.signal

ck4up : Input [KeyCode]
ck4up = input [toCode 'Y']

ck4down : Input [KeyCode]
ck4down = input [toCode 'H']

ck4left : Input [KeyCode]
ck4left = input [toCode 'G']

ck4right : Input [KeyCode]
ck4right = input [toCode 'J']

amountPlayers : Input Int
amountPlayers = input 2

playerName1 : Input Content
playerName1 = input noContent

playerName2 : Input Content
playerName2 = input noContent

playerName3 : Input Content
playerName3 = input noContent

playerName4 : Input Content
playerName4 = input noContent

type PlayerNameContents = { p1:Content, p2:Content, p3:Content, p4:Content }

playerNameContents : Signal PlayerNameContents
playerNameContents = PlayerNameContents <~ playerName1.signal
                                         ~ playerName2.signal
                                         ~ playerName3.signal
                                         ~ playerName4.signal

playerColor1 : Input Color
playerColor1 = input Color.lightBlue

playerColor2 : Input Color
playerColor2 = input Color.lightBlue

playerColor3 : Input Color
playerColor3 = input Color.lightBlue

playerColor4 : Input Color
playerColor4 = input Color.lightBlue

colorDropDown : Handle Color -> Element
colorDropDown handler = dropDown handler
    [("light blue"     , Color.lightBlue)
    , ("light red"      , Color.lightRed)
    , ("yellow"         , Color.yellow)
    , ("light green"    , Color.lightGreen)
    , ("light brown"    , Color.lightBrown)
    , ("light orange"   , Color.lightRed) ]

type PlayerColors = { p1:Color ,p2:Color ,p3:Color ,p4:Color }

playerColors : Signal PlayerColors
playerColors = PlayerColors <~ playerColor1.signal
                             ~ playerColor2.signal
                             ~ playerColor3.signal
                             ~ playerColor4.signal

-- returns the first keycode of the list of keycodes for each direction.  If keycode list
-- is empty, it will give the given default keycode instead
selectKey : Signal CustomKeys -> KeyCode -> KeyCode -> KeyCode -> KeyCode -> Signal CustomKeys'
selectKey ck keycode1 keycode2 keycode3 keycode4 =
    (\c ->
        { up = getKey c.up keycode1
        , down = getKey c.down keycode2
        , left = getKey c.left keycode3
        , right = getKey c.right keycode4 }
    ) <~ ck

getKey : [KeyCode] -> KeyCode -> KeyCode
getKey keys key = if isEmpty keys then key else head keys

-- Replaces the Keyboard.directions function to allow for Signal input
directions : Signal PlayerKeys -> Int -> Signal { x : Int, y : Int }
directions playerkeys playerId =
    (\pkeys kd ->
        let keySet = case playerId of
                        1 -> pkeys.p1
                        2 -> pkeys.p2
                        3 -> pkeys.p3
                        4 -> pkeys.p4
            u = keySet.up
            d = keySet.down
            l = keySet.left
            r = keySet.right
        in  filter (\ky -> member ky [u,d,l,r]) kd |>
                foldl (\ky st -> if | ky == u -> { st | y <- st.y + 1 }
                                    | ky == d -> { st | y <- st.y - 1 }
                                    | ky == l -> { st | x <- st.x - 1 }
                                    | ky == r -> { st | x <- st.x + 1 }
                ) {x=0,y=0}
    ) <~ playerkeys ~ Keyboard.keysDown

-- List.member does not appear to be available in this version of Elm (13.0), so I
-- attempt to recreate it based on the member function here: https://github.com/elm-lang/core/blob/master/src/Native/List.js
member : a -> [a] -> Bool
member el elList =
    let boolList = map (equals el) elList
        boolList' = filter isTrue boolList
    in  if isEmpty boolList' then False else True

equals : a -> a -> Bool
equals a1 a2 = if a1 == a2 then True else False

type Orient = { x:Int, y:Int }
type GameInput = { space:Bool, delta:Time, so1:Orient, so2:Orient, so3:Orient, so4:Orient,
                   amount:Int, pnc:PlayerNameContents, pc:PlayerColors, random:Float }

-- delta frames per second and not delta frames per second + number
-- of key presses or players could cheat with bots that press directions
-- 1000 times per second, accelerating the game to inhuman speed
gameInput : Signal GameInput
gameInput = 
             let sampledInput = sampleOn delta 
                                <| GameInput <~ Keyboard.space
                                              ~ delta
                                              ~ (directions playerKeys 1)
                                              ~ (directions playerKeys 2)
                                              ~ (directions playerKeys 3)
                                              ~ (directions playerKeys 4)
                                              ~ amountPlayers.signal
                                              ~ playerNameContents
                                              ~ playerColors
                                              ~ float delta
             in  lift (Debug.watch "input") sampledInput


-----------
-- MODEL --
-----------
 
-- default values
width = 1024
height = 768

playerW = 64
playerH = 16

data Orientation = N | E | S | W

(halfWidth,halfHeight) = (width/2, height/2)

-- Note that the tail length indicates the amount of positions in
-- the list, not the length in pixels.  This means that a player that
-- builds extra speed can use this to his/her advantage by gaining
-- longer tails. The tail length will normalize when the player has
-- been running on normal speed for a while.
tailLength = 200

-- used for keeping track of original velocity when snakes speed up
baseVelocity = 60

speedIncrease = 0.001
lightnessIncrease = 0.0001

wallWidth = 100
wallHeight = 10

-- the position of a wall is in the same position as that of a snake head:
-- in the middle of the height and at the end of one side of the width
type WallOrient = (Int,Int)
type Wall = { pos:Pos, wo:WallOrient }

type Pos = (Float, Float)
type Vel = Float
type SnakeOrient = (Int, Int)
type SnakeHead = Pos
type SnakeTail = [Pos]
type Snake = { sh:SnakeHead, st:SnakeTail, v:Vel, so:SnakeOrient, alive:Bool,
               name:String, color:Color }

data State = Start | Play | End

type Game = { state:State, player1:Snake, player2:Snake, player3:Snake,
              player4:Snake, walls:[Wall], ai:Bool }

player : SnakeHead -> SnakeOrient -> Bool -> String -> Color -> Snake
player sh' so' b n c = { sh=sh', st=[], v=baseVelocity, so=so', alive=b, name=n, color=c }

defaultGame : State -> Int -> String -> String -> String -> String ->
              Color -> Color -> Color -> Color -> Float -> Game
defaultGame s amount pn1 pn2 pn3 pn4 c1 c2 c3 c4 w =
    { state = s
    , player1 = player (50-halfWidth, 0) (1,0) True 
                (if pn1 == "" then "Player 1" else pn1) c1
    , player2 = player (halfWidth-50, 0) (-1,0) True
                (if pn2 == "" then "Player 2" else pn2) c2
    , player3 = player (0, halfHeight-50) (0,-1) (if amount > 2 then True else False)
                (if pn3 == "" then "Player 3" else pn3) c3
    , player4 = player (0, 50-halfHeight) (0,1) (if amount > 3 then True else False)
                (if pn4 == "" then "Player 4" else pn4) c4
    , walls = generateWalls w
    , ai = if amount == 1 then True else False}

-- w is a number from the interval [0,1).
generateWalls : Float -> [Wall]
generateWalls w =
    let w' = floor (w*10)   -- w' is an Int ranging from 0 to 9 
    in  customWalls w'

customWalls : Int -> [Wall]
customWalls amount =
    let allWalls = [ { pos = (180-halfWidth,halfHeight-250), wo = (1,0) }
                   , { pos = (120-halfWidth,130), wo = (0,1) }
                   , { pos = (150,halfHeight-250), wo = (1,0) }
                   , { pos = (200-halfWidth,-150), wo = (0,1) }
                   , { pos = (-150,250), wo = (1,0) }
                   , { pos = (260,-150), wo = (0,1) }
                   , { pos = (0,0), wo = (1,0) }
                   , { pos = (350,220), wo = (0,1) }
                   , { pos = (-250,-200), wo = (1,0) }
                   ]
    in  take amount allWalls

------------
-- UPDATE --
------------

-- convert SnakeOrient to Orientation
toOrientation : SnakeOrient -> Orientation
toOrientation so =
    case so of
        (1,0)  -> E
        (-1,0) -> W
        (0,1)  -> N
        (0,-1) -> S

-- are n and m near each other?
-- specifically are they within c of each other?
near : Float -> Float -> Float -> Bool
near n c m = m >= n-c && m <= n+c

-- repositions the snake head position to the middle of the light cycle
middleSnakeHead : SnakeOrient -> SnakeHead -> Pos
middleSnakeHead (ox,oy) (x,y) =
    let pw = playerW/2
    in if ox == 0
        then if oy == 1
             then (x,y+pw)
             else (x,y-pw)
        else if ox == 1
             then (x+pw,y)
             else (x-pw,y)

-- is a position within the snake head?
within : SnakeOrient -> SnakeHead -> Pos -> Bool
within (ox,oy) sh (xpos,ypos) = within2 (ox,oy) sh 0 (xpos,ypos)

-- Checks if a position is within a certain range of the snake head.  The search happens
-- in a rectangular box.  The range given will be applied to both the horizontal and
-- vertical side, giving a higher effective range on diagonals.
within2 : SnakeOrient -> SnakeHead -> Float -> Pos -> Bool
within2 (ox,oy) sh n (xpos,ypos) =
    let pw = (playerW/2) + n
        ph = (playerH/2) + n
        (x',y') = middleSnakeHead (ox,oy) sh
    in  if ox == 0
        then (near x' ph xpos) && (near y' pw ypos)
        else (near x' pw xpos) && (near y' ph ypos)

-- checks if the boolean is true or not
isTrue : Bool -> Bool
isTrue b = if b == True
           then True
           else False

-- returns true if the first snake collides into the second snake
snakeCollisionHelper : Snake -> Float -> Snake -> Bool
snakeCollisionHelper s1 n s2 = 
    let l = map (within2 s1.so s1.sh n) <| s2.st
        l' = filter isTrue l
    in  if isEmpty l'
        then False
        else True

-- returns true if the snake is alive
snakeAlive : Snake -> Bool
snakeAlive s = if s.alive then True else False

-- checks if the snake crashes into any other (living) snakes
snakeCollisions : Snake -> [Snake] -> Snake
snakeCollisions s snakes =
    let snakes' = filter snakeAlive snakes
        collisions = map (snakeCollisionHelper s 0) <| snakes'
        collisions' = filter isTrue collisions
        alive' = if isEmpty collisions' then True else False
    in  { s | alive <- alive' }

-- handles any snake collisions (dead snakes are ignored)
snakeInteraction : (Snake,Snake,Snake,Snake) -> (Snake,Snake,Snake,Snake)
snakeInteraction (s1,s2,s3,s4) =
    let snakes = [s1,s2,s3,s4]
        s1' = if s1.alive then snakeCollisions s1 snakes else s1
        s2' = if s2.alive then snakeCollisions s2 snakes else s2
        s3' = if s3.alive then snakeCollisions s3 snakes else s3
        s4' = if s4.alive then snakeCollisions s4 snakes else s4
    in (s1',s2',s3',s4')

-- Returns true if the snake collides into a wall. The integer is the value for the offset
-- on the horizontal and vertical side of the snakehead.  A value of 5 would create a
-- rectangular box around the snake head 5 pixels off the sides, effectively creating a
-- bigger snakehead.
wallCollision : Snake -> Float ->  Bool
wallCollision { sh,so } offset =
    let (xleft,xright,ybot,yup) = getEdgeCoords sh so offset (playerW,playerH)
    in  not ( (xleft > -1*halfWidth) && (xright < halfWidth) &&
        (ybot > -1*halfHeight) && (yup < halfHeight) )

-- Returns true if maximum one player remains alive
lastSnakeStanding : Snake -> Snake -> Snake -> Snake -> Bool
lastSnakeStanding p1 p2 p3 p4 =
    let l = if p1.alive then p1::[] else []
        l' = if p2.alive then p2::l else l
        l'' = if p3.alive then p3::l' else l'
        l''' = if p4.alive then p4::l'' else l''
    in  if length l''' > 1
        then False
        else True

-- step snake head based on velocity, timestep and orientation
stepHead : Time -> SnakeHead -> Vel -> SnakeOrient -> SnakeHead
stepHead t (x,y) v (xOrient,yOrient) =
    (x + (v * t * (toFloat xOrient)), y + (v * t * (toFloat yOrient)))

-- step tail based on current tail length and new position
stepTail : Pos -> SnakeTail -> SnakeTail
stepTail (x,y) positions = 
    let positions' = (x,y) :: positions
    in  if (length positions') > tailLength
        then take tailLength positions'
        else positions'

-- step snake as a whole through time
stepSnake : Time -> Snake -> [Wall] -> Snake
stepSnake t ({sh,st,v,so} as p) walls =
    let headPos' = stepHead t sh v so
        tail' = stepTail sh st
        killedByWall = wallCollision p 0
        killedByCustomWall = customWallCollision p 0 walls
    in  { p | sh <- headPos'
            , st <- tail'
            , alive <- (not killedByWall) && (not killedByCustomWall) }

-- returns true if the snake is within the given offset of one of the walls
-- (for offset = 0 this means the snake head is inside the wall)
customWallCollision : Snake -> Float -> [Wall] -> Bool
customWallCollision s offset walls =
    let collisions = map (customWallCollision2 s offset) walls
        c = filter isTrue collisions
    in  if isEmpty c then False else True
        
customWallCollision2 : Snake -> Float -> Wall -> Bool
customWallCollision2 { sh,so } offset { pos,wo } =
    let (xleft,xright,ybot,yup) = getEdgeCoords sh so offset (playerW,playerH)
        
        -- walls follow the same reasoning, but don't apply offset again!
        -- and reposition pos to the end of the width of the wall (bottom or left)
        (xWall,yWall) = pos
        (sol',sor') = wo
        (x',y') = if sol' == 1 then (xWall - (wallWidth/2), yWall)
                  else (xWall, yWall - (wallWidth/2))
        (xleft',xright',ybot',yup') = getEdgeCoords (x',y') wo 0 (wallWidth,wallHeight)
    in  -- strategy: wall and snake do not collide if wall is either completely left,
        -- above, down or right of snake.  (or nearby from a certain offset)
        not( (xright' < xleft) || (xleft' > xright) || (yup' < ybot) || (ybot' > yup) )

-- returns the most left, upper, bottom and right coordinates of a cycle 
-- (custom wall or snake), when position is based on the end of one side of the rectangle
--   -> this is the side in the middle of the height, while on the bottom when looking 
--      in the direction of the object orientation
getEdgeCoords : Pos -> (Int,Int) -> Float -> (Int,Int) -> (Float,Float,Float,Float)
getEdgeCoords (x,y) (sol,sor) offset (width',height') =
    let (w,h) = (x + toFloat (sol*width'), (toFloat (sor*height'))/2)
        xleft = (minimum [x,w]) - (abs h) - offset
        xright = (maximum [x,w]) + (abs h) + offset
        (w',h') = (y + toFloat (sor*width'), (toFloat (sol*height'))/2)
        ybot = (minimum [y,w']) - (abs h') - offset
        yup = (maximum [y,w']) + (abs h') + offset
    in  (xleft,xright,ybot,yup)


-- Determine the direction for the snake. If multiple keys are pressed, the horizontal
-- keys will get precedence. No diagonal movement allowed!
findDirection : Snake -> Orient -> Snake
findDirection s so' =
    let (x,y) = (so'.x, so'.y)
    in  if (x,y) == (0,0) then s
        else if (not (x==0)) && (not (y==0)) then { s | so <- (x,0) }
             else { s | so <- (x,y) }

-- returns true if snake is close to tails and/or walls
closeToSomething : Snake -> [Snake] -> [Wall] -> Snake
closeToSomething s snakes walls =
    let snakes' = filter snakeAlive snakes
    
        collisions = map (snakeCollisionHelper s 10) <| snakes'
        collisions' = filter isTrue collisions
        snakeNearby = if isEmpty collisions then False else True
        
        wallNearby = wallCollision s 5
        customWallNearby = customWallCollision s 5 walls
    in  if snakeNearby || wallNearby || customWallNearby
        then { s | v <- s.v + speedIncrease*baseVelocity
                 , color <- let hsl' = toHsl s.color
                                (hue,sat,light) = (hsl'.hue, hsl'.saturation, hsl'.lightness)
                                light' = if light < 1 then light + lightnessIncrease
                                         else light
                            in  hsl hue sat light'}
        else s
    
-- Speeds a snake up permanently when moving close to tails and/or walls. Speeds up
-- by 1% of base velocity every second.  Color of snake also becomes brighter.
-- If your own tail should not speed you up, then give for player 1 [s2,s3,s4] 
-- instead of 'snakes', for player 2 [s1,s3,s4], analogous for player 3 and 4
speedAdjustment : (Snake,Snake,Snake,Snake) -> [Wall] -> (Snake,Snake,Snake,Snake)
speedAdjustment (s1,s2,s3,s4) walls =
    let snakes = [s1,s2,s3,s4]
        s1' = if s1.alive then closeToSomething s1 snakes walls else s1
        s2' = if s2.alive then closeToSomething s2 snakes walls else s2
        s3' = if s3.alive then closeToSomething s3 snakes walls else s3
        s4' = if s4.alive then closeToSomething s4 snakes walls else s4
    in (s1',s2',s3',s4')

{--
AI strategy:

- Go through all the snake tails and walls and keep any positions within 300 pixels
  in one of two variables: one on the side if ai would turn 90 degrees left and
  the other 90 degrees to the right (positions directly in front are discarded).
- While going through the snake tails and walls, check if any of these positions
  pose an imminent threat (within 50 pixels in front, taking into account the
  height of the snake cycle). Also take into account that any pixels outside the screen
  will count, meaning that a snake close to an edge would have a high tendency to turn
  away from it (if no other immediate threats are around).
- When finished, check if action is needed (if anything is within 50 pixels to the front)
    -> If so, turn if this does not result in suicide (again, take into account the snake
       cycle height, which could crash into its own tail if the ai turned an instant ago).
       Also give some special attention to pixels that are within 50 pixels when choosing
       a direction to turn (it is preferable to go to a more compact area than to crash
       into a wall beyond which is a large empty space).
    -> Otherwise, keep the same direction.
- In the end it basically comes down to: should I turn left, right or continue straight?
--}

-- AI makes his move.  Prepare to get whooped.
aiStep : Snake -> [Snake] -> [Wall] -> Snake
aiStep s snakes walls =
    let offset = 300
        (leftPixels,rightPixels,frontDist,leftDist,rightDist) = aiWallCheck s offset
        (lp,rp,fd,ld,rd) = aiSnakeCheck s snakes offset
        (lp',rp',fd',ld',rd') = 
            if (isEmpty walls) then (0,0,width,width,width)
            else aiCustomWallCheck s walls offset
        
        leftPixels' = leftPixels + lp + lp'
        rightPixels' = rightPixels + rp + rp'
        frontDist' = minimum [frontDist,fd,fd']
        leftDist' = minimum [leftDist,ld,ld']
        rightDist' = minimum [rightDist,rd,rd']
        
        viewingDistance = 50 + playerW
    in  if frontDist' <= viewingDistance then
            if leftDist' < frontDist' && rightDist' < frontDist' then s
            else if leftDist' <= viewingDistance && rightDist' > viewingDistance then turnRight s
                 else if leftDist' > viewingDistance && rightDist' <= viewingDistance then turnLeft s
                      else if leftDist' > viewingDistance && rightDist' > viewingDistance
                           then if leftPixels' >= rightPixels' then turnRight s
                                else turnLeft s
                           else if leftDist' < rightDist' then turnRight s
                                else turnLeft s
        else s

turnLeft : Snake -> Snake
turnLeft ({so} as s) =
    let so' = turnLeft' so
    in  {s | so <- so'}

turnRight : Snake -> Snake
turnRight ({so} as s) =
    let so' = turnRight' so
    in  {s | so <- so'}

turnLeft' : (Int,Int) -> (Int,Int)
turnLeft' (x,y) =
    if (x == 1) then (0,1)
    else if x == -1 then (0,-1)
         else if y == 1 then (-1,0)
              else (1,0)
                    
turnRight' : (Int,Int) -> (Int,Int)
turnRight' (x,y) =
    if x == 1 then (0,-1)
    else if x == -1 then (0,1)
         else if y == 1 then (1,0)
              else (-1,0)

{--
This doc applies to: aiWallCheck, aiSnakeCheck and aiCustomWallCheck

Returns respectively: 
    amount of obstacle pixels if snake turns to left,
    amount of obstacle pixels if snake turns to right, 
    minimum distance to an obstacle straight ahead,
    minimum distance to obstacle if snake turns left,
    minimum distance to obstacle if snake turns right
--}

aiWallCheck : Snake -> Float -> (Float,Float,Float,Float,Float)
aiWallCheck { sh,so } offset =
    let (xleft,xright,ybot,yup) = getEdgeCoords sh so 0 (playerW,playerH)
        
        (dLeft,dRight,dUp,dBot) = ( distance (0-halfWidth) xleft,
                                    distance halfWidth xright,
                                    distance halfHeight yup,
                                    distance (0-halfHeight) ybot )
        
        -- dz stands for dangerzone
        (sol,sor) = so
        (dz,dzl,dzr,dzb) = 
            if sol == 1 then (dRight, dUp, dBot, dLeft)
            else if sol == -1 then (dLeft, dBot, dUp, dRight)
                 else if sor == 1 then (dUp, dLeft, dRight, dBot)
                      else (dBot, dRight, dLeft, dUp)
        
        -- the detection box is given by the rectangle around the snake minus the
        -- part that's directly behind or in front of the snake
        snakeDetectionBox = (((2*offset) + playerW) * ((2*offset) + playerH)) 
                            - (playerH * 2 * offset)
        halfBox = snakeDetectionBox / 2
        leftBox = halfBox - ((min dzl offset) * ((min dz 300) + (min dzb 300) + playerW))
        rightBox = halfBox - ((min dzr offset) * ((min dz 300) + (min dzb 300) + playerW))
    in  (leftBox,rightBox,dz,(dzl+(playerH/2)-playerW),(dzr+(playerH/2)-playerW))

aiSnakeCheck : Snake -> [Snake] -> Float -> (Float,Float,Float,Float,Float)
aiSnakeCheck ({ sh,so } as s) snakes offset =
    let snakes' = filter snakeAlive snakes
        (xleft,xright,ybot,yup) = getEdgeCoords sh so 0 (playerW,playerH)

        -- create 3 'extended detection rectangles' to the front, left and right:
        (frontView, leftView, rightView) = viewingRectangles so (xleft,xright,ybot,yup)
        
        results = concat (map (aiSnakeCheck2 s frontView leftView rightView offset) snakes')
    in  transformResult results
        
aiSnakeCheck2 : Snake -> (Float,Float,Float,Float) -> (Float,Float,Float,Float) -> 
                (Float,Float,Float,Float) -> Float -> Snake -> [(Bool,Bool,Float,Float,Float)]
aiSnakeCheck2 s1 fv lv rv offset s2 = map (aiSnakeCheck3 s1 fv lv rv offset) s2.st

aiSnakeCheck3 : Snake -> (Float,Float,Float,Float) -> (Float,Float,Float,Float) -> 
                (Float,Float,Float,Float) -> Float -> Pos -> (Bool,Bool,Float,Float,Float)
aiSnakeCheck3 { sh,so } fv lv rv offset (x',y') =
    let (x,y) = sh
        (sol,sor) = so
        
        (leftVision, rightVision) =
            if sol == 1 
            then ( (distance (x+playerW/2) x') <= (offset+(playerW/2))
                        && (distance y y') <= (offset+(playerH/2))
                        && y' > y ,
                   (distance (x+playerW/2) x') <= (offset+(playerW/2))
                        && (distance y y') <= (offset+(playerH/2))
                        && y' < y )
            else if sol == -1
                 then ( (distance (x-playerW/2) x') <= (offset+(playerW/2))
                            && (distance y y') <= (offset+(playerH/2))
                            && y' < y ,
                        (distance (x-playerW/2) x') <= (offset+(playerW/2))
                            && (distance y y') <= (offset+(playerH/2))
                            && y' > y )
                 else if sor == 1
                      then ( (distance x x') <= (offset+(playerH/2))
                                 && (distance (y+playerW/2) y') <= (offset+(playerW/2))
                                 && x' < x ,
                             (distance x x') <= (offset+(playerH/2))
                                 && (distance (y+playerW/2) y') <= (offset+(playerW/2))
                                 && x' > x )
                      else ( (distance x x') <= (offset+(playerH/2))
                                 && (distance (y-playerW/2) y') <= (offset+(playerW/2))
                                 && x' > x ,
                             (distance x x') <= (offset+(playerH/2))
                                 && (distance (y-playerW/2) y') <= (offset+(playerW/2))
                                 && x' < x )
        
        frontDist = withinX sh so fv (x',y')
        leftDist = withinX sh (turnLeft' so) lv (x',y')
        rightDist = withinX sh (turnRight' so) rv (x',y')
    in  (leftVision,rightVision,frontDist,leftDist,rightDist)

viewingRectangles : (Int,Int) -> (Float,Float,Float,Float) ->
                    ( (Float,Float,Float,Float), (Float,Float,Float,Float), (Float,Float,Float,Float) )
viewingRectangles (sol,sor) (xleft,xright,ybot,yup) =
            if sol == 1 then ((xleft,halfWidth,ybot,yup),
                              (xleft-(playerH/2),xleft+(playerH/2),
                                   ybot+(playerH/2),halfHeight),
                              (xleft-(playerH/2),xleft+(playerH/2),
                                   (0-halfHeight),yup-(playerH/2)))
            else if sol == -1 then (((0-halfWidth),xright,ybot,yup),
                                     (xright-(playerH/2),xright+(playerH/2),
                                         (0-halfHeight),yup-(playerH/2)),
                                     (xright-(playerH/2),xright+(playerH/2),
                                         ybot+(playerH/2),halfHeight))
                 else if sor == 1 then ((xleft,xright,ybot,halfHeight),
                                       ((0-halfWidth),xright-(playerH/2),
                                            ybot-(playerH/2),ybot+(playerH/2)),
                                        (xleft+(playerH/2),halfWidth,
                                            ybot-(playerH/2),ybot+(playerH/2)))
                      else ((xleft,xright,(0-halfHeight),yup),
                            (xleft+(playerH/2),halfWidth,
                                yup-(playerH/2),yup+(playerH/2)),
                            ((0-halfWidth),xright-(playerH/2),
                                yup-(playerH/2),yup+(playerH/2)))

-- returns an artificially high distance (screen width) if the pixel is not within
-- the scanning rectangle
withinX : Pos -> (Int,Int) -> (Float,Float,Float,Float) -> Pos -> Float
withinX (x',y') (sol,sor) (xleft,xright,ybot,yup) (x,y) =
    if ( xleft < x && xright > x && ybot < y && yup > y )
    then if not (sol == 0)
         then (distance x' x) - playerW
         else (distance y' y) - playerW
    else width
    
    
transformResult : [(Bool,Bool,Float,Float,Float)] -> (Float,Float,Float,Float,Float)
transformResult results = 
    let (aLeft,aRight,minFront,minLeft,minRight) = unzip5 results
        aLeft' = toFloat (length (filter isTrue aLeft))
        aRight' = toFloat (length (filter isTrue aRight))
        minFront' = if isEmpty minFront then width else minimum minFront
        minLeft' = if isEmpty minLeft then width else minimum minLeft
        minRight' = if isEmpty minRight then width else minimum minRight
    in  (aLeft',aRight',minFront',minLeft',minRight') 

unzip5 : [(a,b,c,d,e)] -> ([a],[b],[c],[d],[e])
unzip5 results =
    let step (aa,bb,cc,dd,ee) (aa',bb',cc',dd',ee') =
            (aa :: aa', bb :: bb', cc :: cc', dd :: dd', ee :: ee')
    in  foldr step ([], [], [], [], []) results

aiCustomWallCheck : Snake -> [Wall] -> Float -> (Float,Float,Float,Float,Float)
aiCustomWallCheck ({sh,so} as s) walls offset =
    let (xleft,xright,ybot,yup) = getEdgeCoords sh so 0 (playerW,playerH)
    
        -- create 3 'extended detection rectangles' to the front, left and right:
        (frontView, leftView, rightView) = viewingRectangles so (xleft,xright,ybot,yup)
    
        results = map (aiCustomWallCheck2 s frontView leftView rightView offset) walls
        
        (aLeft,aRight,minFront,minLeft,minRight) = unzip5 results
        aLeft' = sum aLeft
        aRight' = sum aRight
        minFront' = minimum minFront
        minLeft' = minimum minLeft
        minRight' = minimum minRight
    in  (aLeft',aRight',minFront',minLeft',minRight')

aiCustomWallCheck2 : Snake -> (Float,Float,Float,Float) -> (Float,Float,Float,Float) -> 
                (Float,Float,Float,Float) -> Float -> Wall -> (Float,Float,Float,Float,Float)
aiCustomWallCheck2 { sh,so } fv lv rv offset { pos,wo } =
    let (xWall,yWall) = pos
        (sol',sor') = wo
        (x',y') = if sol' == 1 then (xWall - (wallWidth/2), yWall)
                  else (xWall, yWall - (wallWidth/2))
        (xleft',xright',ybot',yup') = getEdgeCoords (x',y') wo 0 (wallWidth,wallHeight)
        
        (x,y) = sh
        (sol,sor) = so
        
        (halfBoxLeft,halfBoxRight) =
            if sol == 1 then ((x-offset,x+offset+playerW,y,y+offset+playerH/2),
                              (x-offset,x+offset+playerW,y-offset-playerH/2,y))
            else if sol == -1 then ((x-offset-playerW,x+offset,y-offset-playerH/2,y),
                                    (x-offset-playerW,x+offset,y,y+offset+playerH/2))
                 else if sor == 1
                      then ((x-offset-playerH/2,x,y-offset,y+offset+playerW),
                            (x,x+offset+playerH/2,y-offset,y+offset+playerW))
                      else ((x,x+offset+playerH/2,y-offset-playerW,y+offset),
                            (x-offset-playerH/2,x,y-offset-playerW,y+offset))
        
        leftPixels = overlappingPixels halfBoxLeft (xleft',xright',ybot',yup')
        rightPixels = overlappingPixels halfBoxRight (xleft',xright',ybot',yup')
        
        frontDist = withinY sh so fv (xleft',xright',ybot',yup')
        leftDist = withinY sh (turnLeft' so) lv (xleft',xright',ybot',yup')
        rightDist = withinY sh (turnRight' so) rv (xleft',xright',ybot',yup')
    in  (leftPixels,rightPixels,frontDist,leftDist,rightDist)
        
withinY : Pos -> (Int,Int) -> (Float,Float,Float,Float) -> 
          (Float,Float,Float,Float) -> Float
withinY (x',y') (sol,sor) (xleft,xright,ybot,yup) (xleft',xright',ybot',yup') =
    if ( ybot' > yup || yup' < ybot || xleft' > xright || xright' < xleft )
    then width
    else if sol == 1
         then (distance x' xleft') - playerW
         else if sol == -1
              then (distance x' xright') - playerW
              else if sor == 1
                   then (distance y' ybot') - playerW
                   else (distance y' yup') - playerW

overlappingPixels : (Float,Float,Float,Float) -> (Float,Float,Float,Float) -> Float
overlappingPixels (xl,xr,yb,yu) (xl',xr',yb',yu') =
    if xr' < xl || yb' > yu || yu' < yb || xl' > xr then 0
    else if xl' > xl && yu' < yu && yb' > yb && xr' < xr 
         then (distance xr' xl')*(distance yu' yb')
         else if xl' < xl
              then if yu' > yu then (distance yu yb')*(distance xl xr')
                   else if yb' < yb then (distance yu' yb)*(distance xl xr')
                        else (distance yu' yb')*(distance xl xr')
              else if xr' > xr
                   then if yu' > yu then (distance yu yb')*(distance xl' xr)
                        else if yb' < yb then (distance yu' yb)*(distance xl' xr)
                             else (distance yu' yb')*(distance xl' xr)
                   else if yu' > yu then (distance yu yb')*(distance xl' xr')
                        else (distance yu' yb)*(distance xl' xr')

distance : Float -> Float -> Float
distance a b = abs (a-b)

-- step the game through the states through delta and various input
stepGame : GameInput -> Game -> Game
stepGame {space,delta,so1,so2,so3,so4,amount,pnc,pc,random}
         ({state,player1,player2,player3,player4,walls,ai} as game) =
    let state' = if | lastSnakeStanding player1 player2 player3 player4 -> End
                    | otherwise                                         -> state
        
        p1' = findDirection player1 so1
        p2' = if ai then aiStep player2 [player1, player2, player3, player4] walls
              else findDirection player2 so2    -- give full control to ai in Solo mode
        p3' = findDirection player3 so3
        p4' = findDirection player4 so4

        p1'' = if not p1'.alive then p1'
              else stepSnake delta p1' walls
        p2'' = if not p2'.alive then p2'
              else stepSnake delta p2' walls
        p3'' = if not p3'.alive then p3'
              else stepSnake delta p3' walls
        p4'' = if not p4'.alive then p4'
              else stepSnake delta p4' walls
              
        (p1''',p2''',p3''',p4''') = snakeInteraction (p1'',p2'',p3'',p4'')
        (p1'''',p2'''',p3'''',p4'''') = speedAdjustment (p1''',p2''',p3''',p4''') walls
        
        (pn1,pn2,pn3,pn4) = (pnc.p1, pnc.p2, pnc.p3, pnc.p4)
    in  case state' of
            Start -> if space then defaultGame Play amount pn1.string pn2.string 
                                   pn3.string pn4.string pc.p1 pc.p2 pc.p3 pc.p4 random
                     else game
            Play  -> { game | state      <- state'
                            , player1    <- p1''''
                            , player2    <- p2''''
                            , player3    <- p3''''
                            , player4    <- p4'''' }
            End   -> if space then defaultGame Play amount pn1.string pn2.string 
                                   pn3.string pn4.string pc.p1 pc.p2 pc.p3 pc.p4 random
                     else { game | state <- state' }

gamePlay : Signal Game
--gamePlay = foldp stepGame (defaultGame Start 2 "" "" "" "" Color.lightBlue Color.lightBlue
--                           Color.lightBlue Color.lightBlue []) gameInput
gamePlay = lift (Debug.watch "Game state") (foldp stepGame 
            (defaultGame Start 2 "" "" "" "" Color.lightBlue Color.lightBlue
             Color.lightBlue Color.lightBlue 0) gameInput) 

type CustomGame = { game:Game, amount:Int, pnContents:PlayerNameContents, kd:[KeyCode],
                    pkeys:PlayerKeys }

customizedGamePlay : Signal CustomGame
customizedGamePlay = CustomGame <~ gamePlay 
                                 ~ amountPlayers.signal
                                 ~ playerNameContents
                                 ~ Keyboard.keysDown
                                 ~ playerKeys

----------
-- VIEW --
----------

-- this method has been adjusted to recycle its use for the custom walls
showPlayer' : Color -> (Float, Float) -> (Int,Int) -> Orientation -> Bool -> Form
showPlayer' color (x, y) (w,h) o filled' =
    let fw = toFloat w
        (xOffset, yOffset, degs) = case o of
                    N -> (0, fw / 2, 90)
                    E -> (fw / 2, 0, 0)
                    S -> (0, -fw / 2, -90)
                    W -> (-fw / 2, 0, 180)
        r' = rect (fw) (toFloat h)
        r = if filled' then filled color r' else outlined (solid color) r'
    in r
        |> move (x, y)
        |> move (xOffset, yOffset)
        |> rotate (degrees degs) 

-- example use: showTail Color.lightBlue [(10, 10), (20, 10)]
showTail : Color -> [(Float, Float)] -> Form
showTail color positions = 
    traced { defaultLine |
        width <- tailWidth
        , color <- color
        } (path positions)

tailWidth = 2

-- returns the drawn player if player is alive
showLivePlayer : Snake -> [Form]
showLivePlayer ({sh,st,so,alive,color} as s) =
    if not alive then []
    else [showPlayer' color sh (playerW,playerH) (toOrientation so) False]
         ++ [showTail color st]

-- draws a wall
drawWalls : Wall -> Form
drawWalls ({pos,wo}) =
    let (x,y) = pos
        (u,r) = wo
        -- reposition to the bottom or left end of the rectangle (depending on orientation)
        pos' = if u == 1 then (x - (wallWidth/2), y)
               else (x, y - (wallWidth/2))
    in  showPlayer' Color.darkPurple pos' (wallWidth,wallHeight) (toOrientation wo) True
    

playerSelection : Int -> [Form]
playerSelection a = [move (0,halfHeight-50) (toForm (flow right [
      color (buttonPressed 1 a) (button amountPlayers.handle 1 "Solo")
    , color (buttonPressed 2 a) (button amountPlayers.handle 2 "2P")
    , color (buttonPressed 3 a) (button amountPlayers.handle 3 "3P")
    , color (buttonPressed 4 a) (button amountPlayers.handle 4 "4P") ]))]

-- gives a button a lightBlue color if it's selected
buttonPressed : Int -> Int -> Color
buttonPressed p ps = if p == ps then lightBlue else grey

nameField : Handle Content -> String -> Content -> Element
nameField handle placeHolder fieldContent =
    field defaultStyle handle identity placeHolder fieldContent

playersCustomizations : PlayerNameContents -> [KeyCode] -> PlayerKeys -> [Form]
playersCustomizations { p1,p2,p3,p4 } kd pkeys =
    let pkeys1 = pkeys.p1
        pkeys2 = pkeys.p2
        pkeys3 = pkeys.p3
        pkeys4 = pkeys.p4
    in
        [ move (0,150) (toForm (flow right [
          flow down [
            color white (nameField playerName1.handle "Player 1" p1)
          , color white (colorDropDown playerColor1.handle)
          , color white (button ck1up.handle kd (append "up: " (fromList [fromCode pkeys1.up])))
          , color white (button ck1down.handle kd (append "down: " (fromList [fromCode pkeys1.down])))
          , color white (button ck1left.handle kd (append "left: " (fromList [fromCode pkeys1.left])))
          , color white (button ck1right.handle kd (append "right: " (fromList [fromCode pkeys1.right]))) ]
        , flow down [
            color white (nameField playerName2.handle "Player 2" p2)
          , color white (colorDropDown playerColor2.handle)
          , color white (button ck2up.handle kd (append "up: " (fromList [fromCode pkeys2.up])))
          , color white (button ck2down.handle kd (append "down: " (fromList [fromCode pkeys2.down])))
          , color white (button ck2left.handle kd (append "left: " (fromList [fromCode pkeys2.left])))
          , color white (button ck2right.handle kd (append "right: " (fromList [fromCode pkeys2.right]))) ]
        , flow down [
            color white (nameField playerName3.handle "Player 3" p3)
          , color white (colorDropDown playerColor3.handle)
          , color white (button ck3up.handle kd (append "up: " (fromList [fromCode pkeys3.up])))
          , color white (button ck3down.handle kd (append "down: " (fromList [fromCode pkeys3.down])))
          , color white (button ck3left.handle kd (append "left: " (fromList [fromCode pkeys3.left])))
          , color white (button ck3right.handle kd (append "right: " (fromList [fromCode pkeys3.right]))) ]
        , flow down [
            color white (nameField playerName4.handle "Player 4" p4)
          , color white (colorDropDown playerColor4.handle)
          , color white (button ck4up.handle kd (append "up: " (fromList [fromCode pkeys4.up])))
          , color white (button ck4down.handle kd (append "down: " (fromList [fromCode pkeys4.down])))
          , color white (button ck4left.handle kd (append "left: " (fromList [fromCode pkeys4.left])))
          , color white (button ck4right.handle kd (append "right: " (fromList [fromCode pkeys4.right]))) ]
        ]))]

startScreen : Int -> PlayerNameContents -> [KeyCode] -> PlayerKeys -> [Form]
startScreen a pnc kd pkeys = 
    [move (0,200-halfHeight) (toForm (centered 
        (Text.color white (toText "> Press space to start a new game <"))))]
    ++ (playerSelection a)
    ++ (playersCustomizations pnc kd pkeys)

printWinner : Snake -> Snake -> Snake -> Snake -> [Form]
printWinner s1 s2 s3 s4 =
    let winner = if s1.alive then s1.name
                 else if s2.alive then s2.name
                      else if s3.alive then s3.name
                           else if s4.alive then s4.name
                                else "Noone"
        winMsg = winner ++ " won."
    in  [move (0,100) (toForm (centered (Text.color white (toText winMsg))))]

endScreen : Game -> [Form]
endScreen { player1, player2, player3, player4 } = 
    [toForm (centered 
        (Text.color white (toText "> Press space to start a new game <")))]
    ++ printWinner player1 player2 player3 player4

showGamePlay : CustomGame -> Element
showGamePlay ({game,amount,pnContents,kd,pkeys}) =
    let (state,p1,p2,p3,p4,w) = (game.state, game.player1, game.player2,
                                 game.player3, game.player4, game.walls)
        elements = case state of
                        Start -> startScreen amount pnContents kd pkeys
                        Play  -> 
                            let playerForms = map showLivePlayer [p1,p2,p3,p4]
                                walls = map drawWalls w
                            in  (concat playerForms) ++ walls
                        End   -> endScreen game
        forms = [filled black (rect width height)] ++ elements
    in  collage width height forms





-- main function --
main = lift showGamePlay customizedGamePlay

