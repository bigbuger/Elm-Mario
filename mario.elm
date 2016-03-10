import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window


-- MODEL
type alias Block =
  { x : Float
  , y : Float 
  , h : Float
  , w : Float
  }


type alias Model =
  { x : Float
  , y : Float
  , h : Float
  , w : Float
  , vx : Float
  , vy : Float
  , dir : Direction
  , blocks : List Block
  , onGround : Bool
  , moneys : List Money
  }

type alias Money = 
  { x : Float
  , y : Float 
  , h : Float
  , w : Float
  }

type Direction = Left | Right


type alias Inputs = { x:Int, y:Int, foce:Bool, dt:Float}

renderBlock : Float -> Block -> Form
renderBlock groundY {x,y} = toForm (image 20 20 "./imgs/block/block.png") |> move (x, y + groundY)

initialBlocks : List Block
initialBlocks = initialBlocks1 -- ++ initialBlocks2
initialBlocks1 = let genblock i = { x = 20 * i
                                 , y = 30
                                 , h = 18
                                 , w = 20
                                 }
                in List.map genblock [0,4,6,8,10]
initialBlocks2 = let genblock i = { x = 20 * i
                                 , y = 40
                                 , h = 18
                                 , w = 20
                                 }
                in List.map genblock [1,3,5,7,9]


renderMoney : Float -> Block -> Form
renderMoney groundY {x,y} = toForm (image 20 20 "./imgs/money/money.png") |> move (x, y + groundY)

initialMoneys : List Money
initialMoneys = let genblock i = { x = 20 * 3 * i
                                 , y = 60
                                 , h = 20
                                 , w = 10
                                 }
                in List.map genblock [0..5] 

mario : Model
mario =
  { x = 0
  , y = 200
  , vx = 0
  , vy = 0
  , h = 29
  , w = 8
  , dir = Right
  , blocks = initialBlocks
  , onGround = True
  , moneys = initialMoneys
  }

near k c n =
  n >= k-c && n <= k+c

intersect m b = 
  let dx = abs (m.x - b.x)
      dy = abs (m.y - b.y)
  in
    (dx <= m.w / 2 + b.w / 2) && (dy <= m.h / 2 + b.h / 2)


-- UPDATE

update : Inputs -> Model -> Model
update inputs mario =
  mario
    |> jump inputs
    |> walk inputs
    |> gravity inputs.dt
    |> land mario.blocks
    |> physics inputs.dt
    |> pickUp

pickUp : Model -> Model
pickUp m = 
  let moneys = m.moneys
      moneys' = List.filter (\money -> if intersect m money then False else True) moneys
  in { m |  moneys = moneys'}

jump : Inputs -> Model -> Model
jump inputs mario =
  if inputs.y > 0 && mario.onGround then
      { mario | vy = 4.0 , onGround = False }
  else if inputs.y > 0 && mario.vy > 0 then 
      {mario | vy = mario.vy + inputs.dt / 8}
  else
      mario


gravity : Float -> Model -> Model
gravity dt mario =
  { mario |
      vy = if not mario.onGround then mario.vy - dt/4 else 0
  }


physics : Float -> Model -> Model
physics dt mario =
  { mario |
      x = mario.x + dt * mario.vx,
      y = max 0 (mario.y + dt * mario.vy)
  }


walk : Inputs -> Model -> Model
walk inputs mario = 
  let normalV = 1.5
      jumpV = 1.2
      foceV = 3.0
  in
  { mario |
      
      vx = 
        if not mario.onGround then
          if mario.vx /= 0 then mario.vx
          else (toFloat inputs.x) * jumpV
        else (toFloat inputs.x) * normalV ,
      dir =
        if mario.onGround then
          if inputs.x < 0 then
              Left

          else if inputs.x > 0 then
              Right

          else
              mario.dir
        else
          mario.dir
  }

sign a = if a > 0 then 1 else if a < 0 then -1 else 0 

land : List Block -> Model -> Model
land bs m = 
  let m' = physics (1/240) m
      collisionResult = List.map (\b -> if (intersect m' b) && (m.y >= b.y + b.h)  then (b, True) else (b, False)) bs
      (onBlock, y') 
        = List.foldl 
            (\(b, result) (flag, y) -> if result then ((flag || result), max (b.y + b.h/2 + m.h/2) y)  else ((flag || result),y)) 
            (False, m.y) collisionResult
  in 
    if onBlock || m.y == 0 
    then { m | onGround =  True, vy = max 0 m.vy, y = y'} 
    else { m | onGround = False}


-- VIEW

view : (Int, Int) -> Model -> Element
view (w',h') mario =
  let
    (w,h) = (toFloat w', toFloat h')

    verb =
      if not mario.onGround then
          "jump"

      else if mario.vx /= 0 then
          "walk"

      else
          "stand"

    dir =
      case mario.dir of
        Left -> "left"
        Right -> "right"

    src =
      "./imgs/"++ verb ++ "/" ++ dir ++ ".gif"

    marioImage =
      image 35 35 src

    groundY = 62 - h/2

    position =
      (mario.x, mario.y + groundY)
  in
    collage w' h'
      ([ rect w h
          |> filled (rgb 174 238 238)
      , rect w 50
          |> filled (rgb 74 167 43)
          |> move (0, 24 - h/2)
      ] ++ (List.map (renderBlock groundY) mario.blocks)
        ++ (List.map (renderMoney groundY) mario.moneys)
        ++ [marioImage
          |> toForm
          |> move position])

-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update mario input)


input : Signal Inputs
input =
  let
    delta = Signal.map (\t -> t/20) (fps 60)
  in
    Signal.map4 Inputs
      (Signal.map .x Keyboard.arrows)
      (Signal.map .y Keyboard.arrows)
      Keyboard.space
      delta