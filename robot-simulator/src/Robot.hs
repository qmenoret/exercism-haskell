module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

type Position = (Integer, Integer)
data Robot = Robot Bearing Position

bearing :: Robot -> Bearing
bearing (Robot b _) = b

coordinates :: Robot -> Position
coordinates (Robot _ pos) = pos

mkRobot :: Bearing -> Position -> Robot
mkRobot = Robot

simulate :: Robot -> String -> Robot
simulate = foldl move
    where
        move r 'A' = advance r
        move (Robot b p) 'R' = Robot (turnRight b) p
        move (Robot b p) 'L' = Robot (turnLeft b) p

advance :: Robot -> Robot
advance (Robot North (x,y)) = Robot North (x,y+1)
advance (Robot East  (x,y)) = Robot East  (x+1,y)
advance (Robot South (x,y)) = Robot South (x,y-1)
advance (Robot West  (x,y)) = Robot West  (x-1,y)

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft East  = North
turnLeft South = East
turnLeft West  = South

turnRight :: Bearing -> Bearing
turnRight West  = North
turnRight North = East
turnRight East  = South
turnRight South = West
