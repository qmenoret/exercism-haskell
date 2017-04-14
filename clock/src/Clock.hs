module Clock (clockHour, clockMin, fromHourMin, toString) where

data Clock = Clock  { hours     :: Int
                    , minutes   :: Int
                    }
    deriving (Show, Eq)

instance Num Clock where
    Clock{hours=h1, minutes=m1} + Clock{hours=h2, minutes=m2} = mkClock h m
        where
            m=m1+m2
            h=h1+h2
    
    fromInteger cn = mkClock h m
        where
            n = fromInteger . toInteger $ cn
            m = n `rem` 60
            h = n `quot` 60

    negate Clock{hours=h, minutes=m} = Clock{hours=23-h, minutes=60-m}

mkClock :: Int -> Int -> Clock
mkClock hour min = Clock{hours=h, minutes=m}
    where
        h = (total `quot` 60) `rem` 24
        m = total `rem` 60
        total = (min + 60*hour) `rem` (60*24) + 60*24

clockHour :: Clock -> Int
clockHour = hours

clockMin :: Clock -> Int
clockMin = minutes

fromHourMin :: Int -> Int -> Clock
fromHourMin = mkClock

toString :: Clock -> String
toString clock = hh ++ ":" ++ mm
    where
        mm = if m > 9 then show m else '0':show m
        m  = minutes clock
        hh = if h > 9 then show h else '0':show h
        h  = hours clock
