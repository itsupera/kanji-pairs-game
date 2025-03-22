module Timer exposing (..)

import Time exposing (Posix, posixToMillis)

type alias Timer =
  { active : Bool
  , start : Int
  , elapsed : Int
  }

stoppedTimer : Timer
stoppedTimer =
  { active = False
  , start = 0
  , elapsed = 0
  }

activatedTimer : Timer
activatedTimer =
  { active = True
  , start = 0
  , elapsed = 0
  }

startTimer : Int -> Timer
startTimer currentTime =
    { active = True
    , start = currentTime
    , elapsed = 0
    }


updateTimer : Timer -> Time.Posix -> Timer
updateTimer timer posixTime =
  let
    currentTime = posixToMillis posixTime
  in
    case (timer.active, timer.start) of
      (False, _) -> timer
      (True, 0) -> startTimer currentTime
      (True, _) -> { timer | elapsed = (currentTime -  timer.start) }
  