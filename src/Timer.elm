module Timer exposing (..)

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

updateTimer : Timer -> Int -> Timer
updateTimer timer currentTime =
  { timer | elapsed = (currentTime -  timer.start) }
