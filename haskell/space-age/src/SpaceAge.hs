module SpaceAge (Planet(..), ageOn) where

type Seconds = Float
type Years = Float

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Seconds -> Years
ageOn planet seconds = seconds / (earthYear * orbitalPeriod planet)

orbitalPeriod :: Planet -> Float
orbitalPeriod Earth   = 1
orbitalPeriod Mercury = 0.2408467
orbitalPeriod Venus   = 0.61519726
orbitalPeriod Mars    = 1.8808158
orbitalPeriod Jupiter = 11.862615
orbitalPeriod Saturn  = 29.447498
orbitalPeriod Uranus  = 84.016846
orbitalPeriod Neptune = 164.79132

earthYear :: Seconds
earthYear = 31557600
