import Data.Maybe
import Data.List

type Place = Char
type LabelledPlace = (Place, Integer)
type Neighbours = (Place, [LabelledPlace])
type Network = [Neighbours]

places = ['b', 'c']
start = 'a'



--travel _ [] _ = 0
--travel start placesToVisit network = map  placesToVisit

myNetwork :: Network
myNetwork = [('a', [('b', 10), ('c', 11)]),
             ('b', [('a', 10)]),
             ('c', [('a', 11)])]

getDistance :: Place -> Place -> Network -> Integer
getDistance a b network = getLabelledPlaces b $ getLabelledPlaces a network
                          where getLabelledPlaces place network = fromJust $ lookup place network
