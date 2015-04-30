import Data.Maybe
import Data.List

type Place = Char
type Network = [Neighbours]

myNetwork :: Network
myNetwork = [('a', [('b', 10), ('c', 11)]),
             ('b', [('a', 10), ('c', 22)]),
             ('c', [('a', 11), ('b', 22)])]

getDistance :: Place -> Place -> Network -> Integer
getDistance a b network = getLabelledPlaces b $ getLabelledPlaces a network
                          where getLabelledPlaces place network = fromJust $ lookup place network

shortestRouteLength :: [Place] -> Place -> Network -> Integer
shortestRouteLength [] _ _ = 0
shortestRouteLength (a:[]) start network = getDistance a start network
shortestRouteLength places start network = minimum $ map (\place -> getDistance start place network + shortestRouteLength (filter (place /=) places) place network) places

placesInNetwork :: Network -> [Place]
placesInNetwork = map fst

shortestRoute :: Network -> Place -> Integer
shortestRoute network start = shortestRouteLength (filter (start /=) (placesInNetwork network)) start network

