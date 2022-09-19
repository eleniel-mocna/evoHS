import EvoHS
import Data.List

-- Calculate euclidean distance between two vectors
euclideanDistance::[Double]->[Double]->Double
euclideanDistance x y =sqrt (sum (zipWith (\a b -> (a -b)^2) x y))

-- Travelling salesman fitness to maximize
tSPFitness::[[Double]] -> [Int] -> Double
tSPFitness graph ordering = - (foldl1  (+) (map (\(x,y) -> euclideanDistance x y) (zip orderedGraph (drop 1 orderedGraph))))
    where orderedGraph = map fst (sortBy (\(_, x) (_, y) -> compare x y)(zip graph ordering))

-- Cut shuffle needed for thoroughShuffle
cutShuffle::(Double, [a]) -> (Double, [a])
cutShuffle (rnd, input) = (next rnd, concat [drop cutPlace (take (cutPlace+range) input), take cutPlace input, drop (cutPlace + range) input])
    where cutPlace = randInt 0 (length input) rnd
          range = randInt 0 ((length input)-cutPlace) rnd

-- Thorough shuffling needed for rabdin initialization
thoroughShuffle::Double -> [a] -> [a]
thoroughShuffle rnd input = snd ((iterate cutShuffle (cutShuffle (rnd, input))) !! nShuffles)
    where nShuffles = (length input)^2 -- I'm not sure this is enough... But for lists short enough this will work and run fast enough

-- Get `nPoints` on a `dimensions`-dimensional space in the (-`range`, `range`) range
getGraph::Double -> Int -> Int -> Double -> Double -> [[Double]]
getGraph rnd nPoints dimensions minRange maxRange = take nPoints (map (\x -> getPoint x dimensions) (iterate (\x -> next (x*9)) rnd))
    where getPoint rnd2 dimensions = take dimensions (map (\x -> if x>0.5 then randDouble minRange maxRange x else -(randDouble minRange maxRange x)) (iterate next rnd2))

-- Randomly initialized population
tSPDefaultPopulation::Int->Int->[[Int]]
tSPDefaultPopulation nIndividuals graphPoints = take nIndividuals (map (\x-> thoroughShuffle x [0..(graphPoints-1)]) (iterate next (0.2)) )

myPopulation = tSPDefaultPopulation 1000 80
myGraph = getGraph 0.1 80 2 10 15 
tSPTrained = runEvolution  (next 1024) myPopulation (\x -> tSPFitness myGraph x) roulleteSelector1to1 orderCrossoverTo2 (switchMutation 0.8) 10
secondTSPTrained = runEvolution  (next 512) tSPTrained (\x -> tSPFitness myGraph x) rankSelector1to1 orderCrossoverTo2 (switchMutation 0.4) 10

-- This needs more tweaking. But as a proof of usage this works.

untrainedBestIndividual =
    getBestIndividualFn myPopulation (\x -> tSPFitness myGraph  x)
trainedBestIndividual =
    getBestIndividualFn tSPTrained (\x -> tSPFitness myGraph  x)
secondTrainedBestIndividual =
    getBestIndividualFn secondTSPTrained (\x -> tSPFitness myGraph  x)