module EvoHS
    (
        next,
        randDouble,
        randInt,
        roulleteSelector1to2,
        roulleteSelector1to1,
        rankSelector1to1,
        rankSelector1to2,
        orderCrossoverTo2,
        onePointCrossoverTo1,
        onePointCrossoverTo2,
        relativeMutation,
        switchMutation,
        getBestIndividualFn,
        getBestIndividual,
        runEvolution
    ) where

import Data.List

-- Generate a pseudorandom number sequence (use last number to generate the *next* one)
next::Double -> Double
next seed = (fromIntegral (mod (round ((seed * 15485863)^3)) (round 2038074743))) /2038074743

-- Get a random integer in given range from a randomly generated number
-- from: inclusive start of range
-- to:   exclusive end of range
-- rnd:  randomly generated number in range (0,1)
randInt::Int -> Int -> Double -> Int
randInt from to rnd 
    | ret==to = from
    | otherwise = ret
    where range=to-from
          ret = round (rnd * (fromIntegral (range)))+from

-- Get a random integer in given range from a randomly generated number
-- from: exclusive start of range
-- to:   exclusive end of range
-- rnd:  randomly generated number in range (0,1)
randDouble::Double->Double->Double->Double
randDouble from to rnd = ((next rnd) * range) + from
    where range=to-from

-- Generate a new population (do 1 step of evolution) with given functions
newPopulationGenerator :: Double -> [a] -> (a->b) -> (Double -> [a] -> [b] -> [(a,a)]) -> (Double -> (a,a)->[a]) -> (Double -> a -> a) -> [a]
newPopulationGenerator rnd population fitnessFunction parentsSelector crossover mutator =
    mutatedPopulation
    where fitnesses = map fitnessFunction population
          parents = parentsSelector rnd population fitnesses
          breadPopulation = concat (zipWith crossover (iterate (\x -> next (x*13)) rnd) parents)
          mutatedPopulation = zipWith mutator (iterate (\x -> next (x*2)) rnd) breadPopulation

-- Select one pair of parents using the roullete selection
roulleteSelect1:: [a] -> [Double] -> Double -> (a,a)
roulleteSelect1 population probabilities rnd = (fst a, fst b)
    where a = head (filter (\(_, p) -> p > rnd) (zip population probabilities))
          rnd2 = next rnd
          b = head (filter (\(_, p) -> p > rnd2) (zip population probabilities))

-- Generate a population of `nPairs` using the roullete selection method
roulleteSelector::Int -> Double -> [a] -> [Double] -> [(a,a)]
roulleteSelector nPairs rnd population fitnesses =
    take (nPairs) (map (roulleteSelect1 population (fit2CumProb fitnesses))
    (iterate (\x -> next (x+1)) rnd))

-- Generate a population of `n` pairs using the roullete selection method (use with `..to1` crossovers)
roulleteSelector1to2::Double -> [a] -> [Double] -> [(a,a)]
roulleteSelector1to2 rnd population fitnesses =
    roulleteSelector (length population) rnd population fitnesses

-- Generate a population of `n/2` pairs using the roullete selection method (use with `..to2` crossovers)
roulleteSelector1to1::Double -> [a] -> [Double] -> [(a,a)]
roulleteSelector1to1 rnd population fitnesses =
    roulleteSelector (div (length population) 2) rnd population fitnesses

-- Use given selector on ranks instead of on fitnesses
rankSelector::(Double -> [a] -> [Double] -> [(a,a)]) -> Double -> [a] -> [Double] -> [(a,a)]
rankSelector selector rnd population fitnesses =
    selector rnd sortedPopulation (take (length population)(iterate (+1) 1))
    where sortedPopulation = map fst (sortBy (\(_,x) (_,y) -> compare x y) (zip population fitnesses))

-- Generate population of `n/2` pairs using the roullete selection on ranks
rankSelector1to1::Double -> [a] -> [Double] -> [(a,a)]
rankSelector1to1 rnd population fitnesses =
    rankSelector roulleteSelector1to1 rnd population fitnesses

-- Generate population of `n` pairs using the roullete selection on ranks
rankSelector1to2::Double -> [a] -> [Double] -> [(a,a)]
rankSelector1to2 rnd population fitnesses =
    rankSelector roulleteSelector1to2 rnd population fitnesses

-- Transform fitnesses to probabilities of these to be picked
-- Linearly transforms the probabilities such that the lowest gets 0 probability
-- and for the rest the probability is split linearly.
fit2CumProb::[Double] -> [Double]
fit2CumProb fitnesses = scanl1 (+) probs
    where totalMin = foldr1 min fitnesses
          scaledFitnesses = map (\x -> x - totalMin) fitnesses
          totalSum = sum scaledFitnesses
          probs = if totalSum /=0
            then map (\x -> x/totalSum) scaledFitnesses
            else map (\_ -> 1/(fromIntegral (length fitnesses))) scaledFitnesses

-- Crossover using the order method (used for combinatoric optimization)
orderCrossoverTo2:: Eq a => Double -> ([a], [a]) -> [[a]]
orderCrossoverTo2 rnd (x, y) =
    [concat [take firstPoint firstRest, firstMiddle, drop firstPoint firstRest],
        concat [take firstPoint secondRest, secondMiddle, drop firstPoint secondRest]]
    where firstPoint = randInt 0 (length x) rnd
          secondPoint = randInt firstPoint (length x) (next rnd)
          range = secondPoint - firstPoint
          firstMiddle = take range (drop firstPoint x)
          secondMiddle = take range (drop firstPoint y)
          firstRest = y \\ firstMiddle
          secondRest = x \\ secondMiddle

-- Crossover using the one point method to generate 1 offspring
onePointCrossoverTo1::Double ->([a], [a]) -> [[a]]
onePointCrossoverTo1 rnd (x, y) = [concat [take k x, drop k y]]
    where k = round (rnd * (fromIntegral (length x)))

-- Crossover using the one point method to generate 2 offsprings
onePointCrossoverTo2::Double ->([a], [a]) -> [[a]]
onePointCrossoverTo2 rnd (x, y) = [concat [take k x, drop k y], concat[drop k x, take k y]]
    where k = round (rnd * (fromIntegral (length x)))

-- Do relative mutation on Double vector with probability of prob per dimension
relativeMutation::Double->Double->Double->[Double]->[Double]
relativeMutation prob amount rnd individual =
    zipWith (\rnd value -> relativeMutation1 prob amount rnd value)
        (iterate (\x -> next (x*11)) rnd) individual

-- Do relative mutation on 1 individual 
relativeMutation1::Double->Double->Double->Double->Double
relativeMutation1 prob amount rnd value
    | prob>rnd = value
    | otherwise = value + mutationAmount
    where mutationAmount = (2*(next rnd) - 1)*amount

-- Do 1 switch mutation on a vector with probability of prob
switchMutation::Double -> Double -> [a] -> [a]
switchMutation prob rnd individual
    | prob < rnd = individual
    | otherwise = switchedIndividual
    where
        firstPoint = randInt 0 ((length individual)-1) rnd
        secondPoint = randInt 0 ((length individual)-1) (next (fromIntegral firstPoint))
        switchedIndividual = swapTwo firstPoint secondPoint individual

-- Swap the `first` and `second` element in a list
swapTwo::Int -> Int -> [a] -> [a]
swapTwo first second xs = zipWith (\x y -> 
    if x == first then xs !! second
    else if x == second then xs !! first
    else y) [0..] xs

-- Run the evolution with the given attributes
runEvolution :: Double -> [a] -> (a->b) -> (Double -> [a] -> [b] -> [(a,a)]) -> (Double -> (a,a)->[a]) -> (Double -> a -> a) -> Int-> [a]
runEvolution rnd population fitnessFunction parentsSelector crossover mutator epochs
    | epochs > 0 = runEvolution
        (next (rnd^7))
        (newPopulationGenerator rnd population fitnessFunction parentsSelector crossover mutator)
        fitnessFunction
        parentsSelector
        crossover
        mutator
        (epochs-1)
    | otherwise = population

-- Get the best individual and its fitness provided a population and a fitness function
getBestIndividualFn::[a] -> (a -> Double) -> (a, Double)
getBestIndividualFn population fitnessFunction =
    getBestIndividual population (map fitnessFunction population)

-- Get the best individual and its fitness provided a population and fitnesses for each individual in the population
getBestIndividual::[a] -> [Double] -> (a, Double)
getBestIndividual population fitnesses =
    foldl1 (\(x0,y0) (x1, y1) -> if y0>y1 then (x0,y0) else (x1,y1)) (zip population fitnesses)
