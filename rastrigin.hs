import EvoHs
-- This file contains an example usage of EvoHS
-- to optimize the rastrigin function in 8 dimensions

-- The rastrigin function which can be optimised (minimum @ f(0, ...) = 0)
-- This is the rastrigin function * -1 (the library supports only maximisation)
rastriginFitness:: [Double] -> Double
rastriginFitness vector = - (a*n + sum (map (\x -> x^2 - (a * cos (2*pi*x))) vector))
    where a = 10
          n = fromIntegral (length vector)


-- Generate an individual (vector of Doubles) in the given range
-- with the given amount of dimensions
generateDoubleIndividual::Double->Double->Double->Int->[Double]
generateDoubleIndividual rnd min max dimensions = map (\x -> randDouble min max x) (take dimensions (iterate (\x -> next (x*5)) rnd))

-- Default population for any optimization problem
defaultPopulation::Double->Double->Int->Int->[[Double]]
defaultPopulation min max dimensions nIndividuals = take nIndividuals (map (\x-> generateDoubleIndividual x min max dimensions) (iterate (\x -> next (x*7)) rnd))
    where rnd=next 1024

-- Default population for the rastrigin problem
defaultRastriginPop dimensions nIndividuals = (\x y -> defaultPopulation (-5.12) 5.12 x y) dimensions nIndividuals

myPopulation = defaultRastriginPop 8 1000
trainedPopulation = runEvolution (next 1024)
                                 myPopulation
                                 rastriginFitness
                                 roulleteSelector1to1
                                 onePointCrossoverTo2
                                 (relativeMutation 0.3 0.7)
                                 20

-- ([-1.1123039850163137,7.763511886080071e-2,1.1185001744953176,1.7616461200903082,-3.1172050984980826e-3,-3.8939521640497965e-2,-0.7174327119856765,1.3346652488299027e-2],
-- -33.95028736917697) <- fitness after training
bestTrained = getBestIndividualFn trainedPopulation rastriginFitness

-- ([-0.9535954582800104,2.991077215542531,-0.8950172296208079,0.9231920035034751,2.5782042071064515,1.2334248903844047,1.021300005973334,-7.970176249811889e-2],
-- -53.499388499711166) <- fitness on generated population
bestUntrained = getBestIndividualFn myPopulation rastriginFitness
