import EvoHs
-- This file contains an example usage of EvoHS
-- to optimize a simple quadratic function in 8 dimensions
-- (The results are not very impressive, because this isn't the
-- best algorithm to do this with, e.g. hill climbing algo would be much better)

quadraticFitness::[Double]->Double
quadraticFitness individual = - (sum (map (^2) individual))

generateDoubleIndividual::Double->Double->Double->Int->[Double]
generateDoubleIndividual rnd min max dimensions =
    map (\x -> randDouble min max x) (take dimensions (iterate (\x -> next (x*5)) rnd))

-- Default population for any optimization problem
defaultPopulation::Double->Double->Int->Int->[[Double]]
defaultPopulation min max dimensions nIndividuals =
    take nIndividuals (map (\x-> generateDoubleIndividual x min max dimensions)
        (iterate (\x -> next (x*7)) rnd))
    where rnd=next 1024

defaultQuadraticPopulation dimensions nIndividuals =
    (\x y -> defaultPopulation (-5.12) 5.12 x y) dimensions nIndividuals
myPopulation = defaultQuadraticPopulation 8 1000
trainedPopulation =
    (runEvolution (next 1024)
                  myPopulation
                  quadraticFitness
                  roulleteSelector1to1
                  onePointCrossoverTo2
                  (relativeMutation 0.2 1)
                  10)

--([-0.9080344793419576,0.5623035449981046,-9.325755900405686e-2,0.9290578161293663,-0.5847515219416075,-0.6386573984642135,-0.31755211309244813,0.23452161234108304],
-- -2.918214636624361) <- fitness
bestTrained = getBestIndividualFn trainedPopulation quadraticFitness
-- ([2.3904682592104516,-1.15536193255303,-1.244155956531571,-0.37823548150412467,-1.079644838521018,-0.6340454778110169,-1.3181264359155058,1.843883601750714],
-- -15.44519650026914) <- fitness
bestUntrained = getBestIndividualFn myPopulation quadraticFitness
