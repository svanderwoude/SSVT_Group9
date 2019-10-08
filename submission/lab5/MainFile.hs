module MainFile where
import Ass1
import Ass2
import Ass3
-- import Ass4
import Ass5
import Ass6
import Data.List
import Test.QuickCheck

main = do
    putStrLn "\n==== ASSIGNMENT 1 ===="


    putStrLn "\n==== ASSIGNMENT 2 ===="
    putStrLn "Testing composite implementation:"
    compositeTest

    putStrLn "\n==== ASSIGNMENT 3 ===="
    putStrLn "Find first composite number to fail primeTestsF for k=1:"
    f1Fail <- findFirstFailF1
    print f1Fail
    putStrLn "\nFind first composite number to fail primeTestsF for k=2:"
    f2Fail <- findFirstFailF2
    print f2Fail
    putStrLn "\nFind first composite number to fail primeTestsF for k=3:"
    f3Fail <- findFirstFailF3
    print f3Fail

    putStrLn "\n==Ex 5 ==\n"

    putStrLn "List of Mersenne Primes (exponents): "
    print $ realPrimes
    
    putStrLn "\nList of Mersenne Primes: "
    print $ mersenneP
    
    -- Verify list of mersenne primes using Lucas-Lehmer test and checkMersennePrime
    let printMersennes = (\x -> putStrLn $ "\nAfter Lucas-Lehmer test + checkMersennePrime: " ++ show x)
    printMersennes $ filter lucasLehmer $ realPrimes
    
    -- Verify whether the numbers that realPrimes found are genuine Mersenne primes using Miller-Rabin test.
    let testMersennes = millerR 1 realPrimes
    useAllBools (testMersennes)
    
    -- From the results we can say realPrimes (using checkMersennePrime and prime) returns mersenne primes.
    -- however it can take a lot of processing power due to its exponential complexity.
    
    putStrLn "\n====\n"
    
    -- Prints list of mersenne primes using Sieve of Eratosthenes and Lucas-Lehmer test
    -- This function
    let printMersennes = \x -> putStrLn $ "After Lucas-Lehmer + Sieve of Eratosthenes: " ++ show x
    printMersennes $ take 20 $ filter lucasLehmer $ sieve [2..]
    
    -- Verify whether the numbers that Sieve of Eratosthenes and Lucas-Lehmer test found are genuine Mersenne primes.
    let testMersennesVersion2 = millerR 1 $ take 20 $ filter lucasLehmer $ sieve [2..]
    useAllBools (testMersennesVersion2)
    
    -- From the results we can say Sieve of Eratosthenes and Lucas-Lehmer test returns genuine mersenne primes.
    -- and it showed a better performance overall.
    
    -- We found that a prime number in order to be a Mersenne prime it requires both ((2^p) - 1) to be a prime and p to be a prime.

    putStrLn "\n==END OF ASSIGNMENT 5 ==\n"

    

    putStrLn "\n==== ASSIGNMENT 6 ===="
    putStrLn "Testing tree1 for all coprimes:"
    quickCheckTree1
    putStrLn "\nTesting tree1 for expected list of pairs:"
    quickCheckTree1List
    putStrLn "\nTesting tree2 for all coprimes:"
    quickCheckTree2
    putStrLn "\nTesting tree2 for expected list of pairs:"
    quickCheckTree2List
