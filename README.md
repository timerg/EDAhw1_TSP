# Use state monad to solve traveling salesman problem

A Haskell project using state monad to implement cc and "Branch and Bound" method for solving traveling salesman problem (TSP).
The "Nearest neighbour" algorithm may not find a solution and the "Branch and Bound" can be taken as a brute force method.

## Getting Started

### Prerequisites

* [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)

### Installing

```
cd HaskellForTSP
 
stack setup
 
stack build
```


## Running the tests
An input data should be a txt file. An example is given in `data/hw1.TSP.txt`. 

You can run the test with command:
```
stack exec TSP [option] [Your data path]
```

There are three option you can use. Each option gives different output:
* If the option is not specific, you will get single solution such as:

```
total length: 55
cycle: [1,3,10,12,14,15,13,11,5,7,9,8,6,4,2,1]
```

* With option of `All` you will get all valid solution
```
stack exec TSP All [Your data path]
```

* With option of `NN`, you will get all results from "Nearest neighbour" algorithm.
```
stack exec TSP NN [Your data path]
```







## Acknowledgments

* [Travelling salesman problem](https://en.wikipedia.org/wiki/Travelling_salesman_problem#Constructive_heuristics)
* [About monad](http://adit.io/posts/2013-06-10-three-useful-monads.html#the-state-monad)
