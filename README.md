# purification

Source code used to generate the data in "Post-selection-free preparation of high-quality physical qubits" by Ben Barber, Neil I. Gillespie and J. M. Taylor.

This is a Haskell Stack project.

## Example usage

`stack run` to print basic usage information, including a list of known circuits.

    stack run goodTriples 3-1-1 0.001

Prints triples c,t,p (space separated, one per line) for which running a (3,1,1) circuit with idle depolarisation rate 0.1% results in an improvement over naive preparation.

    stack run threshold 7-4-1 0.001

Prints gnuplot grid format data for a threshold plot for the (7,4,1) circuit at 0.1% idle depolarisation rate, CNOT depolarisation rates up to 5% and Toffoli depolarisation rates between 1 and 3 times the CNOT rate.

    stack run fixedICT 5-1-2a idle cnot toffoli

Prints preparation vs output error rates for the type (a) (5,1,2) circuit given idle, CNOT and Toffoli depolarisation rates.

    stack run leadingOrder circuitName

Prints k, the internal representation of its leading order error terms and a pretty-printed LaTeX format.

    stack run print circuitName

Print tikz/yquant form of circuit for including in LaTeX documents.


## File structure

### app/Main.hs

Expose the most common simulation functions via the command line.

### src/Binomial.hs

Helper functions for working with binomial probabilities.

### src/Circuits.hs

Data types, general constructions and helper functions (e.g. pretty printing of circuits).

### src/Examples.hs

The specific circuits simulated in the paper.

### src/Pathfinding.hs

Generic pathfinding functions, used in `Search.hs` to find small purification circuits.

### src/Search.hs

Searches the space of short circuits to find small purification circuits.

### src/Simulate.hs

Simulation of circuit performance by computing the polynomial in preparation, idle, CNOT and Toffoli error rates of each possible outcome.  Implementation of the numerical functions exposed in `Main.hs`.


## Performance

The code tries to get the get the big performance calls correct, but hasn't been optimised beyond that for either speed or memory usage.

The main performance gain during simulation comes from not tracking the probability polynomials which make a negligible contribution.
In most cases this is done automatically and conservatively by capping each of the number of idle, CNOT and Toffoli faults tracked such that the probability of exceeding the cap is less than the tolerance parameter `eps`; numerical probability estimates should then be accurate to within `3*eps` plus any floating point error.

We also save during polynomial evaluation by substituting variables one at a time rather than evaluating naively, allowing us to reuse partial evaluations when stepping over ranges of parameters.

Threshold computation is the most expensive operation (it works to higher resolution than `goodTriples`), and completes in hours for all included examples.
Memory usage is typically reasonable, around or below 2GB.
The large fault-tolerant light cone is an exception.
Here the exponential cost of simulation begins to bite, and it uses around 50GB of RAM.

Since `leadingOrder` is not reporting numerical results, it does not trim the polynomials in this way.
It still completes in reasonable time and memory for all circuits but the large fault-tolerant light cone.
The simplest way to obtain the leading order error probabilities for the large fault-tolerant light cone is to change the definition of `leadingOrder` to only work to degree 2 in the preparation error rate and degree one in each other error rate.
This returns the result

    6\prep^2+(\idle/2)+3(\cnot/4)+12(\toffoli/8)

Since every term is minimal in the set of monomials which aren't a constant multiple of 1 or \prep, increasing the degree bounds would not affect this result.
This process could be automated, but since this was the only case in which the naive approach did not complete using a reasonable amount of resources there was no incentive to do so.