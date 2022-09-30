# Fractoin Sum

Find a set $S$ of numbers such that the sum of the reciprocal of every element sums to one. 

If the set S has $j$ elements, then: 

$$
1 = \sum_{i = 0}^{j} \frac{1}{n_i}
$$

## Hash map generation

Generate a hash map that will store the map from a number $n \in N$, to every tuple $(a, b)$, where $a, b \in N$, such that $\frac{1}{n} = \frac{1}{a} + \frac{1}{b}$.

Because there is an infinite number of tuples that satisty that requirement, we will also set a number $l \in N$, and set a second requirement, $a, b \leq l$

If we set $l = 100$, this is the map that shold be generated. 
