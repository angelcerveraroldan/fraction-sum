package com.angelcerveraroldan.fractionsum

import scala.collection.parallel.CollectionConverters._

// Generate data
object PrecalculateData {
  type DenominatorPair = (Int, Int)

  // 1/n = 1/a + 1/b for any (a, b) tuple returned
  // a := n + k
  def nextSeq(n: Int): Seq[DenominatorPair] = {
    // any value above this will make the first term a decimal, and therefore b wont be an integer
    val largestK = n * n
    val fromK: Int => DenominatorPair = k => (n + k, (largestK / k) + n)

    // Should the first filter be n^2 % k??
    // Second filter seems like a waste of performance... (you check all elements, just to remove one)
    (1 to largestK)
      .filter(k => n % k == 0)
      .map(fromK)
      .filterNot { case (a, b) => a == b }
  }

  // Generate a hashmap in the form (n, Seq(sets of fractions that will add to 1/n))
  def prepareMapEquivalence(limit: Int): Map[Int, Seq[(Int, Int)]] =
    (2 to limit).par
      .map(n =>
        (
          n,
          // Remove tuples out of scope
          nextSeq(n).filter(t => t._2 <= limit)
        )
      )
      .filterNot { case (_, b) => b.isEmpty }
      .toMap
      .seq
}
