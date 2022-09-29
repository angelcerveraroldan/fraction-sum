package com.angelcerveraroldan.fractionsum

import com.angelcerveraroldan.fractionsum.Main._
import com.angelcerveraroldan.fractionsum.PrecalculateData.prepareMapEquivalence
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class EnrichTest extends AnyWordSpec with Matchers {
  // Add the multiple fractions in the form 1/n, given the n's
  def addDenominators(denominators: Seq[Int]): Fraction = {
    // Turn every item into a fraction
    denominators
      .map(d => Fraction(1, d))
      // Add all fractions together
      .foldLeft(Fraction(0, 1))(_ + _)
  }

  "enrich" when {
    "limit 100" in {
      val data = prepareMapEquivalence(100)

      val answer =
        (0 to 10)
          .map(x => enrich(Seq(2, 2), data))

      answer.foreach(x =>
        println(s"${addDenominators(x)} from $x, with min 1/${x.max} ")
      )

      answer.foreach(x => addDenominators(x) shouldBe Fraction(1, 1))
    }
    "limit 1000" in {
      val data = prepareMapEquivalence(1000)

      val answer =
        (0 to 1000)
          .map(x => enrich(Seq(2, 2), data))

      answer.foreach(x => addDenominators(x) shouldBe Fraction(1, 1))

      val a = answer.map(x => (x.length, x)).maxBy(_._1)
      println(a)
    }
  }
}
