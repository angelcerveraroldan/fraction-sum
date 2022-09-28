package com.angelcerveraroldan.fractionsum

import scala.annotation.tailrec
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.util.Random

/*
List(256, 462, 840, 750, 100, 60, 420, 162, 108, 342, 468, 210, 660, 96, 190, 816, 192, 42, 448, 486, 480, 55, 432, 297, 576, 612, 133, 88, 84, 552, 440, 19, 264, 546, 9, 864, 900, 252, 896, 135, 130, 14, 728, 495, 306, 544, 650, 120, 300, 540, 72, 924, 144, 500, 240, 756, 220, 260, 441, 624, 288, 49, 36, 992, 910, 918, 182, 330, 450, 506, 180, 150, 50, 600, 768, 588, 648, 882, 90, 800, 384, 250, 912, 378, 25, 528, 594, 405, 189, 132, 195, 1000, 270, 880, 702, 294, 224, 630, 960, 396, 780, 168, 45, 216, 336, 504, 570, 156, 792, 812, 640, 616, 798, 110, 324, 684, 720, 320, 54, 280, 930, 99, 810, 770, 56, 208, 312, 520, 272, 936, 392, 784, 165, 200, 990, 360, 560, 672, 126, 13, 972, 364, 304, 870, 760, 456, 380, 128, 112)
 */

object Main extends App {
  // 1/n = 1/a + 1/b for any (a, b) tuple returned
  // a := n + k
  def nextSeq(n: Int): Seq[(Int, Int)] = {
    // any value above this will make the first term a decimal, and therefore b wont be an integer
    val largestK = n * n
    val fromK: Int => (Int, Int) = k => (n + k, ((n * n) / k) + n)

    // Should the first filter be n^2 % k??
    // Second filter seems like a waste of performance... (you check all elements, just to remove one)
    val a = (1 to largestK).filter(k => n % k == 0)
    a.map(fromK).filterNot { case (a, b) => a == b }
  }

  def enrich(denominators: Seq[Int], limit: Int = 100) = {
    // Generate a hashmap in the form (n, Seq(sets of fractions that will add to 1/n))
    val mappedEquivalence =
      (2 to limit)
        .map(n => (
          n,
          // Remove tuples out of scope
          nextSeq(n).filter(t => t._2 <= limit)
        )).filterNot { case (_, b) => b.isEmpty }.toMap

    @tailrec
    def recursiveEnrich(
                         unchecked: Seq[Int] = denominators,
                         acc: Seq[Int] = Seq.empty,
                         eq: Map[Int, Seq[(Int, Int)]] = mappedEquivalence): Seq[Int] = {
      if (unchecked.isEmpty) {
        acc
      } else {
        //println(s"${(unchecked ++ acc).length} -> ${(unchecked).sorted} ${acc.sorted}")
        if (!eq.contains(unchecked.head)) {
          recursiveEnrich(unchecked.tail, acc :+ unchecked.head)
        } else {

          // TODO: Try to use EVERY element that fits the predicate, not any
          @tailrec
          def findElemOrElse[V](from: Seq[V], predicate: V => Boolean, e: V): V = from match {
            case x if x.isEmpty => e
            case _ => if (predicate(from.head)) {
              from.head
            } else {
              findElemOrElse(from.tail, predicate, e)
            }
          }

          val fullList = acc ++ unchecked

          def f(t: (Int, Int)) = !(fullList.contains(t._1) || fullList.contains(t._2))

          findElemOrElse[(Int, Int)](eq(unchecked.head), f, (-1, -1)) match {
            case (-1, -1) => recursiveEnrich(unchecked.tail, acc :+ unchecked.head)
            case (f, s) => recursiveEnrich((acc ++ Seq(f, s) ++ unchecked.tail), Seq.empty)
          }
        }
      }
    }

    recursiveEnrich()
  }

  // Add the multiple fractions in the form 1/n, given the n's
  def addDenominators(denominators: Seq[Int]) = {
    // Turn every item into a fraction
    denominators.map(d => Fraction(1, d))
      // Add all fractions together
      .foldLeft(Fraction(0, 1))(_ + _)
  }

  // Some 'testing'
  def t() = {
    val t = for (x <- 2 to 1000)
      yield nextSeq(x).map { case (a, b) => Fraction(1, a) + Fraction(1, b) == Fraction(1, x) }

    assert(!t.exists(v => v.contains(false)))
  }

  val l = enrich(Seq(2, 2), 1000)
  println(addDenominators(l))
  println(l.length)
}