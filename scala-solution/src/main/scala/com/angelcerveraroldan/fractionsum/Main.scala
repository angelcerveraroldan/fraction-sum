package com.angelcerveraroldan.fractionsum

import com.angelcerveraroldan.fractionsum.PrecalculateData.DenominatorPair

import scala.annotation.tailrec
import scala.util.Random

/*
List(256, 462, 840, 750, 100, 60, 420, 162, 108, 342, 468, 210, 660, 96, 190, 816, 192, 42, 448, 486, 480, 55, 432, 297, 576, 612, 133, 88, 84, 552, 440, 19, 264, 546, 9, 864, 900, 252, 896, 135, 130, 14, 728, 495, 306, 544, 650, 120, 300, 540, 72, 924, 144, 500, 240, 756, 220, 260, 441, 624, 288, 49, 36, 992, 910, 918, 182, 330, 450, 506, 180, 150, 50, 600, 768, 588, 648, 882, 90, 800, 384, 250, 912, 378, 25, 528, 594, 405, 189, 132, 195, 1000, 270, 880, 702, 294, 224, 630, 960, 396, 780, 168, 45, 216, 336, 504, 570, 156, 792, 812, 640, 616, 798, 110, 324, 684, 720, 320, 54, 280, 930, 99, 810, 770, 56, 208, 312, 520, 272, 936, 392, 784, 165, 200, 990, 360, 560, 672, 126, 13, 972, 364, 304, 870, 760, 456, 380, 128, 112)
 */

object Main extends App {
  def enrich(
      denominators: List[Int],
      mappedEquivalence: Map[Int, List[DenominatorPair]]
  ) = {
    @tailrec
    def recursiveEnrich(
        unchecked: List[Int] = denominators,
        acc: List[Int] = List.empty,
        eq: Map[Int, List[DenominatorPair]] = mappedEquivalence
    ): List[Int] = unchecked match {
      case Nil =>
        acc
      case _ if !eq.contains(unchecked.head) =>
        recursiveEnrich(unchecked.tail, acc :+ unchecked.head)
      case _ =>
        val fullList = acc ++ unchecked

        def listDoesntContainEither(t: DenominatorPair) =
          !(fullList.contains(t._1) || fullList.contains(t._2))

        eq(unchecked.head).find(listDoesntContainEither) match {
          case Some((a, b)) =>
            recursiveEnrich(
              Random.shuffle(acc ++ List(a, b) ++ unchecked.tail),
              List.empty
            )
          case None =>
            recursiveEnrich(unchecked.tail, acc :+ unchecked.head)
        }
    }

    recursiveEnrich()
  }
}
