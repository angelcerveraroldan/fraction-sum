package com.angelcerveraroldan.fractionsum
import scala.language.postfixOps

case class Fraction (enumerator: Long, denominator: Long) {
    // Simplify fraction by dividing top and bottom by gcd
    def simplify = {
      // Gcd method
      def gcd(a: Long, b: Long): Long = if (b == 0) {
        a
      } else {
        gcd(b, a % b)
      }

      val fractionsGcd = gcd(enumerator, denominator)

      Fraction(enumerator / fractionsGcd, denominator / fractionsGcd)
    }

    // Add two fractions together
    def +(f: Fraction): Fraction =
      Fraction(
        (enumerator * f.denominator) + (f.enumerator * denominator),
        denominator * f.denominator
      ) simplify
}
