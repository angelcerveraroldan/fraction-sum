package com.angelcerveraroldan.fractionsum
import scala.annotation.tailrec
import scala.language.postfixOps

case class Fraction(enumerator: BigInt, denominator: BigInt) {
  // Simplify fraction by dividing top and bottom by gcd
  def simplify: Fraction = {
    // Gcd method
    @tailrec
    def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) {
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
