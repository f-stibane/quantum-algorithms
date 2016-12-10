package qa

object ApproximateDoubleMatcher {
  val precision = 1e-6

  def approximatelyEquals(l: Double, r: Double): Boolean =
    (l - r).abs < precision
}

class ApproximateDoubleMatcherTest extends Test {
  import ApproximateDoubleMatcher._

  "A Double" should "approximately equal another slightly larger double" in {
    val something = 42
    val somethingSlightlyLarger = something + precision / 2

    approximatelyEquals(something, somethingSlightlyLarger) shouldBe true
  }

}