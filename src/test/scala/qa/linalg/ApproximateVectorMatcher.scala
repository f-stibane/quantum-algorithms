package qa.linalg

import org.scalatest.matchers.{MatchResult, Matcher}
import qa.{ApproximateDoubleMatcher, Test}

object ApproximateVectorMatcher {

  class VectorApproximatelyEquals(right: Vector) extends Matcher[Vector] {
    override def apply(left: Vector): MatchResult = {
      val equals = left.entries.size == right.entries.size &&
        left.entries.indices.forall(
          i => ApproximateDoubleMatcher.approximatelyEquals(left.entries(i), right.entries(i)))

      MatchResult(
        equals,
        s"$left did not (approximately) equal $right",
        s"$left did (approximately) equal $right"
      )
    }
  }
  def approximatelyEqualVector(right: Vector): VectorApproximatelyEquals = new VectorApproximatelyEquals(right)

}

class ApproximateVectorMatcherTest extends Test {
  import ApproximateVectorMatcher._

  "Two Vectors with approximately equal entries" should "be approximately equal" in {
    val something = Vector(0.3, 0.6)
    val somethingSlightlyDifferent = Vector(
      0.3 - ApproximateDoubleMatcher.precision / 2,
      0.6 + ApproximateDoubleMatcher.precision / 2
    )

    something should approximatelyEqualVector (somethingSlightlyDifferent)
  }

}