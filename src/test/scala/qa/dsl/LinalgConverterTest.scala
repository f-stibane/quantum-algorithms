package qa.dsl

import qa.Test
import qa.linalg.Vector
import qa.linalg.ApproximateVectorMatcher._
import qa.dsl.ApproximateStateMatcher._
import scala.collection.immutable.{Vector => StdVector}

class LinalgConverterTest extends Test {

  "Converting a SingularState to a Vector" should "result in a Vector with exactly one non-zero entry" in {
    val s = 0.3 * State(0, 1)
    LinalgConverter.toVector(s) should approximatelyEqualVector (Vector(0, 0.3, 0, 0))
  }

  "Converting a CompositeState to a Vector" should "result in a Vector with several non-zero entries" in {
    val s = 0.3 * State(0, 1) + 0.5 * State(1, 0)
    LinalgConverter.toVector(s) should approximatelyEqualVector (Vector(0, 0.3, 0.5, 0))
  }

//  "Converting a Vector with exactly one non-zero entry to a State" should "result in a SingularState" in {
//    val v = Vector(0, 0.3, 0, 0)
//    LinalgConverter.toState(v) should approximatelyEqualState (SingularState(0.3, StdVector(0, 1)))
//  }

}
