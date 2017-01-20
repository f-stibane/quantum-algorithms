package qa.dsl

import qa.Test
import qa.dsl.ApproximateStateMatcher.approximatelyEqual
import scala.collection.immutable.{Vector => StdVector}

class StateTest extends Test {

  "The simplest SingularState" should "be constructed by multiplying it with a Double" in {
    val simplestState = 0.5 * State(0)
    simplestState should approximatelyEqual (SingularState(0.5, StdVector(0)))
  }

  "Adding a SingularState with the same Qubits" should "add coefficients" in {
    val s1 = 0.1 * State(0)
    val s2 = 0.2 * State(0)
    s1 + s2 should approximatelyEqual (0.3 * State(0))
  }

  "Adding a State with another Qubit" should "build a CompositeState" in {
    val s1 = State(0)
    val s2 = State(1)

    s1 + s2 should approximatelyEqual (SuperposedState(s1, s2))
  }

  "Adding States with different numbers of Qubits" should "not work" in {
    intercept[IllegalArgumentException] {
      State(0, 1, 1, 0) + State(1, 0)
    }
  }

  "A State's Qubit" must "be either one or Zero" in {
    intercept[IllegalArgumentException] {
      State(-1)
    }
    intercept[IllegalArgumentException] {
      State(2)
    }
    intercept[IllegalArgumentException] {
      State(5)
    }
  }

  "Tensoring two SingularStates" should "produce a new SingularState with more Qubits" in {
    SingularState(1, StdVector(0)) ⊗ SingularState(1, StdVector(1)) should approximatelyEqual(SingularState(1, StdVector(0, 1)))
    // TODO: Check coefficients
  }

  // TODO: Unallowed operations (different sizes etc.)

}
