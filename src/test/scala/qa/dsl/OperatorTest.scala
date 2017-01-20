package qa.dsl

import qa.Test
import qa.dsl.ApproximateStateMatcher.approximatelyEqual

class OperatorTest extends Test {

  import Operator._

  "Applying the Identity Operator on a 1-qubit State" should "give the same State" in {
    val s = 0.3 * State(0)
    I(s) should approximatelyEqual (s)
  }

  "Applying the Identity Operator on a 2-qubit State" should "give the same State" in {
    val s = 0.3 * State(0, 1)
    I(s) should approximatelyEqual (s)
  }

  // TODO: Property-based test - Identity Operator

  "Applying the Hadamard Operator" should "give a SuperposedState" in {
    val s = State(0)
    H(s) should approximatelyEqual (1 / math.sqrt(2) * (State(0) + State(1)))
  }

  "Applying the Hadamard Operator on a (special) SuperposedState" should "give a SingularState" in {
    val s = 1 / math.sqrt(2) * (State(0) + State(1))
    H(s) should approximatelyEqual (State(0))
  }

  // TODO: Property-based test - Hadamard Operator

  "Applying the X Operator on a 1-qubit State" should "give the 'opposite' State" in {
    val s = 0.3 * State(0)
    X(s) should approximatelyEqual (0.3 * State(1))
  }

  "Applying the X Operator on a 2-qubit State" should "give the 'opposite' State" in {
    val s = 0.3 * State(0, 1)
    X(s) should approximatelyEqual (0.3 * State(1, 0))
  }

  // TODO: Property-based test - X Operator

  "Applying the Z Operator on |0>" should "act like the I Operator" in {
    val s = 0.3 * State(0)
    Z(s) should approximatelyEqual (s)
  }

  "Applying the Z Operator on |1>" should "swap the sign" in {
    val s = 0.3 * State(1)
    Z(s) should approximatelyEqual (-0.3 * State(1))
  }

  "Applying the Z Operator on a |0, 1>" should "swap the sign" in {
    val s = 0.3 * State(0, 1)
    Z(s) should approximatelyEqual (-0.3 * State(0, 1))
  }

  // TODO: Property-based test - Z Operator

}
