package qa.dsl

import qa.Test
import qa.dsl.ApproximateStateMatcher.approximatelyEqual

class ControlledOperatorTest extends Test {
  import ControlledOperator._

  "A CNot" should "swap the controlled Qubit if the one controlling Qubit is |1>" in {
    val s = State(1, 0)
    CNot(0)(1)(s) should approximatelyEqual (State(1, 1))
  }

  "A CNot" should "not swap the controlled Qubit if the one controlling Qubit is |0>" in {
    val s = State(0, 0)
    CNot(0)(1)(s) should approximatelyEqual (State(0, 0))
  }

  "A CNot" should "enable multiple controlling Qubits" in {
    val s0 = State(0, 0, 0)
    CNot(0, 1)(2)(s0) should approximatelyEqual (State(0, 0, 0))

    val s1 = State(1, 0, 0)
    CNot(0, 1)(2)(s1) should approximatelyEqual (State(1, 0, 0))

    val s2 = State(1, 1, 0)
    CNot(0, 1)(2)(s2) should approximatelyEqual (State(1, 1, 1))
  }

  "A CNot" should "enable 'non-interactive' Qubits" in {
    val s0 = State(0, 0, 0)
    CNot(0)(2)(s0) should approximatelyEqual (State(0, 0, 0))

    val s1 = State(1, 0, 0)
    CNot(0)(2)(s1) should approximatelyEqual (State(1, 0, 1))

    val s2 = State(0, 1, 0)
    CNot(0)(2)(s2) should approximatelyEqual (State(0, 1, 0))

    val s3 = State(1, 1, 0)
    CNot(0)(2)(s3) should approximatelyEqual (State(1, 1, 1))
  }

  "A CNot" should "enable controlling Qubits in later order" in {
    val s0 = State(0, 0, 0)
    CNot(2)(0)(s0) should approximatelyEqual (State(0, 0, 0))

    val s1 = State(0, 0, 1)
    CNot(2)(0)(s1) should approximatelyEqual (State(1, 0, 1))
  }

}
