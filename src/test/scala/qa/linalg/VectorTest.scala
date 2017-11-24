package qa.linalg

import qa.Test

class VectorTest extends Test {

  "Creating a UnitVector" should "create a Vector with exactly one 1, rest are 0s" in {
    val uv = UnitVector(dimensions = 4, index = 1)

    uv shouldEqual Vector(0, 1, 0, 0)
  }

  "Adding two Vectors" should "add the entries" in {
    val v1 = Vector(0, 1, 2, 3)
    val v2 = Vector(1, 2, 3, 4)

    v1 + v2 shouldEqual Vector(1, 3, 5, 7)
  }

  "Multiplying a Vector with a Double" should "multiply all its entries" in {
    val v = Vector(0, 1, 2, 3)

    3 * v shouldEqual Vector(0, 3, 6, 9)
  }

  "Multiplying Vectors by the tensor product ⊗" should "expand the Vector" in {
    val uv0 = UnitVector(dimensions = 2, index = 0)
    val uv1 = UnitVector(dimensions = 2, index = 1)

    uv0 ⊗ uv1 shouldEqual UnitVector(dimensions = 4, index = 1)
    uv1 ⊗ uv0 shouldEqual UnitVector(dimensions = 4, index = 2)
  }

  "Vector(0.4, 0) ⊗ Vector(0, 0.5)" should "equal Vector(0, 0.2, 0, 0)" in {
    Vector(0.4, 0) ⊗ Vector(0, 0.5) shouldEqual Vector(0, 0.2, 0, 0)
  }

  // TODO: Unallowed operations (different sizes etc.)

}
