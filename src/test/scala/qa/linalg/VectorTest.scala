package qa.linalg

import org.scalatest.{FlatSpec, Matchers}

class VectorTest extends FlatSpec with Matchers {

  "Creating a UnitVector" should "create a Vector with exactly one 1, rest are 0s" in {
    val uv = UnitVector(dimensions = 4, index = 1)

    uv shouldEqual Vector(0, 1, 0, 0)
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

}
