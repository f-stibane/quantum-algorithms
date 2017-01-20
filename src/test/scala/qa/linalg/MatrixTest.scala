package qa.linalg

import qa.Test
import scala.collection.immutable.{Vector => StdVector}

class MatrixTest extends Test {

  "Multiplying a Vector by a Matrix" should "give a new Vector" in {
    val m = Matrix(2, 2, StdVector(
      1, 2,
      3, 4
    ))

    val v = Vector(5, 6)

    (m * v) shouldEqual Vector(17, 39)
  }

  "Multiplying a Matrix with tensor product ⊗ by another Matrix" should "give an 'inflated' Matrix" in {
    val m1 = Matrix(2, 3, StdVector(
      1, 2, 3,
      4, 5, 6
    ))

    val m2 = Matrix(3, 2, StdVector(
      1, 2,
      3, 4,
      5, 6
    ))

    val inflated = Matrix(6, 6, StdVector(
       1,  2,  3,  2,  4,  6,
       4,  5,  6,  8, 10, 12,
       3,  6,  9,  4,  8, 12,
      12, 15, 18, 16, 20, 24,
       5, 10, 15,  6, 12, 18,
      20, 25, 30, 24, 30, 36
    ))

    (m1 ⊗ m2) shouldEqual inflated
  }

  // TODO: Validations

}
