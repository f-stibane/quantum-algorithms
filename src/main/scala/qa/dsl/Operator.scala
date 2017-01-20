package qa.dsl

import qa.linalg.Matrix
import scala.collection.immutable.{Vector => StdVector}

trait Operator {
  def apply(s: State): State = {
    val matrices = for {
      _ <- 0 until s.numberOfQubits
    } yield matrix

    val tensored = matrices.reduce(_ ⊗ _)

    val applied = tensored * LinalgConverter.toVector(s)
    LinalgConverter.toState(applied)
  }
  protected def matrix: Matrix
}

object Operator {
  object I extends Operator {
    val matrix = Matrix(2, 2, StdVector(
      1, 0,
      0, 1
    ))
  }

  object H extends Operator {
    val matrix = Matrix(2, 2, StdVector(
      1,  1,
      1, -1
    ).map(_ * 1 / math.sqrt(2)))
  }

  object X extends Operator {
    val matrix = Matrix(2, 2, StdVector(
      0, 1,
      1, 0
    ))
  }

  object Z extends Operator {
    val matrix = Matrix(2, 2, StdVector(
      1,  0,
      0, -1
    ))
  }
}
