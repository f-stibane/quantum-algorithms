package qa

package object linalg {

  implicit class MultiplyVectorByDouble(d: Double) {
    def *(v: Vector): Vector = v * d
  }

}
