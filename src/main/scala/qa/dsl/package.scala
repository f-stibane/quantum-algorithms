package qa

package object dsl {

  implicit class DoubleDsl(d: Double) {
    def *(s: State): State = s * d
  }

}
