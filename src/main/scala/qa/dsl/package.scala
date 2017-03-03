package qa

package object dsl {

  implicit class DoubleDsl(val d: Double) extends AnyVal {
    def *(s: State): State = s * d
  }

}
