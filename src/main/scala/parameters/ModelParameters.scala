package parameters

import scala.math.sqrt

case class ModelParameters(x0: Double, d: Double) {
  val chi: Double = (x0 * x0 - 1) / (x0 * x0 + 1)
  val sigma0: Double = sqrt(2) * d
}

object ModelParameters {
  val epsilon0: Int = 1
}
