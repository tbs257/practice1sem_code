package model

import parameters.ModelParameters
import parameters.ModelParameters.epsilon0

import scala.math._

class ApproximateModel(modelParameters: ModelParameters) {
  import modelParameters._

  val epsilon: Double = epsilon0 / sqrt(1 - chi * chi)

  def functionF(t: Double, r: Double): Double =
    exp(-functionU(t, r)) / 0.86

  def functionA(t: Double, r: Double): Double =
    sigma0 / (r - sigma(t) + sigma0)

  def rcrit(t: Double): Double = (pow(2, 1 / 6) - 1) * d + sigma(t)

  def sigma(t: Double): Double =
    sigma0 / sqrt(1 - (t * chi) / (1 - chi * chi))

  private def functionU(t: Double, r: Double): Double =
    if (r <= rcrit(t))
      4 * epsilon * (pow(functionA(t, r), 12) - pow(functionA(t, r), 6)) + epsilon
    else 0
}
