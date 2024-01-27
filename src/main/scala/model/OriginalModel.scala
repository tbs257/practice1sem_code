package model

import breeze.integrate.trapezoid
import breeze.linalg.DenseVector
import parameters.AllParameters
import parameters.ModelParameters.epsilon0

import scala.math._

class OriginalModel(parameters: AllParameters) {
  import parameters.methodParameters.integrationNodes
  import parameters.modelParameters._

  val vectorG: DenseVector[Double] = parameters.vectorR.map(r =>
    trapezoid(
      f =>
        trapezoid(
          z => trapezoid(t => functionF(t, z, f, r), 0, Pi, integrationNodes),
          0,
          Pi,
          integrationNodes,
        ),
      0,
      2 * Pi,
      integrationNodes,
    ),
  )

  private def functionG(t: Double, z: Double, f: Double): Double =
    (1 + cos(t) * cos(t) * (1 - 2 * chi)) * cos(z) * cos(z) +
      (1.0 / 2 * (1 - chi) * sin(2 * t) * sin(2 * z) +
        sin(t) * sin(t) * sin(z) * sin(z) * cos(f)) * cos(f)

  private def sigma(t: Double, z: Double, f: Double): Double =
    d / sqrt(
      1 - chi * functionG(t: Double, z: Double, f: Double) / (1 - chi * chi * cos(t) * cos(t)),
    )

  private def functionA(t: Double, z: Double, f: Double, r: Double): Double =
    d / (r - sigma(t: Double, z: Double, f: Double) + d)

  private def epsilon(t: Double): Double =
    epsilon0 / sqrt(1 - chi * chi * cos(t) * cos(t))

  private def rcrit(t: Double, z: Double, f: Double): Double =
    (pow(2, 1 / 6) - 1) * d + sigma(t: Double, z: Double, f: Double)

  private def functionU(t: Double, z: Double, f: Double, r: Double): Double =
    if (r <= rcrit(t, z, f))
      4 * epsilon(t) * (pow(functionA(t, z, f, r), 12) - pow(
        functionA(t, z, f, r),
        6,
      )) + epsilon(t)
    else 0

  private def functionF(t: Double, z: Double, f: Double, r: Double): Double =
    exp(-functionU(t, z, f, r)) * sin(z) * sin(t) / (8 * Pi)

}
