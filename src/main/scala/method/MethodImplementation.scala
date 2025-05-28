package method

import breeze.integrate.trapezoid
import breeze.linalg.{inv, DenseMatrix, DenseVector}
import method.basisfunctions.{BasisFunctions, PolynomialBasis}
import model.{ApproximateModel, OriginalModel}
import parameters.AllParameters
import parameters.MethodParameters._

import scala.math._

class MethodImplementation(
  paramteres: AllParameters,
  val basisFunctions: PolynomialBasis,
  approximateModel: ApproximateModel,
  val originalModel: OriginalModel,
) {
  import approximateModel._
  import basisFunctions._
  import paramteres._
  import paramteres.methodParameters._
  import paramteres.modelParameters._

  def calculateStep(c: DenseVector[Double]): DenseVector[Double] =
    inv(jacobiMatrix(c).t * jacobiMatrix(c) + alpha * matrixB) *
      (jacobiMatrix(c).t * (vectorA(c) - originalModel.vectorG) +
        alpha * matrixB * c)

  private def functionT(c: DenseVector[Double], s: Double): Double =
    c.t * basisFunctions.phi(s)

  private def jacobiMatrix(c: DenseVector[Double]): DenseMatrix[Double] =
    DenseMatrix.tabulate(rSamples, basisSize) { case (i, j) =>
      trapezoid(
        f = s => {
          val t = functionT(c, s)
          val r = vectorR(i)
          if (r <= rcrit(t))
            functionF(t, r) * 4 * epsilon * (12 * pow(functionA(t, r), 11) - 6 * pow(
              functionA(t, r),
              5,
            )) *
              sigma0 / ((r - sigma(t) + sigma0) * (r - sigma(t) + sigma0)) *
              sigma0 * pow(1 - (t * chi) / (1 - chi * chi), -3 / 2) * chi / (chi * chi - 1) *
              phi(s)(j)
          else 0
        },
        start = s0,
        end = S,
        nodes = integrationNodes,
      )
    }

  def vectorA(c: DenseVector[Double], scale: Int = 1): DenseVector[Double] =
    vectorRScaled(scale).map(beta2(c, _))

  def beta3Show(c: DenseVector[Double], scale: Int = 1): DenseVector[Double] =
    vectorRScaled(scale).map(beta3(c, _))

  def g(c: DenseVector[Double], scale: Int = 1): DenseVector[Double] =
    vectorRScaled(scale).map(r => beta2(c, r) + beta3(c, r))

  private def beta3(c: DenseVector[Double], r: Double) =
    2 * Pi * beta2(c, r) *
      trapezoid(
        f = s23 =>
          trapezoid(
            f = s13 => {
              val t = functionT(c, s13) // TODO: or s23?
              trapezoid(
                f = r13 =>
                  r13 * r13 * functionF(s13, r) * (trapezoid(
                    f = q => functionF(s23, sqrt(r13 * r13 + r * r - 2 * r * r13 * q) - 1),
                    start = -1,
                    end = 1,
                    nodes = integrationNodes,
                  ) - 2),
                start = 0,
                end = rcrit(t),
                nodes = integrationNodes,
              )
            },
            start = s0,
            end = S,
            nodes = integrationNodes,
          ),
        start = s0,
        end = S,
        nodes = integrationNodes,
      )

  private def beta2(c: DenseVector[Double], r: Double) =
    trapezoid(s => functionF(functionT(c, s), r), s0, S, integrationNodes)
}

object MethodImplementation {
  def make(
    parameters: AllParameters,
    basisFunctionsFactory: BasisFunctions.Factory[PolynomialBasis],
  ) =
    new MethodImplementation(
      paramteres = parameters,
      basisFunctions = basisFunctionsFactory.make(parameters.methodParameters),
      approximateModel = new ApproximateModel(parameters.modelParameters),
      originalModel = new OriginalModel(parameters),
    )
}
