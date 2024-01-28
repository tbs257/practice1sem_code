package method

import breeze.integrate.trapezoid
import breeze.linalg.{DenseMatrix, DenseVector, inv}
import method.basisfunctions.{BasisFunctions, PolynomialBasis}
import model.{ApproximateModel, OriginalModel}
import parameters.AllParameters
import parameters.MethodParameters._

import scala.math.pow

class MethodImplementation(
    paramteres: AllParameters,
    val basisFunctions: PolynomialBasis,
    approximateModel: ApproximateModel,
    val originalModel: OriginalModel,
) {
  import paramteres.modelParameters._
  import paramteres.methodParameters._
  import paramteres.vectorR

  import approximateModel._

  import basisFunctions._

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

  def vectorA(c: DenseVector[Double]): DenseVector[Double] =
    vectorR.map(r => trapezoid(s => functionF(functionT(c, s), r), s0, S, integrationNodes))
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
