package method.basisfunctions

import breeze.integrate.trapezoid
import breeze.linalg.{DenseMatrix, DenseVector}
import parameters.MethodParameters
import parameters.MethodParameters._

import math.pow

class Powers(methodParameters: MethodParameters) extends BasisFunctions {
  import methodParameters.basisSize

  override def phi(s: Double): DenseVector[Double] = DenseVector.tabulate(basisSize)(pow(s, _))

  override val matrixB: DenseMatrix[Double] = DenseMatrix.tabulate(basisSize, basisSize) {
    case (i, j) =>
      trapezoid(s => phi(s)(i) * phi(s)(j), s0, S, methodParameters.integrationNodes)
  }
}

object Powers extends BasisFunctions.Factory {
  override def make(methodParameters: MethodParameters): BasisFunctions = new Powers(
    methodParameters,
  )
}
