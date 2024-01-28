package method.basisfunctions

import breeze.integrate.trapezoid
import breeze.linalg.{DenseMatrix, DenseVector}
import parameters.MethodParameters
import parameters.MethodParameters._
import spire.implicits._
import spire.math.Polynomial

class Powers(methodParameters: MethodParameters) extends PolynomialBasis {
  import methodParameters.basisSize

  override val matrixB: DenseMatrix[Double] = DenseMatrix.tabulate(basisSize, basisSize) {
    case (i, j) =>
      trapezoid(s => phi(s)(i) * phi(s)(j), s0, S, methodParameters.integrationNodes)
  }

  override def polynomials: DenseVector[Polynomial[Double]] =
    DenseVector.tabulate(basisSize)(Polynomial(1, _))
}

object Powers extends BasisFunctions.Factory[PolynomialBasis] {  
  override def make(methodParameters: MethodParameters): Powers = new Powers(
    methodParameters,
  )
}
