package method.basisfunctions

import breeze.linalg.DenseVector
import spire.math.Polynomial
import spire.implicits._

trait PolynomialBasis extends BasisFunctions {
  override def phi(s: Double): DenseVector[Double] = polynomials.map(_.apply(s))
  
  def polynomials: DenseVector[Polynomial[Double]]
}
