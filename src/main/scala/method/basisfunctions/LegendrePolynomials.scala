package method.basisfunctions

import breeze.linalg.{DenseMatrix, DenseVector}
import org.apache.commons.math3.util.CombinatoricsUtils.binomialCoefficientDouble
import parameters.MethodParameters
import parameters.MethodParameters._
import spire.implicits._
import spire.math.Polynomial

class LegendrePolynomials(methodParameters: MethodParameters) extends PolynomialBasis {
  import methodParameters.basisSize

  override def phi(s: Double): DenseVector[Double] = {
    val x = 2 * (s - s0) / (S - s0) - 1
    super.phi(x)
  }

  override val matrixB: DenseMatrix[Double] = DenseMatrix.tabulate(basisSize, basisSize) { (i, j) =>
    if (i == j) 0.86 / (2 * i + 1) else 0
  }

  override def polynomials: DenseVector[Polynomial[Double]] =
    DenseVector.tabulate(basisSize) { i =>
      (0 to i)
        .foldLeft(Polynomial.zero[Double]) { case (poly, k) =>
          poly +
            Polynomial.constant(
              binomialCoefficientDouble(i, k) * binomialCoefficientDouble(k + i, k),
            ) *
            Polynomial
              .dense(Array(-1.toDouble / 2, 1.toDouble / 2))
              .pow(k)
        }
    }
}

object LegendrePolynomials extends BasisFunctions.Factory[PolynomialBasis] {
  override def make(methodParameters: MethodParameters): LegendrePolynomials =
    new LegendrePolynomials(
      methodParameters,
    )
}
