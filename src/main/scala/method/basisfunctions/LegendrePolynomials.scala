package method.basisfunctions

import breeze.linalg.{DenseMatrix, DenseVector}
import org.apache.commons.math3.util.CombinatoricsUtils.binomialCoefficient
import parameters.MethodParameters
import parameters.MethodParameters._

import scala.math.pow

class LegendrePolynomials(methodParameters: MethodParameters) extends BasisFunctions {
  import methodParameters.basisSize

  override def phi(s: Double): DenseVector[Double] = {
    val x = 2 * (s - s0) / (S - s0) - 1

    DenseVector.tabulate(basisSize) { i =>
      (0 to i)
        .map(k => binomialCoefficient(i, k) * binomialCoefficient(k + i, k) * pow((x - 1) / 2, k))
        .sum
    }
  }

  override val matrixB: DenseMatrix[Double] = DenseMatrix.tabulate(basisSize, basisSize) { (i, j) =>
    if (i == j) 0.86 / (2 * i + 1) else 0
  }
}

object LegendrePolynomials extends BasisFunctions.Factory {
  override def make(methodParameters: MethodParameters): BasisFunctions = new LegendrePolynomials(
    methodParameters,
  )
}
