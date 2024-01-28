package method.basisfunctions

import breeze.linalg.{DenseMatrix, DenseVector}
import parameters.MethodParameters
import spire.math.Polynomial

trait BasisFunctions {

  def phi(s: Double): DenseVector[Double]

  def matrixB: DenseMatrix[Double]

}

object BasisFunctions {
  trait Factory[BF <: BasisFunctions] {
    def make(methodParameters: MethodParameters): BF
  }
}
