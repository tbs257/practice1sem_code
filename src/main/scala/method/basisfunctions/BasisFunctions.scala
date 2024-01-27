package method.basisfunctions

import breeze.linalg.{DenseMatrix, DenseVector}
import parameters.MethodParameters

trait BasisFunctions {
  
  def phi(s: Double): DenseVector[Double]
  
  def matrixB: DenseMatrix[Double]

}

object BasisFunctions {
  trait Factory {
    def make(methodParameters: MethodParameters): BasisFunctions
  }
}
