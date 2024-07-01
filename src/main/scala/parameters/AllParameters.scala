package parameters

import breeze.linalg.DenseVector
import cats.Show

case class AllParameters(modelParameters: ModelParameters, methodParameters: MethodParameters) {
  import methodParameters.rSamples
  import modelParameters.d
  val vectorR: DenseVector[Double] =
    DenseVector.tabulate(rSamples)(i => 0.9 * d + 1.2 * d * i / rSamples)
}

object AllParameters {
  val defaultParameters: AllParameters = AllParameters(
    ModelParameters(
      x0 = 1.7,
      d = 2,
    ),
    MethodParameters(
      basisSize = 7,
      rSamples = 15,
      methodSteps = 25,
      integrationNodes = 20,
      alpha = 0.05,
    ),
  )

  implicit val show: Show[AllParameters] =
    p => s"""x0 = ${p.modelParameters.x0}
            |d = ${p.modelParameters.d}
            |basisSize (m) = ${p.methodParameters.basisSize}
            |rSamples (n) = ${p.methodParameters.rSamples}
            |methodSteps (M) = ${p.methodParameters.methodSteps}
            |integrationNodes (N) = ${p.methodParameters.integrationNodes}
            |alpha = ${p.methodParameters.alpha}""".stripMargin
}
