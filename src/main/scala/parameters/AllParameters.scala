package parameters

import breeze.linalg.DenseVector
import cats.Show

case class AllParameters(modelParameters: ModelParameters, methodParameters: MethodParameters) {
  import modelParameters.d
  import methodParameters.rSamples
  val vectorR: DenseVector[Double] =
    DenseVector.tabulate(rSamples)(i => 0.9 * d + 1.2 * d * i / rSamples)
}

object AllParameters {
  val defaultParameters = AllParameters(
    ModelParameters(x0 = 1.7, d = 2),
    MethodParameters(
      basisSize = 7,
      rSamples = 15,
      methodSteps = 25,
      integrationNodes = 20,
      alpha = 0.05,
    ),
  )

  implicit val show: Show[AllParameters] = (t: AllParameters) =>
    s"""x0 = ${t.modelParameters.x0}
       |d = ${t.modelParameters.d}
       |basisSize (m) = ${t.methodParameters.basisSize}
       |rSamples (n) = ${t.methodParameters.rSamples}
       |methodSteps (M) = ${t.methodParameters.methodSteps}
       |integrationNodes (N) = ${t.methodParameters.integrationNodes}
       |alpha = ${t.methodParameters.alpha}""".stripMargin
}
