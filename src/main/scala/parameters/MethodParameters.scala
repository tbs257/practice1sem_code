package parameters

case class MethodParameters(
    basisSize: Int,
    rSamples: Int,
    methodSteps: Int,
    integrationNodes: Int,
    alpha: Double,
)

object MethodParameters {
  val s0: Double = -0.01
  val S: Double = 0.85
}
