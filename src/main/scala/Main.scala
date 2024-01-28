import breeze.linalg._
import breeze.plot._
import cats.effect.{IO, IOApp}
import cats.syntax.all._
import method.MethodImplementation
import method.basisfunctions._
import parameters.AllParameters
import spire.implicits._
import spire.math.Polynomial

object Main extends IOApp.Simple {

  private lazy val parameterUpdateRegex = "(\\w+)\\s*=\\s*(.*)".r

  private def updateParameters(
      parameters: AllParameters,
  ): IO[AllParameters] =
    IO.readLine.flatMap { inputUntrimmed =>
      val input = inputUntrimmed.trim

      if (input.toLowerCase == "next") parameters.pure[IO]
      else {
        lazy val printErrorMessageAndRetry = IO.println(
          "Некорректная попытка обновления параметра. Попробуйте ещё раз.",
        ) >> updateParameters(parameters)
        input match {
          case parameterUpdateRegex(maybeName, maybeValue) =>
            val valueOpt = Option(maybeValue)
            lazy val intValue = valueOpt.flatMap(_.toIntOption)
            lazy val doubleValue = valueOpt.flatMap(_.toDoubleOption)
            val updatedParametersOpt = Option(maybeName).flatMap {
              case "x0" =>
                doubleValue.map(double =>
                  parameters.copy(modelParameters = parameters.modelParameters.copy(x0 = double)),
                )
              case "d" =>
                doubleValue.map(double =>
                  parameters.copy(modelParameters = parameters.modelParameters.copy(d = double)),
                )
              case "basisSize" | "m" =>
                intValue.map(int =>
                  parameters.copy(methodParameters =
                    parameters.methodParameters.copy(basisSize = int),
                  ),
                )
              case "rSamples" | "n" =>
                intValue.map(int =>
                  parameters.copy(methodParameters =
                    parameters.methodParameters.copy(rSamples = int),
                  ),
                )
              case "methodSteps" | "M" =>
                intValue.map(int =>
                  parameters.copy(methodParameters =
                    parameters.methodParameters.copy(methodSteps = int),
                  ),
                )
              case "integrationNodes" | "N" =>
                intValue.map(int =>
                  parameters.copy(methodParameters =
                    parameters.methodParameters.copy(integrationNodes = int),
                  ),
                )
              case "alpha" =>
                doubleValue.map(double =>
                  parameters.copy(methodParameters =
                    parameters.methodParameters.copy(alpha = double),
                  ),
                )
              case _ => None
            }
            updatedParametersOpt
              .map(updatedParameters =>
                IO.println("Параметры успешно обновлены. Новые значения:") >>
                  IO.println(updatedParameters) >>
                  updateParameters(updatedParameters),
              )
              .getOrElse(printErrorMessageAndRetry)
          case _ => printErrorMessageAndRetry
        }
      }
    }

  private def chooseBasisFunctions: IO[BasisFunctions.Factory[PolynomialBasis]] =
    IO.readLine.map(_.trim.toLowerCase).flatMap {
      case "legendre" => LegendrePolynomials.pure[IO]
      case "normal"   => Powers.pure[IO]
      case _ =>
        IO.println("Введённая опция не распознана. Попробуйте ещё раз.") >> chooseBasisFunctions
    }

  override def run: IO[Unit] = {
    val defaultParameters = AllParameters.defaultParameters

    for {
      _ <- IO.println("Параметры по умолчанию:")
      _ <- IO.println(defaultParameters)
      _ <- IO.println(
        "Измените желаемые параметры, набрав, например, 'm = 17', или наберите 'NEXT' для перехода " +
          "к следующему шагу настройки",
      )
      updatedParameters <- updateParameters(defaultParameters)
      _ <- IO.println(
        "Выберите набор базисных функций: 'legendre' для многочленов Лежандра, " +
          "'normal' для обычных многочленов.",
      )
      basisFunctions <- chooseBasisFunctions
      methodImplementation <- IO.delay(MethodImplementation.make(updatedParameters, basisFunctions))
      c0 = DenseVector.ones[Double](updatedParameters.methodParameters.basisSize)
      c <- (0 to updatedParameters.methodParameters.methodSteps).foldLeft(c0.pure[IO]) {
        case (cIO, stepNumber) =>
          cIO.flatMap(c =>
            IO.println(
              s"На шаге $stepNumber значение многочлена t(s) = ${
                val polynomial = c.toArray
                  .zip(methodImplementation.basisFunctions.polynomials.toArray)
                  .foldLeft(Polynomial.zero[Double]) { case (resultPoly, (coef, basisPoly)) =>
                    resultPoly + Polynomial.constant(coef) * basisPoly
                  }
                val polynomialString = polynomial.toString()
                polynomialString.slice(1, polynomialString.length - 1).replace('x', 's')
              }",
            ) >> IO.delay(c - methodImplementation.calculateStep(c)),
          )
      }
      _ <- IO.delay {
        val f = Figure()
        val p = f.subplot(0)
        p += plot(
          x = updatedParameters.vectorR,
          y = methodImplementation.originalModel.vectorG,
          style = '.',
          name = "beta_2",
        )
        p += plot(
          x = updatedParameters.vectorR,
          y = methodImplementation.vectorA(c),
          name = "beta_2 approximation",
        )

      }
    } yield ()
  }
}
