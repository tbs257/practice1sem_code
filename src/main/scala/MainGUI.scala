import breeze.linalg._
import breeze.plot._
import method._
import method.basisfunctions._
import parameters._
import spire.implicits._
import spire.math.Polynomial

import java.io.File
import javax.swing.table.DefaultTableModel
import scala.swing.GridBagPanel.Anchor
import scala.swing._

object MainGUI extends SimpleSwingApplication {
  override def top: Frame = new MainFrame {
    val textWidth = 8

    val defaultParameters = AllParameters.defaultParameters

    val x0Field               = new TextField { columns = textWidth }
    x0Field.text = defaultParameters.modelParameters.x0.toString
    val dField                = new TextField { columns = textWidth }
    dField.text = defaultParameters.modelParameters.d.toString
    val basisSizeField        = new TextField { columns = textWidth }
    basisSizeField.text = defaultParameters.methodParameters.basisSize.toString
    val rSamplesField         = new TextField { columns = textWidth }
    rSamplesField.text = defaultParameters.methodParameters.rSamples.toString
    val methodStepsField      = new TextField { columns = textWidth }
    methodStepsField.text = defaultParameters.methodParameters.methodSteps.toString
    val integrationNodesField = new TextField { columns = textWidth }
    integrationNodesField.text = defaultParameters.methodParameters.integrationNodes.toString
    val alphaField            = new TextField { columns = textWidth }
    alphaField.text = defaultParameters.methodParameters.alpha.toString
    val legendre              = new RadioButton("legendre")
    val normal                = new RadioButton("normal")
    normal.selected = true
    val _                     = new ButtonGroup(legendre, normal)

    val plotSize = new Dimension(300, 300)

    val plot1 = new Plot
    plot1.legend = true

    val plot1Panel = new Panel {
      override lazy val peer = plot1.panel
      preferredSize = plotSize
    }

    val plot2 = new Plot
    plot2.legend = true

    val plot2Panel = new Panel {
      override lazy val peer = plot2.panel
      preferredSize = plotSize
    }

    val plot3 = new Plot
    plot3.legend = true

    val plot3Panel = new Panel {
      override lazy val peer = plot3.panel
      preferredSize = plotSize
    }

    val logTable = new DefaultTableModel(Array[AnyRef]("Evaluation log"), 0)

    contents = new GridBagPanel {

      def constraints(
        x: Int,
        y: Int,
        gridwidth: Int = 1,
        gridheight: Int = 1,
        weightx: Double = 0.0,
        weighty: Double = 0.0,
        anchor: GridBagPanel.Anchor.Value = GridBagPanel.Anchor.West,
        fill: GridBagPanel.Fill.Value = GridBagPanel.Fill.None,
        insets: Insets = new Insets(2, 2, 2, 2),
      ): Constraints = {
        val c = new Constraints
        c.gridx = x
        c.gridy = y
        c.gridwidth = gridwidth
        c.gridheight = gridheight
        c.weightx = weightx
        c.weighty = weighty
        c.anchor = anchor
        c.fill = fill
        c.insets = insets
        c
      }

      add(
        new Label(text0 = "x0 (particle shape)", icon0 = Swing.EmptyIcon, align = Alignment.Left),
        constraints(0, 0),
      )

      add(
        x0Field,
        constraints(1, 0),
      )

      add(
        new Label(text0 = "d (particle diameter)", icon0 = Swing.EmptyIcon, align = Alignment.Left),
        constraints(0, 1),
      )

      add(
        dField,
        constraints(1, 1),
      )

      add(
        new Label(text0 = "m (polynomial degree)", icon0 = Swing.EmptyIcon, align = Alignment.Left),
        constraints(0, 2),
      )

      add(
        basisSizeField,
        constraints(1, 2),
      )

      add(
        new Label(text0 = "n (sampling rate for r value)", icon0 = Swing.EmptyIcon, align = Alignment.Left),
        constraints(0, 3),
      )

      add(
        rSamplesField,
        constraints(1, 3),
      )

      add(
        new Label(text0 = "M (steps of method)", icon0 = Swing.EmptyIcon, align = Alignment.Left),
        constraints(0, 4),
      )

      add(
        methodStepsField,
        constraints(1, 4),
      )

      add(
        new Label(
          text0 = "N (nodes count for numerical integration)",
          icon0 = Swing.EmptyIcon,
          align = Alignment.Left,
        ),
        constraints(0, 5),
      )

      add(
        integrationNodesField,
        constraints(1, 5),
      )

      add(
        new Label(text0 = "alpha (regularization parameter)", icon0 = Swing.EmptyIcon, align = Alignment.Left),
        constraints(0, 6),
      )

      add(
        alphaField,
        constraints(1, 6),
      )

      add(
        new Label(text0 = "basis functions", icon0 = Swing.EmptyIcon, align = Alignment.Left),
        constraints(0, 7),
      )

      add(
        new BoxPanel(Orientation.Vertical) {
          contents += legendre
          contents += normal
        },
        constraints(1, 7),
      )

      add(
        Button("go")(go()),
        constraints(0, 8),
      )

      add(
        plot1Panel,
        constraints(
          x = 2,
          y = 0,
          gridheight = 9,
          anchor = Anchor.East,
        ),
      )

      add(
        plot2Panel,
        constraints(
          x = 3,
          y = 0,
          gridheight = 9,
          anchor = Anchor.East,
        ),
      )

      add(
        plot3Panel,
        constraints(
          x = 4,
          y = 0,
          gridheight = 9,
          anchor = Anchor.East,
        ),
      )

      add(
        new ScrollPane(new Table(logTable)) {
          preferredSize = new Dimension(1000, 200)
        },
        constraints(
          x = 0,
          y = 9,
          gridwidth = 3,
        ),
      )

      border = Swing.EmptyBorder(10, 10, 10, 10)

    }

    def go(): Unit = {
      val x0 = x0Field.text.toDouble

      val d                = dField.text.toDouble
      val basisSize        = basisSizeField.text.toInt
      val rSamples         = rSamplesField.text.toInt
      val methodSteps      = methodStepsField.text.toInt
      val integrationNodes = integrationNodesField.text.toInt
      val alpha            = alphaField.text.toDouble

      val parameters = AllParameters(
        modelParameters = ModelParameters(
          x0 = x0,
          d = d,
        ),
        methodParameters = MethodParameters(
          basisSize = basisSize,
          rSamples = rSamples,
          methodSteps = methodSteps,
          integrationNodes = integrationNodes,
          alpha = alpha,
        ),
      )

      val basisFunctions = if (legendre.selected) LegendrePolynomials else Powers

      val methodImplementation = MethodImplementation.make(parameters, basisFunctions)

      val c0 = DenseVector.ones[Double](parameters.methodParameters.basisSize)

      val cFinal = (0 to parameters.methodParameters.methodSteps).foldLeft(c0) { case (c, stepNumber) =>
        val polynomial =
          c
            .toArray
            .zip(methodImplementation.basisFunctions.polynomials.toArray)
            .foldLeft(Polynomial.zero[Double]) { case (resultPoly, (coef, basisPoly)) =>
              resultPoly + Polynomial.constant(coef) * basisPoly
            }

        val polynomialString =
          polynomial
            .map(BigDecimal(_).setScale(4, BigDecimal.RoundingMode.HALF_UP))
            .toString

        report(
          s"На шаге $stepNumber значение многочлена t(s) = " +
            polynomialString.slice(1, polynomialString.length - 1).replace('x', 's'),
        )

        c - methodImplementation.calculateStep(c)
      }

      val orig   = methodImplementation.originalModel.vectorG
      val approx = methodImplementation.vectorA(cFinal)

      val scale        = 10 // TODO: setting in ui.
      val approxScaled = methodImplementation.vectorA(cFinal, scale)

      val beta3scaled = methodImplementation.beta3Show(cFinal, scale)
      val gScaled = methodImplementation.g(cFinal, scale)

      plot1 += plot(
        x = parameters.vectorR,
        y = orig,
        style = '.',
        name = "beta_2",
      )

      plot1 += plot(
        x = parameters.vectorRScaled(scale),
        y = approxScaled,
        name = "beta_2 approximation",
      )

      plot2 += plot(
        x = parameters.vectorRScaled(scale),
        y = beta3scaled,
        name = "beta_3 approximation",
      )

      plot3 += plot(
        x = parameters.vectorRScaled(scale),
        y = gScaled,
        name = "g approximation",
      )

      val averageRelativeError = euclideanDistance(orig, approx) / norm(orig)

      report(
        s"Average relative error = " +
          BigDecimal(averageRelativeError * 100).setScale(2, BigDecimal.RoundingMode.HALF_UP).toString +
          "%",
      )

      csvwrite(new File("beta_2.csv"), DenseVector.horzcat(orig, approx))

      report(
        "Output file with beta_2 original and approximate values created",
      )
    }

    def report(msg: String): Unit = {
      println(msg)

      logTable.addRow(Array[AnyRef](msg))
    }

  }
}
