package observatory

import com.sksamuel.scrimage.Pixel
import observatory.Visualization.greatestCircleDistance
import org.scalacheck.Gen

/**
  * @author sali
  */
object TestUtils {

  val colorScales: List[(Double, Color)] = List(
    (-60, Color(0, 0, 0)),
    (-50, Color(33, 0, 107)),
    (-27, Color(255, 0, 255)),
    (-15, Color(0, 0, 255)),
    (0, Color(0, 255, 255)),
    (12, Color(255, 255, 0)),
    (32, Color(255, 0, 0)),
    (60, Color(255, 255, 255))
  )

  val colorScales2: List[(Double, Color)] = List(
    (-7, Color(0, 0, 255)),
    (-2, Color(0, 255, 255)),
    (0, Color(255, 255, 255)),
    (4, Color(255, 0, 0)),
    (7, Color(0, 0, 0))
  )

  def roundDouble(input: Double)(scale: Int): Double =
    BigDecimal(input).setScale(scale, BigDecimal.RoundingMode.HALF_UP).toDouble

  def roundDouble1(input: Double): Double = roundDouble(input)(1)

  def roundDouble4(input: Double): Double = roundDouble(input)(4)

  def colorDifference(color1: Color, color2: Color): Double =
    euclideanDistance(
      Array(color1.red.toDouble, color1.green.toDouble, color1.blue.toDouble),
      Array(color2.red.toDouble, color2.green.toDouble, color2.blue.toDouble)
    )

  def temperatureDifference(temperature1: Double, temperature2: Double): Double =
    euclideanDistance(Array(temperature1), Array(temperature2))

  def getColor: Color = Color(randomInt(0, 255), randomInt(0, 255), randomInt(0, 255))

  def getTemperature: Double = Gen.choose(-100, 100).sample.get

  def getLocation: Location = Location(randomDouble(-90, 90, 3), randomDouble(-180, 180, 3))

  def validateVisualize(locations: (Location, Location), colors: (Color, Color), location: Location,
                        pixel: Pixel): Unit = {
    val color = Color(pixel.red, pixel.green, pixel.blue)
    val distanceToColor1 = colorDifference(colors._1, color)
    val distanceToColor2 = colorDifference(colors._2, color)

    val distanceToLocation1 = greatestCircleDistance(locations._1, location)
    val distanceToLocation2 = greatestCircleDistance(locations._2, location)

    if (distanceToLocation1 < distanceToLocation2) {
      val label =
        s"""Incorrect computed color at $location:$color.
           |Expected to be closer to ${colors._1} than ${colors._2}.
           |Distance to color1: $distanceToColor1
           |Distance to color2: $distanceToColor2
           |Distance to Location1: $distanceToLocation1
           |Distance to Location2: $distanceToLocation2
           """.stripMargin
      assert(distanceToColor1 < distanceToColor2, label)
    } else {
      val label =
        s"""Incorrect computed color at $location:$color.
           |Expected to be closer to ${colors._2} than ${colors._1}.
           |Distance to color1: $distanceToColor1
           |Distance to color2: $distanceToColor2
           |Distance to Location1: $distanceToLocation1
           |Distance to Location2: $distanceToLocation2
           """.stripMargin
      assert(distanceToColor2 <= distanceToColor1, label)
    }
  }

  def validateVisualizeGrid(locations: (Location, Location), colors: (Color, Color), location: Location,
                        pixel: Pixel, grid: (Int, Int) => Double): Unit = {
    val color = Color(pixel.red, pixel.green, pixel.blue)
    val distanceToColor1 = colorDifference(colors._1, color)
    val distanceToColor2 = colorDifference(colors._2, color)

    val distanceToLocation1 = greatestCircleDistance(locations._1, location)
    val distanceToLocation2 = greatestCircleDistance(locations._2, location)
  }

  def validatePredictTemperature(locations: (Location, Location), temperatures: (Double, Double), location: Location,
                                 predictedTemperature: Double): Unit = {
    val distanceToLocation1 = greatestCircleDistance(locations._1, location)
    val distanceToLocation2 = greatestCircleDistance(locations._2, location)

    val distanceToTemperature1 = math.abs(temperatures._1 - predictedTemperature)
    val distanceToTemperature2 = math.abs(temperatures._2 - predictedTemperature)

    if (distanceToLocation1 < distanceToLocation2) {
      val label =
        s"""Incorrect computed temperature at $location:$predictedTemperature.
           |Expected to be closer to ${temperatures._1} than ${temperatures._2}.
           |Distance to temperature1: $distanceToTemperature1
           |Distance to temperature2: $distanceToTemperature2
           |Distance to Location1: $distanceToLocation1
           |Distance to Location2: $distanceToLocation2
         """.stripMargin
      assert(distanceToTemperature1 < distanceToTemperature2, label)
    } else {
      val label =
        s"""Incorrect computed temperature at $location:$predictedTemperature.
           |Expected to be closer to ${temperatures._2} than ${temperatures._1}.
           |Distance to temperature1: $distanceToTemperature1
           |Distance to temperature2: $distanceToTemperature2
           |Distance to Location1: $distanceToLocation1
           |Distance to Location2: $distanceToLocation2
         """.stripMargin
      assert(distanceToTemperature2 <= distanceToTemperature1, label)
    }
  }

  def validateMakeGrid(locations: (Location, Location), temperatures: (Double, Double), location: Location,
                       predictedTemperature: Double): Unit = {
    val distanceToLocation1 = greatestCircleDistance(locations._1, location)
    val distanceToLocation2 = greatestCircleDistance(locations._2, location)

    val distanceToTemperature1 = temperatureDifference(temperatures._1, predictedTemperature)
    val distanceToTemperature2 = temperatureDifference(temperatures._2, predictedTemperature)
    if (distanceToLocation1 < distanceToLocation2) {
      val label =
        s"""Incorrect computed temperature at $location:$predictedTemperature.
           |Expected to be closer to ${temperatures._1} than ${temperatures._2}.
           |Distance to temperature1: $distanceToTemperature1
           |Distance to temperature2: $distanceToTemperature2
           |Distance to Location1: $distanceToLocation1
           |Distance to Location2: $distanceToLocation2
         """.stripMargin
      assert(distanceToTemperature1 < distanceToTemperature2, label)
    } else {
      val label =
        s"""Incorrect computed temperature at $location:$predictedTemperature.
           |Expected to be closer to ${temperatures._2} than ${temperatures._1}.
           |Distance to temperature1: $distanceToTemperature1
           |Distance to temperature2: $distanceToTemperature2
           |Distance to Location1: $distanceToLocation1
           |Distance to Location2: $distanceToLocation2
         """.stripMargin
      assert(distanceToTemperature2 <= distanceToTemperature1, label)
    }
  }

  def getTemperaturesLocations: ((Double, Double), (Location, Location), List[(Location, Double)]) = {
    val temperatures = (getTemperature, getTemperature)
    val locations = (getLocation, getLocation)
    val knownTemperatures = (locations._1, temperatures._1) :: (locations._2, temperatures._2) :: Nil
    (temperatures, locations, knownTemperatures)
  }

  def getTemperaturesLocationsColors: ((Double, Double), (Location, Location), (Color, Color), List[(Location, Double)], List[(Double, Color)]) = {
    val temperatures = (getTemperature, getTemperature)
    val locations = (getLocation, getLocation)
    val colors = (getColor, getColor)
    val knownTemperatures = (locations._1, temperatures._1) :: (locations._2, temperatures._2) :: Nil
    val colorsScale = (temperatures._1, colors._1) :: (temperatures._2, colors._2) :: Nil
    (temperatures, locations, colors, knownTemperatures, colorsScale)
  }

  private def randomInt(start: Int, end: Int): Int = Gen.choose(start, end).sample.get

  private def randomDouble(start: Double, end: Double, scale: Int): Double = {
    val dbl = Gen.choose(start, end).sample.get
    BigDecimal(dbl).setScale(scale, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  private def euclideanDistance(a1: Array[Double], a2: Array[Double]): Double = {
    assert(a1.length == a2.length)
    val sumOfSquares = a1.zip(a2).foldLeft(0.0) {
      case (sum, (v1, v2)) => sum + euclideanDistance(v1, v2)
    }
    math.sqrt(sumOfSquares)
  }

  private def euclideanDistance(a1: Double, a2: Double): Double = square(a2 - a1)

  private def square(a: Double): Double = a * a
}
