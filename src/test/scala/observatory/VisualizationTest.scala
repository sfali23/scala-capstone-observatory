package observatory

import java.nio.file.Paths
import java.time.LocalDateTime

import TestUtils._
import observatory.Visualization._
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait VisualizationTest extends FunSuite with Checkers {

  test("distance between two locations") {
    val d = roundDouble4(greatestCircleDistance(Location(37.358, -78.438), Location(37.957, -78.439)))
    assert(d === 66.6058)
  }

  test("basic predictTemperature") {
    val data = Seq(
      (Location(37.35, -78.433), 27.3),
      (Location(37.358, -78.438), 1.0)
    )
    val t = predictTemperature(data, Location(38.350, -78.433))
    assert(t === 13.83350434430813)
  }

  test("predictTemperature") {
    println("Starting locateTemperatures")
    val temperatures = Extraction.locateTemperatures(1975, "/stations.csv", "/1975.csv")
    println("Finished locateTemperatures")

    println("Starting locationYearlyAverageRecords")
    val averageTemperatures = Extraction.locationYearlyAverageRecords(temperatures)
    println("Finished locationYearlyAverageRecords")

    val location = Location(168.0, 85.0)
    println(s"predicting temperature for location $location")

    println(s"Starting locationYearlyAverageRecords @ ${LocalDateTime.now()}")
    val temperature = Visualization.predictTemperature(averageTemperatures, location)
    println(s"Finished locationYearlyAverageRecords @ ${LocalDateTime.now()}")

    println(s"Temperature for location $location will be $temperature")
  }

  test("predictTemperature with distance < 1KM should result temperature of teh station") {
    val data = Seq(
      (Location(37.35, -78.433), 27.3),
      (Location(37.358, -78.438), 1.0)
    )
    val t = predictTemperature(data, Location(37.35, -78.444))
    assert(t === 27.3)
  }

  test("color interpolation") {
    def toHex(v: Int): String = {
      val s = v.toLong.toHexString.toUpperCase
      if (s.length <= 1) s + "0" else s
    }

    def findColor(temperature: Double): Unit = {
      val color = interpolateColor(colorScales, temperature)
      val hex = s"#${toHex(color.red)}${toHex(color.green)}${toHex(color.blue)}"
      println(s"color for $temperature is $hex")
    }

    def interpolateColors(start: Double, end: Double): Unit = {
      println(s"Colors between $start and $end")
      for (i <- start to end by 2) findColor(i)
      println()
    }

    interpolateColors(-60, -50)
    interpolateColors(-27, -15)
    interpolateColors(-15, 0)
    interpolateColors(0, 12)
    interpolateColors(12, 32)
    interpolateColors(32, 60)
  }

  test("basic color interpolation") {
    val temperature1 = 21.0
    val temperature2 = 95.0
    val colors = List((temperature1, Color(39, 20, 213)), (temperature2, Color(233, 170, 45)))
    val temperatures = List((Location(-7.699, 116.607), temperature1), (Location(-160.57, -181.691), temperature2))

    val location = Location(-77.0, 88.0)
    val color = Color(136, 95, 129)

    val distanceToLocation1 = greatestCircleDistance(temperatures.head._1, location)
    val distanceToLocation2 = greatestCircleDistance(temperatures.last._1, location)

    val distanceToColor1 = colorDifference(colors.head._2, color)
    val distanceToColor2 = colorDifference(colors.last._2, color)

    val predictedTemperature = predictTemperature(temperatures, location)
    val interpolatedColor = interpolateColor(colors, predictedTemperature)

    println(
      s"""Distance to Location1: $distanceToLocation1
         |Distance to Location2: $distanceToLocation2
         |Distance to Color2: $distanceToColor1
         |Distance to Color2: $distanceToColor2
       """.stripMargin)
    assert(color === interpolatedColor)
  }

  test("color interpolation with only one item in scale") {
    val scale = colorScales(3) :: Nil
    assert(scale.head._2 === interpolateColor(scale, 32))
    assert(scale.head._2 === interpolateColor(scale, -13))
  }

  test("predicted temperature at location z should be closer to known temperature at location x than to known temperature at location y, if z is closer (in distance) to x than y, and vice versa") {
    val temperatures = (getTemperature, getTemperature)
    val locations = (getLocation, getLocation)
    val location = getLocation
    val knownTemperatures = (locations._1, temperatures._1) :: (locations._2, temperatures._2) :: Nil

    println(s"Temperatures: $knownTemperatures")
    val predictedTemperature = predictTemperature(knownTemperatures, location)
    validatePredictTemperature(locations, temperatures, location, predictedTemperature)
  }

  test("exceeding the greatest value of a color scale should return the color associated with the greatest value") {
    assert(colorScales.head._2 === interpolateColor(colorScales, -100))
    assert(colorScales.last._2 === interpolateColor(colorScales, 100))
  }

  test("visualize") {
    val temperatures = (getTemperature, getTemperature)
    val locations = (getLocation, getLocation)
    val colors = (getColor, getColor)

    val colorsScale = (temperatures._1, colors._1) :: (temperatures._2, colors._2) :: Nil
    val knownTemperatures = (locations._1, temperatures._1) :: (locations._2, temperatures._2) :: Nil

    val gpsCoordinates = new Array[Location](LENGTH)
    for (x <- 0 until WIDTH) {
      for (y <- 0 until HEIGHT) {
        gpsCoordinates(y * WIDTH + x) = Location(yOrigin - y, x - xOrigin)
      }
    }

    println(s"Color scales: $colorsScale")
    println(s"Temperatures: $knownTemperatures")

    val image = visualize(knownTemperatures, colorsScale)
    image.output(Paths.get("image.png"))

    println("Validating")
    image.pixels.view.zipWithIndex.foreach {
      case (pixel, index) => validateVisualize(locations, colors, gpsCoordinates(index), pixel)
    }
  }

}
