package observatory

import observatory.Manipulation._
import observatory.TestUtils._
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait ManipulationTest extends FunSuite with Checkers {

  test("makeGrid must return a grid whose predicted temperatures are consistent with the known temperatures") {
    val (temperatures, locations, knownTemperatures) = getTemperaturesLocations

    val gpsCoordinates = new Array[Location](LENGTH)
    for (x <- 0 until WIDTH) {
      for (y <- 0 until HEIGHT) {
        gpsCoordinates(y * WIDTH + x) = Location(yOrigin - y, x - xOrigin)
      }
    }

    val fun = makeGrid(knownTemperatures)

    gpsCoordinates.foreach(location =>
      validateMakeGrid(locations, temperatures, location, fun(location.lat.toInt, location.lon.toInt))
    )
  }

  test("average must return a grid whose predicted temperatures are the average of the known temperatures") {
    val (_, _, knownTemperatures1) = getTemperaturesLocations
    val (_, _, knownTemperatures2) = getTemperaturesLocations
    val (_, _, knownTemperatures3) = getTemperaturesLocations
    val temperatures: List[List[(Location, Double)]] = knownTemperatures1 :: knownTemperatures2 :: knownTemperatures3 :: Nil
    val grids: List[(Int, Int) => Double] = temperatures.map(makeGrid)
    val r: List[Double] = grids.map(f => f(90, -180))
    println(s"Total Length: ${r.length}, Elements: ${r.mkString(",")}, Sum: ${r.sum}, Avg: ${r.sum / r.length}")

    val m = temperatures.map(_.filter(tuple => tuple._1 === Location(90.0, -180.0)))
    println(m)

    val gridOfAverages = average(temperatures)
    println(gridOfAverages(90, -180))
  }
}