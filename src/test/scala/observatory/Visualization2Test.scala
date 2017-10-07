package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import Visualization2._
import Manipulation._
import TestUtils._
import observatory.Visualization.{LENGTH, yOrigin}

trait Visualization2Test extends FunSuite with Checkers {

  test("bilinear interpolation") {
    assert(45.1 === bilinearInterpolation(0.1, 0.0, 50.0, 50.0, 1.0, 50.0))
  }

  test("grid visualization") {
    val (t1, t2, t3) = (getTemperature, getTemperature, getTemperature)
    val (l1, l2, l3) = (getLocation, getLocation, getLocation)
    val (c1, c2) = (getColor, getColor)

    val knownTemperatures1 = (l1, t1) :: (l2, t2) :: Nil
    val knownTemperatures2 = (l3, t3) :: Nil

    val colorScales = (t1, c1) :: (t2, c2) :: Nil

    val temperatures: List[List[(Location, Double)]] = knownTemperatures1 :: Nil

    val averages = average(temperatures)
    val deviations: (Int, Int) => Double = deviation(knownTemperatures2, averages)
    val image = visualizeGrid(deviations, colorScales, 0, 1, 1)

    val gpsCoordinates = getLocations.toSeq

    println(s"($t1, $t2)")
    println(s"($l1, $l2)")
    println(s"($c1, $c2)")
    image.pixels.view.zipWithIndex.foreach {
      case (pixel, index) => validateVisualize((l1, l2), (c1, c2), gpsCoordinates(index), pixel)
    }
  }

}
