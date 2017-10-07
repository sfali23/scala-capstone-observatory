package observatory

import com.sksamuel.scrimage.Image
import org.apache.commons.math3.util.FastMath._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  private[observatory] val WIDTH = 360
  private[observatory] val HEIGHT = 180
  private[observatory] val LENGTH = WIDTH * HEIGHT
  private[observatory] val xOrigin = 180
  private[observatory] val yOrigin = 90
  private[observatory] val POWER = 6
  private val RADIUS_OF_EARTH = 6371.0 // KM


  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    def seqOp(aggregator: (Double, Double, Double), element: (Location, Double)): (Double, Double, Double) = {
      val gcd = greatestCircleDistance(element._1, location)
      val _pow = pow(gcd, POWER)
      val numeratorSum = (element._2 / _pow) + aggregator._1
      val denominatorSum = (1 / _pow) + aggregator._2
      val minTemperature = if (gcd < 1.0) element._2 else aggregator._3
      (numeratorSum, denominatorSum, minTemperature)
    }

    def combOp(aggregator1: (Double, Double, Double), aggregator2: (Double, Double, Double)): (Double, Double, Double) = {
      val minTemperature = if (!aggregator1._3.isPosInfinity) aggregator1._3
      else aggregator2._3
      (aggregator1._1 + aggregator2._1, aggregator1._2 + aggregator2._2, minTemperature)
    }

    val (numeratorSum, denominatorSum, minTemperature) = temperatures.par.aggregate((0.0, 0.0, Double.PositiveInfinity))(seqOp, combOp)
    if (!minTemperature.isPosInfinity) minTemperature
    else numeratorSum / denominatorSum
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    if (points.isEmpty) throw new IllegalArgumentException("cannot interpolate colors from empty list")

    val min = points.minBy(_._1)
    val max = points.maxBy(_._1)
    if (points.size == 1 || value < min._1) min._2
    else if (value > max._1) max._2
    else interpolateColorRecursive(points, value)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    AkkaAdapter().visualize(temperatures, colors, WIDTH, HEIGHT)
  }

  private def interpolateColorRecursive(points: Iterable[(Double, Color)], value: Double): Color = {
    if (points.isEmpty) throw new RuntimeException(s"Could not interpolate color for value $value for given scale of points.")
    else {
      val lower = points.minBy(_._1)
      val filteredPoints = points.filterNot(_._1 == lower._1)
      val upper = filteredPoints.minBy(_._1)

      if (value >= lower._1 && value <= upper._1) interpolateColor(lower, upper, value)
      else interpolateColorRecursive(filteredPoints, value)
    }
  }

  private def interpolateColor(lowerPair: (Double, Color), upperPair: (Double, Color), value: Double): Color = {
    def linearInterpolate(x0: Double, x1: Double, y0: Int, y1: Int, x: Double): Int =
      rint(((y0 * (x1 - x)) + (y1 * (x - x0))) / (x1 - x0)).toInt

    val red = linearInterpolate(lowerPair._1, upperPair._1, lowerPair._2.red, upperPair._2.red, value)
    val green = linearInterpolate(lowerPair._1, upperPair._1, lowerPair._2.green, upperPair._2.green, value)
    val blue = linearInterpolate(lowerPair._1, upperPair._1, lowerPair._2.blue, upperPair._2.blue, value)
    Color(red, green, blue)
  }

  private[observatory] def greatestCircleDistance(location1: Location, location2: Location): Double = {
    def _sin(d: Double): Double = sin(toRadians(d))

    def _cos(d: Double): Double = cos(toRadians(d))

    val v1 = _sin(location1.lat) * _sin(location2.lat)
    val _abs = abs(toRadians(location1.lon) - toRadians(location2.lon))
    val v2 = _cos(location1.lat) * _cos(location2.lat) * cos(_abs)
    val v = v1 + v2
    acos(if (v > 1) 1 else if (v < -1) -1 else v) * RADIUS_OF_EARTH
  }

}

