package observatory

case class Location(lat: Double, lon: Double)

case class Color(red: Int, green: Int, blue: Int)

class Grid {

  private val grid: Array[Double] = new Array[Double](Manipulation.LENGTH)

  def +(latitude: Int, longitude: Int, temperature: Double): Grid = {
    grid(getIndex(latitude, longitude)) = temperature
    this
  }

  def ++(other: Grid): Grid = {
    for (i <- other.grid.indices) {
      grid(i) = grid(i) + other.grid(i)
    }
    this
  }

  def temperature(latitude: Int, longitude: Int): Double = grid(getIndex(latitude, longitude))

  private def getIndex(latitude: Int, longitude: Int): Int = {
    val x = Manipulation.xOrigin + longitude
    val y = Manipulation.yOrigin - latitude
    val index = y * Manipulation.WIDTH + x
    if (index >= Manipulation.LENGTH) {
      println(s"$index, $latitude, $longitude")
    }
    index
  }
}
