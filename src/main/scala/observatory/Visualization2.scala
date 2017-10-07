package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Interaction.{tileLocation, toTileXY}
import org.apache.commons.math3.util.FastMath._

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  private[observatory] val TILE_SIZE = 256
  private[observatory] val LENGTH = TILE_SIZE * TILE_SIZE

  /**
    * @param x   X coordinate between 0 and 1
    * @param y   Y coordinate between 0 and 1
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(x: Double, y: Double, d00: Double, d01: Double,
                            d10: Double, d11: Double): Double =
    d00 * (1 - x) * (1 - y) + d10 * x * (1 - y) + d01 * (1 - x) * y + d11 * x * y

  /**
    * @param grid   Grid to visualize
    * @param colors Color scale to use
    * @param zoom   Zoom level of the tile to visualize
    * @param x      X value of the tile to visualize
    * @param y      Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(grid: (Int, Int) => Double, colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val location = tileLocation(zoom, x, y)
    val zoom8 = zoom + 8
    val (zoomedX, zoomedY) = toTileXY(zoom8, location)

    val pixels: Array[Pixel] = Array.ofDim[Pixel](LENGTH)
    for (i <- 0 until TILE_SIZE) {
      val ty = zoomedY + i
      for (j <- 0 until TILE_SIZE) {
        val index = i * TILE_SIZE + j
        val tx = zoomedX + j
        val currentLocation = tileLocation(zoom8, tx, ty)
        pixels(index) = interpolateColor(grid, colors, currentLocation)
      }
    }
    Image(TILE_SIZE, TILE_SIZE, pixels)
  }

  private def interpolateColor(grid: (Int, Int) => Double, colors: Iterable[(Double, Color)],
                               currentLocation: Location): Pixel = {
    val latFloor = floor(currentLocation.lat).toInt
    val latCeil = ceil(currentLocation.lat).toInt
    val lonFloor = floor(currentLocation.lon).toInt
    val lonCeil = ceil(currentLocation.lon).toInt

    // println(s"Location: $currentLocation, latFloor: $latFloor, latCeil: $latCeil, lonFloor: $lonFloor, lonCeil: $lonCeil")

    val d00 = t(grid, latFloor, lonFloor)
    val d01 = t(grid, latCeil, lonFloor)
    val d10 = t(grid, latFloor, lonCeil)
    val d11 = t(grid, latCeil, lonCeil)

    val deltaX = delta(currentLocation.lon, lonFloor, lonCeil)
    val deltaY = delta(currentLocation.lat, latFloor, latCeil)

    // println(s"d00: $d00, d01: $d01, d10: $d10, d11: $d11, deltaX: $deltaX, deltaY: $deltaY")

    val predictedTemperature = bilinearInterpolation(deltaX, deltaY, d00, d01, d10, d11)
    val color = Visualization.interpolateColor(colors, predictedTemperature)
    Pixel(color.red, color.green, color.blue, 127)
  }

  private def t(grid: (Int, Int) => Double, lat: Int, lon: Int): Double = {
    // val _lon = if (lon >= 180) 179 else lon
    // val _lat = if (lat >= -90) -89 else lat
    grid(lat, lon)
  }

  private def delta(x: Double, minX: Int, maxX: Int): Double = min(x - minX, maxX - x)
}
