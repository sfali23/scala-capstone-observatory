package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Visualization.{interpolateColor, predictTemperature}
import org.apache.commons.math3.util.FastMath._


/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  private[observatory] def totalTiles(zoom: Int): Array[(Int, Int)] = {
    val numOfTiles = pow(2, 2 * zoom).toInt
    val tiles: Array[(Int, Int)] = new Array[(Int, Int)](numOfTiles)
    val width = pow(2, zoom).toInt
    val range = 0 until width
    for (y <- range) {
      for (x <- range) {
        tiles(x * width + y) = (x, y)
      }
    }
    tiles
  }

  private[observatory] def toTileXY(zoom: Int, location: Location): (Int, Int) = {
    val n = pow(2, zoom)
    val x = n * ((location.lon + 180) / 360)
    val lat_rad = toRadians(location.lat)
    val y = n * (1 - (log(tan(lat_rad) + (1 / cos(lat_rad))) / PI)) / 2
    (x.toInt, y.toInt)
  }

  /**
    * @param zoom Zoom level
    * @param x    X coordinate
    * @param y    Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    val _2PowerZoom = pow(2.0, zoom)
    val latitude = toDegrees(atan(sinh(PI - ((y.toDouble / _2PowerZoom) * (2.0 * PI)))))
    val longitude = ((x.toDouble / _2PowerZoom) * 360.0) - 180.0
    Location(latitude, longitude)
  }

  def tileBounds(zoom: Int, x: Int, y: Int): (Location, Location, Location, Location) = {
    (tileLocation(zoom, x, y), tileLocation(zoom, x, y + 1), tileLocation(zoom, x + 1, y), tileLocation(zoom, x + 1, y + 1))
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param zoom         Zoom level
    * @param x            X coordinate
    * @param y            Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)],
           zoom: Int, x: Int, y: Int): Image = tile(temperatures, colors, zoom, x, y, 256)


  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)],
           zoom: Int, x: Int, y: Int, tileSize: Int): Image = {
    AkkaAdapter().tile(temperatures, colors, zoom, x, y, tileSize)
  }

  def generateTiles[Data](yearlyData: Iterable[(Int, Data)], zoomAndTiles: Seq[(Int, Array[(Int, Int)])],
                          generateImage: (Int, Int, Int, Int, Data) => Unit): Unit = {
    val inputs = for {
      (year, data) <- yearlyData
      (zoom, tiles) <- zoomAndTiles
      (x, y) <- tiles
    } yield (year, zoom, x, y, data)

    inputs.foreach {
      case (year, zoom, x, y, data) => generateImage(year, zoom, x, y, data)
    }
  }

  /**
    * Generates all the tiles for the given zoom levels, for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param zoomLevels    Given zoom levels
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](yearlyData: Iterable[(Int, Data)], zoomLevels: Range,
                          generateImage: (Int, Int, Int, Int, Data) => Unit): Unit = {
    val zoomAndTiles = zoomLevels.map(zoom => (zoom, totalTiles(zoom)))
    generateTiles(yearlyData, zoomAndTiles, generateImage)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](yearlyData: Iterable[(Int, Data)],
                          generateImage: (Int, Int, Int, Int, Data) => Unit): Unit =
    generateTiles(yearlyData, 0 to 3, generateImage)

}
