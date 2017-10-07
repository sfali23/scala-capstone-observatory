package observatory

import java.nio.file.{Files, Path, Paths}

import scala.collection.JavaConverters._

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  private[observatory] val LATITUDE_START = -89
  private[observatory] val LATITUDE_END = 90
  private[observatory] val LONGITUDE_START = -180
  private[observatory] val LONGITUDE_END = 179

  private[observatory] val WIDTH = 360
  private[observatory] val HEIGHT = 180
  private[observatory] val LENGTH = WIDTH * HEIGHT
  private[observatory] val xOrigin = 180
  private[observatory] val yOrigin = 90

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Double)]): (Int, Int) => Double = {
    AkkaAdapter().makeGrid(WIDTH, HEIGHT, temperatures)
  }

  /**
    * @param temperatures Sequence of known temperatures over the years (each element of the collection
    *                     is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperatures: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double =
    averageInternal(temperatures.map(makeGrid))

  private def averageInternal(grids: Iterable[(Int, Int) => Double]): (Int, Int) => Double = {
    val averages = for {
      x <- 0 until WIDTH
      y <- 0 until HEIGHT
      lat = yOrigin - y
      lon = x - xOrigin
      ls = grids.map(_ (lat, lon))
      avg = ls.sum / ls.size
    } yield (lat, lon, avg)

    val gridOfAverages = averages.foldLeft(new Grid) {
      case (grid, (lat, lon, avg)) => grid + (lat, lon, avg)
    }

    gridOfAverages.temperature
  }

  /**
    * @param temperatures Known temperatures
    * @param normals      A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Double)], normals: (Int, Int) => Double): (Int, Int) => Double = {
    val grid = makeGrid(temperatures)

    val latLongSeq = for {
      x <- 0 until WIDTH
      y <- 0 until HEIGHT
      lat = yOrigin - y
      lon = x - xOrigin
    } yield (lat, lon)

    val gridOfDeviations = latLongSeq.foldLeft(new Grid) {
      case (g, (lat, lon)) => g + (lat, lon, grid(lat, lon) - normals(lat, lon))
    }

    gridOfDeviations.temperature
  }

  private[observatory] def readAverage: (Int, Int) => Double =
    readGrid(1111)(Paths.get("data", "1111.csv"))

  private[observatory] def readDeviation(year: Int): (Int, Int) => Double =
    readGrid(year)(Paths.get("data", "deviations", s"$year.csv"))

  private[observatory] def saveGrid(year: Int, temperatures: Iterable[(Location, Double)]): Unit = {
    val path = saveData(Paths.get("data", "grids", s"$year.csv"), makeGrid(temperatures))
    println(s"File created ${path.toAbsolutePath}")
  }

  private[observatory] def saveAverages(): Unit = {
    val averages = averageInternal((1975 to 1990).map(year => readGrid(year)(Paths.get("data", "grids", s"$year.csv"))))
    val path = saveData(Paths.get("data", "1111.csv"), averages)
    println(s"File created ${path.toAbsolutePath}")
  }

  private[observatory] def saveDeviation(year: Int, temperatures: Iterable[(Location, Double)]): Unit = {
    val normals = readGrid(1111)(Paths.get("data", "1111.csv"))
    val path = saveData(Paths.get("data", "deviations", s"$year.csv"), deviation(temperatures, normals))
    println(s"File created ${path.toAbsolutePath}")
  }

  private def readGrid(year: Int)(path: Path): (Int, Int) => Double = {
    Files.readAllLines(path)
      .asScala.toList.map(_.split(","))
      .foldLeft(new Grid) {
        case (g, array) => g + (array(0).toInt, array(1).toInt, array(2).toDouble)
      }.temperature
  }

  private def saveData(path: Path, grid: (Int, Int) => Double): Path = {
    val latLongSeq = for {
      x <- 0 until WIDTH
      y <- 0 until HEIGHT
      lat = yOrigin - y
      lon = x - xOrigin
      temp = grid(lat, lon)
    } yield (lat, lon, temp)

    val ls = latLongSeq.map {
      case (lat, lon, temp) => lat.toInt.toString + "," + lon.toInt.toString + "," + temp.toString
    }.asJava

    Files.write(path, ls)
  }

}
