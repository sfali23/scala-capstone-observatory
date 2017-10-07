package observatory

import java.nio.file.{Files, Paths}

import observatory.Extraction.{locateTemperatures, locationYearlyAverageRecords}
import observatory.Interaction.generateTiles

import scala.collection.mutable.ListBuffer

object Main extends App {

  private type Data = Iterable[(Location, Double)]

  private val colorScales: List[(Double, Color)] = List(
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

  /*private val year = 1985
  println(Utils.convertTime(runMakeGrid(year)))
  println(Utils.convertTime(testTile(year, 0, 0, 0)))
  println(Utils.convertTime(testVisualizeFull(year)))*/

  var ls = ListBuffer[Location]()
 /* val map = (for {
    y <- Manipulation.LATITUDE_START to Manipulation.LATITUDE_END
    x <- Manipulation.LONGITUDE_START to Manipulation.LONGITUDE_END
  } yield (y, x) -> 0.0).toMap*/

  for (y <- -89 to 90) {
    for (x <- -180 to 179) {
      ls += Location(y, x)
    }
  }

  println(ls.length)
  println(ls.take(10).mkString(System.lineSeparator()))
  println("*" * 100)
  println(ls.takeRight(10).mkString(System.lineSeparator()))
  println("*" * 100)
  println()
  val i = ls.indexWhere(_ == Location(0, 0))
  println(i)

  ls = ListBuffer[Location]()
  for (y <- 0 until 180) {
    for (x <- 0 until 360) {
      ls += Location(90 - y, x - 180)
    }
  }
  println(ls.length)
  println(ls.take(10).mkString(System.lineSeparator()))
  println("*" * 100)
  println(ls.takeRight(10).mkString(System.lineSeparator()))
  println("*" * 100)
  println()

  /* private val year = 1992
   val deviation = readDeviation(year)
   val image = visualizeGrid(deviation, colorScales2, 1, 1, 1)
   image.output(Paths.get("images", s"$year.png"))*/

  /*(2006 to 2015).foreach { year =>
    val temperatures = getTemperatures(year)
    val startTime = System.nanoTime()
    saveDeviation(year, temperatures)
    val time = convertTime(System.nanoTime() - startTime)
    println(s"Time to get averages for year $year is $time")
  }*/

  /*private val startTime1 = System.nanoTime()
  val grid = Manipulation.makeGrid(temperatures)
  println(convertTime(System.nanoTime() - startTime1))
  println(grid(0, 0))
  println(grid(90, 90))*/

  /*val time = testTile(2015, 0, 0, 0)
  println(s"Time taken to visualize year 2105 is ${convertTime(time)}")*/

  /*

  (2015 :: Nil).foreach { year =>
    val time = testVisualizeFull(year)
    println(s"Time taken to visualize year $year is ${convertTime(time)}")
  }
  Extraction.spark.close()

  private val timeElapsed: Long = System.nanoTime() - startTime
  println(s"Total time taken ${convertTime(timeElapsed)}")*/

  private def getTemperatures(year: Int): Iterable[(Location, Double)] = {
    val startTime = System.nanoTime()
    val temperatures = Extraction.locateTemperatures(year, "/stations.csv", s"/$year.csv")
    val result = Extraction.locationYearlyAverageRecords(temperatures)
    val time = Utils.convertTime(System.nanoTime() - startTime)
    println(s"Took $time to get temperatures of year $year")
    result
  }

  private def testTile(year: Int, zoom: Int, x: Int, y: Int): Long = {
    val startTime = System.nanoTime()

    val temperatures = getTemperatures(year)

    val dir = Paths.get("images", "temperatures", year.toString, zoom.toString)
    if (!Files.exists(dir)) {
      Files.createDirectories(dir)
    }

    val path = Paths.get(dir.toString, s"$x-$y.png")

    val startTileTime = System.nanoTime()
    val image = Interaction.tile(temperatures, colorScales, zoom, x, y)
    val time = Utils.convertTime(System.nanoTime() - startTileTime)
    println(s"Took $time to generate tile for year $year")
    image.output(path)

    System.nanoTime() - startTime
  }

  private def testVisualizeFull(year: Int): Long = {
    val startTime = System.nanoTime()
    val temperatures = getTemperatures(year)

    val startVisualizeTime = System.nanoTime()
    val image = Visualization.visualize(temperatures, colorScales)
    val time = Utils.convertTime(System.nanoTime() - startVisualizeTime)
    println(s"Took $time to visualize year $year")

    image.output(Paths.get("images", s"$year.png"))
    System.nanoTime() - startTime
  }

  private def generateImage(year: Int, zoom: Int, x: Int, y: Int, data: Data): Unit = {
    val startTime = System.nanoTime()
    println(s"Generating image for year $year @ zoom level $zoom for tile ($x, $y)")
    val image = Interaction.tile(data, colorScales, zoom, x, y)

    val dir = Paths.get("images", "temperatures", year.toString, zoom.toString)
    if (!Files.exists(dir)) {
      Files.createDirectories(dir)
    }
    val path = Paths.get(dir.toString, s"$x-$y.png")
    val imagePath = image.output(path)
    println(s"Image generated: ${imagePath.toAbsolutePath}")
    val time = Utils.convertTime(System.nanoTime() - startTime)
    println(s"Time taken to generate image for year $year @ zoom level $zoom for tile ($x, $y) is $time")
  }

  def runGenerateTiles(year: Int): Unit = {
    val temps = locateTemperatures(year, "/stations.csv", s"/$year.csv")
    val temperatures = locationYearlyAverageRecords(temps)
    val yearlyData = List((year, temperatures))
    generateTiles(yearlyData, generateImage)
  }

  def runMakeGrid(year: Int): Long = {
    val startTime = System.nanoTime()
    val temperatures = getTemperatures(year)

    val startGridTime = System.nanoTime()
    Manipulation.makeGrid(temperatures)
    val time = Utils.convertTime(System.nanoTime() - startGridTime)
    println(s"Took $time to make grid for year $year")
    System.nanoTime() - startTime
  }

}
