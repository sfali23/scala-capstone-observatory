package observatory

import java.nio.file.Paths

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import Interaction._
import Extraction._
import TestUtils._
import com.sksamuel.scrimage.{Image, Pixel}

import scala.collection.concurrent.TrieMap

trait InteractionTest extends FunSuite with Checkers {

  private type Data = Iterable[(Location, Double)]

  private val tileSize = 256
  private val largeSize: Int = tileSize * 2
  test("tileLocation must return the corresponding latitude and longitude, given some Web Mercator coordinates") {
    val zoom = 1
    val tiles = totalTiles(zoom).mkString("[", ",", "]")
    println(s"Tiles for zoom level $zoom are $tiles")

    val tuples: Array[(Int, Int)] = totalTiles(zoom + 8)
    println(tuples.head + " : " + tuples.last)
    val subLocation0 = tileLocation(zoom + 8, tuples.head._1 + tileSize, tuples.head._2 + tileSize)
    val subLocation1 = tileLocation(zoom + 8, tuples.last._1 + 1, tuples.last._2 - largeSize + 1)

    println(tuples.take(10).mkString("[", ",", "]"))
    println(tuples.takeRight(10).mkString("[", ",", "]"))

    val nw = tileLocation(zoom, 1, 1)
    val ne = tileLocation(zoom, 2, 1)
    val sw = tileLocation(zoom, 1, 2)
    val se = tileLocation(zoom, 2, 2)
    println(s"$nw, $ne, $sw, $se")
  }

  test("tile") {
    val year = 2021
    val zoom = 0
    val temps = locateTemperatures(year, "/stations-test-2.csv", "/temperatures-test-2.csv")
    val temperatures = locationYearlyAverageRecords(temps)
    val image = tile(temperatures, colorScales, zoom, 0, 0)
    val path = image.output(s"$year.png")
    println(path.toAbsolutePath)
  }

  test("tile must be consistent across zoom levels") {
    def pasteImage(tile: Image, xoff: Int, yoff: Int, width: Int, largeImagePixels: Array[Pixel]): Unit = {
      val smallWidth = width / 2
      for {
        y <- 0 until smallWidth
        x <- 0 until smallWidth
      } {
        val bigpos = (y + yoff) * width + x + xoff
        largeImagePixels(bigpos) = tile.pixel(x, y)
      }
    }

    val temperatures = (getTemperature, getTemperature)
    val locations = (getLocation, getLocation)
    val colors = (getColor, getColor)

    val colorsScale = (temperatures._1, colors._1) :: (temperatures._2, colors._2) :: Nil
    val knownTemperatures = (locations._1, temperatures._1) :: (locations._2, temperatures._2) :: Nil

    val originalZoomLevel = 0
    val originalImage = tile(knownTemperatures, colorsScale, originalZoomLevel, 0, 0)
    originalImage.output(Paths.get("originalImage.png"))

    val nextZoomLevel = originalZoomLevel + 1
    val n = math.pow(2, nextZoomLevel).toInt
    val width = n * tileSize
    val largeImagePixels = Array.ofDim[Pixel](width * width)

    val nw = tile(knownTemperatures, colorsScale, nextZoomLevel, 0, 0)
    val ne = tile(knownTemperatures, colorsScale, nextZoomLevel, 1, 0)
    val sw = tile(knownTemperatures, colorsScale, nextZoomLevel, 0, 1)
    val se = tile(knownTemperatures, colorsScale, nextZoomLevel, 1, 1)
    nw.output(Paths.get("nw.png"))
    ne.output(Paths.get("ne.png"))
    sw.output(Paths.get("sw.png"))
    se.output(Paths.get("se.png"))

    pasteImage(nw, 0, 0, width, largeImagePixels)
    pasteImage(ne, tileSize, 0, width, largeImagePixels)
    pasteImage(sw, 0, tileSize, width, largeImagePixels)
    pasteImage(se, tileSize, tileSize, width, largeImagePixels)

    val largeImage = Image(largeSize, largeSize, largeImagePixels)
    val p = largeImage.output(Paths.get("largeImage.png"))
    val scaleDownImage = largeImage.scaleTo(tileSize, tileSize)
    val p1 = scaleDownImage.output(Paths.get("scaleDownImage.png"))

    val tuples = originalImage.pixels.zip(scaleDownImage.pixels).view.zipWithIndex.toStream
    tuples.foreach { case ((pixel1, pixel2), index) =>
      val color1 = pixel1.toColor
      val color2 = pixel2.toColor
      val c1 = Color(color1.red, color1.green, color1.blue)
      val c2 = Color(color2.red, color2.green, color2.blue)
      val diff = colorDifference(c1, c2)
      if (diff > 10.0) {
        println(s"$c1, $c2, $diff")
      }
    }


    println(s"${originalImage.color(0, 0)}, ${nw.color(0, 0)}, ${largeImage.color(0, 0)}")
  }

}
