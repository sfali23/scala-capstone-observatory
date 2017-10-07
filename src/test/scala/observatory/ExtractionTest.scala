package observatory

import java.time.LocalDate

import observatory.Extraction._
import org.apache.spark.sql.Row
import org.scalatest.FunSuite

trait ExtractionTest extends FunSuite {

  private def rowToStation(row: Row): Station =
    Station(row.getAs[String](0), row.getAs[String](1), row.getAs[Double](2), row.getAs[Double](3))

  private def rowToTemperature(row: Row): Temperature =
    Temperature(row.getAs[String](0), row.getAs[String](1), row.getAs[Int](2), row.getAs[Int](2), row.getAs[Double](4))


  test("should load stations file") {
    val df = loadStationsFile("/stations-test.csv")
    assert(df != null)
    val stations = df.na.drop(Array("latitude", "longitude")).collect().toList.map(rowToStation)
    val expected = List(
      //Station("010013", "", Double.NaN, Double.NaN),
      Station("724017", "03707", 37.358, -78.438),
      Station("724017", "", 37.350, -78.433)
    )
    assert(stations === expected)
  }

  test("should load temperatures file") {
    val df = loadTemperaturesFile("/temperatures-test.csv")
    assert(df != null)
    val temperatures = df.collect().toList.map(rowToTemperature)
    val expected = List(
      Temperature("010013", "", 11, 11, 39.2),
      Temperature("724017", "", 8, 8, 81.14),
      Temperature("724017", "03707", 12, 12, 32.0),
      Temperature("724017", "03707", 1, 1, 35.6)
    )
    assert(temperatures === expected)
  }

  test("should locateTemperatures") {
    val results = locateTemperatures(2015, "/stations-test.csv", "/temperatures-test.csv")
      .map {
        case (localDate, location, temperature) => (localDate, location, TestUtils.roundDouble1(temperature))
      }.toSet
    val expected = Seq(
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0)
    ).toSet
    assert(results === expected)
  }

  test("should locationYearlyAverageRecords") {
    val results = locationYearlyAverageRecords(Seq(
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0)
    )).toSet

    val expected = Seq(
      (Location(37.35, -78.433), 27.3),
      (Location(37.358, -78.438), 1.0)
    ).toSet
    assert(results === expected)
  }

}

case class Station(stnId: String, wbanId: String, latitude: Double, longitude: Double)

case class Temperature(stnId: String, wbanId: String, month: Int, day: Int, temperature: Double)