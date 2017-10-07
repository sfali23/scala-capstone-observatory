package observatory

import java.nio.file.Paths
import java.time.LocalDate

import org.apache.log4j.{Level, Logger}
import org.apache.spark.sql._
import org.apache.spark.sql.types._

/**
  * 1st milestone: data extraction
  */
object Extraction {

  private[observatory] val STN_IDENTIFIER_COLUMN_NAME = "stnId"
  private[observatory] val WBAN_IDENTIFIER_COLUMN_NAME = "wbanId"
  private[observatory] val LATITUDE_COLUMN_NAME = "latitude"
  private[observatory] val LONGITUDE_COLUMN_NAME = "longitude"
  private[observatory] val MONTH_COLUMN_NAME = "month"
  private[observatory] val DAY_COLUMN_NAME = "day"
  private[observatory] val TEMPERATURE_COLUMN_NAME = "Temperature"

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  val spark: SparkSession = SparkSession.builder().appName("Time Usage").config("spark.master", "local").getOrCreate()

  def locateTemperaturesDF(year: Int, stationsFile: String, temperaturesFile: String): DataFrame = {
    val stationsDF = loadStationsFile(stationsFile).na.drop(Array(LATITUDE_COLUMN_NAME, LONGITUDE_COLUMN_NAME))
    val temperaturesDF = loadTemperaturesFile(temperaturesFile)

    val joinCondition1 = stationsDF(STN_IDENTIFIER_COLUMN_NAME) === temperaturesDF(STN_IDENTIFIER_COLUMN_NAME)
    val joinCondition2 = stationsDF(WBAN_IDENTIFIER_COLUMN_NAME) === temperaturesDF(WBAN_IDENTIFIER_COLUMN_NAME)
    stationsDF.join(temperaturesDF, joinCondition1 && joinCondition2)
      .select(LATITUDE_COLUMN_NAME, LONGITUDE_COLUMN_NAME, MONTH_COLUMN_NAME, DAY_COLUMN_NAME, TEMPERATURE_COLUMN_NAME)
  }

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val df = locateTemperaturesDF(year, stationsFile, temperaturesFile)
    collect(year, df)
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records.groupBy(_._2).par.map {
      case (location, ls) => (location, ls.foldLeft(0.0)((accum, next) => accum + next._3) / ls.size)
    }.to[Iterable]
  }

  private def parseRow(row: Row)(year: Int): (LocalDate, Location, Double) = {
    (LocalDate.of(year, row.getAs[Int](MONTH_COLUMN_NAME), row.getAs[Int](DAY_COLUMN_NAME)),
      Location(row.getAs[Double](LATITUDE_COLUMN_NAME), row.getAs[Double](LONGITUDE_COLUMN_NAME)),
      fahrenheitToCelsius(row.getAs[Double](TEMPERATURE_COLUMN_NAME))
    )
  }

  private[observatory] def collect(year: Int, df: DataFrame) = df.collect().toStream.map(parseRow(_)(year))

  private[observatory] def loadStationsFile(stationsFile: String): DataFrame = {
    val schema = StructType(
      StructField(STN_IDENTIFIER_COLUMN_NAME, StringType, nullable = true) ::
        StructField(WBAN_IDENTIFIER_COLUMN_NAME, StringType, nullable = true) ::
        StructField(LATITUDE_COLUMN_NAME, DoubleType, nullable = true) ::
        StructField(LONGITUDE_COLUMN_NAME, DoubleType, nullable = true) ::
        Nil
    )
    val rdd = spark.sparkContext.textFile(fsPath(stationsFile))
    val data = rdd.map(_.split(",", -1)).map(getRowForStation)
    spark.createDataFrame(data, schema)
  }

  private[observatory] def loadTemperaturesFile(temperaturesFile: String): DataFrame = {
    val schema = StructType(
      StructField(STN_IDENTIFIER_COLUMN_NAME, StringType, nullable = true) ::
        StructField(WBAN_IDENTIFIER_COLUMN_NAME, StringType, nullable = true) ::
        StructField(MONTH_COLUMN_NAME, IntegerType, nullable = true) ::
        StructField(DAY_COLUMN_NAME, IntegerType, nullable = true) ::
        StructField(TEMPERATURE_COLUMN_NAME, DoubleType, nullable = true) ::
        Nil
    )
    val rdd = spark.sparkContext.textFile(fsPath(temperaturesFile))
    val data = rdd.map(_.split(",", -1)).map(getRowForTemperature)
    spark.createDataFrame(data, schema)
  }

  private def getDoubleValue(s: String, defaultValue: Double = 0.0): Double = if (s.isEmpty) defaultValue else s.toDouble

  private def getIntegerValue(s: String): Int = if (s.isEmpty) 0 else s.toInt

  private[observatory] def fahrenheitToCelsius(temperature: Double): Double = ((temperature - 32) * 5) / 9

  /**
    * @param resource resource to read
    * @return The filesystem path of the given resource
    */
  private def fsPath(resource: String): String = Paths.get(getClass.getResource(resource).toURI).toString

  private def getRowForStation(ls: Array[String]): Row =
    Row.fromSeq(ls(0) :: ls(1) :: getDoubleValue(ls(2), Double.NaN) :: getDoubleValue(ls(3), Double.NaN) :: Nil)

  private def getRowForTemperature(ls: Array[String]): Row =
    Row.fromSeq(ls(0) :: ls(1) :: getIntegerValue(ls(2)) :: getIntegerValue(ls(3)) :: getDoubleValue(ls(4)) :: Nil)

}
