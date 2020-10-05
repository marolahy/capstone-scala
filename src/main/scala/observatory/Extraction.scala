package observatory

import java.io.InputStream
import java.time.LocalDate

import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.Row
import org.apache.spark.sql.types._

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {

  import org.apache.spark.sql.SparkSession

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Observatory")
      .master("local")
//      .config("spark.driver.host","172.18.1.194")
//      .config("spark.driver.bindAddress","192.168.132.1")
      .config("spark.driver.host", "localhost")
//      .config("spark.driver.port", "51265")
//      .config("spark.driver.bindAddress", "localhost")
      .getOrCreate()
  val sc: SparkContext = spark.sparkContext

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations_test.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    import spark.implicits._

    val stationsFields = Array(
      StructField("stn", IntegerType, nullable = true),
      StructField("wban", IntegerType, nullable = true),
      StructField("latitude", DoubleType, nullable = true),
      StructField("longitude", DoubleType, nullable = true)
    )
    val temperatureFields = Array(
      StructField("temp_stn", IntegerType, nullable = true),
      StructField("temp_wban", IntegerType, nullable = true),
      StructField("month", IntegerType, nullable = false),
      StructField("day", IntegerType, nullable = false),
      StructField("temperature", DoubleType, nullable = false)
    )

    val stationsSchema = StructType(stationsFields)
    val temperaturesSchema = StructType(temperatureFields)

    val stations = readLines(stationsFile)
    val stationsRdd: RDD[Row] = sc.parallelize(stations).map(parseStation)
    val stationsDf = spark.createDataFrame(stationsRdd, stationsSchema)

    val temperatures = readLines(temperaturesFile)
    val temperaturesRdd: RDD[Row] = sc.parallelize(temperatures).map(parseTemperature)
    val temperaturesDf = spark.createDataFrame(temperaturesRdd, temperaturesSchema)

    val knownStations = stationsDf
      .where($"stn".isNotNull || $"wban".isNotNull)
      .where($"latitude".isNotNull && $"longitude".isNotNull)
      .select("*")

    val temperaturesData = temperaturesDf.join(knownStations, $"temp_stn" <=> $"stn" &&  $"temp_wban" <=> $"wban", "inner")

    temperaturesData.rdd
      .map(r => (
        LocalDate.of(year, r.getAs[Int](2), r.getAs[Int](3)),
        Location(r.getAs[Double](7), r.getAs[Double](8)),
        roundToFirstDecimal(fahrenheitToCelsius(r.getAs[Double](4)))
      ))
      .collect()
      .toList
  }

  def readLines(file: String): List[String] = {
    val stream: InputStream = getClass.getResourceAsStream(file)
    scala.io.Source
      .fromInputStream(stream)
      .getLines()
      .toList
  }

  def parseStation(line: String): Row = {
    val arr = line.split(",", -1)
    Row(
      if (arr(0) == "") null else arr(0).toInt,
      if (arr(1) == "") null else arr(1).toInt,
      if (arr(2) == "") null else arr(2).toDouble,
      if (arr(3) == "") null else arr(3).toDouble )
  }

  def parseTemperature(line: String): Row = {
    val arr = line.split(",", -1)
    Row(
      if (arr(0) == "") null else arr(0).toInt,
      if (arr(1) == "") null else arr(1).toInt,
      arr(2).toInt,
      arr(3).toInt,
      arr(4).toDouble )
  }

  def roundToFirstDecimal(x: Double): Double = (math rint x * 10) / 10

  def fahrenheitToCelsius(fahrenheit: Double): Double = (fahrenheit - 32.0) * 5.0 / 9.0

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    val recordsRdd: RDD[(LocalDate, Location, Temperature)] = sc.parallelize(records.toSeq)

    recordsRdd
      .map({ case (_, location, temperature) => (location, temperature)})
      .groupByKey()
      .mapValues(temperatures => roundToFirstDecimal(temperatures.sum / temperatures.size))
      .collect()
      .toList
  }

}
