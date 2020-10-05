package observatory

import observatory.Visualization.predictTemperature

import scala.collection.mutable

/**
  * 4th milestone: value-added information
  */
object Manipulation extends ManipulationInterface {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    val predictedTemperatures = new mutable.HashMap[String, Temperature]()
    for (lat <- -89 to 90) {
      for (lon <- -180 to 179) {
        val temperature = predictTemperature(temperatures, Location(lat, lon))
        predictedTemperatures.put(s"$lat/$lon", temperature)
      }
    }

    def getTemperature (gridLocation: GridLocation): Temperature = {
      val GridLocation(lat, lon) = gridLocation
      predictedTemperatures.getOrElse(s"$lat/$lon", Int.MinValue)
    }

    getTemperature
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    val averages = new mutable.HashMap[String, Temperature]()
    val grids = temperaturess.map(makeGrid)
    val yearsCount = temperaturess.size

    for (lat <- -89 to 90) {
      for (lon <- -180 to 179) {
        val sum: Double = grids.map(_ (GridLocation(lat, lon))).sum
        averages.put(s"$lat/$lon", sum / yearsCount)
      }
    }

    def getAverageTemperature(gridLocation: GridLocation): Temperature = {
      val GridLocation(lat, lon) = gridLocation
      averages.getOrElse(s"$lat/$lon", Int.MinValue)
    }

    getAverageTemperature
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    val deviations = new mutable.HashMap[String, Temperature]()
    for (lat <- -89 to 90) {
      for (lon <- -180 to 179) {
        val temperature = predictTemperature(temperatures, Location(lat, lon))
        val normal = normals(GridLocation(lat, lon))
        deviations.put(s"$lat/$lon", temperature - normal)
      }
    }

    def getDeviation (gridLocation: GridLocation): Temperature = {
      val GridLocation(lat, lon) = gridLocation
      deviations.getOrElse(s"$lat/$lon", Int.MinValue)
    }

    getDeviation
  }


}

