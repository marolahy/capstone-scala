package observatory

import Visualization._

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
    val memo = Map[GridLocation, Temperature]()
    (gridLocation: GridLocation) => {
      memo.get(gridLocation) match {
        case Some(temperature) => temperature
        case _ => 
          val location = Location(gridLocation.lat, gridLocation.lon)
          Visualization.predictTemperature(temperatures, location)
      }
    }
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperatures: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    val yearlyGrids = temperatures.map {
      case averageLocationTemperatures => 
        makeGrid(averageLocationTemperatures)
    }
    val memo = Map[GridLocation, Temperature]()
    (gridLocation: GridLocation) => {
      memo.get(gridLocation) match {
        case Some(temperature) => temperature
        case _ => 
          val gridLocationYearlyTemperatures = yearlyGrids.map {
            case makeGridFn => 
              makeGridFn(gridLocation)
          }
          gridLocationYearlyTemperatures.sum / gridLocationYearlyTemperatures.size  
      }          
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    val makeGridFn = makeGrid(temperatures)
    val memo = Map[GridLocation, Temperature]()
    (gridLocation: GridLocation) => {
      memo.get(gridLocation) match {
        case Some(temperature) => temperature
        case _ =>
          val currentTemperature = makeGridFn(gridLocation)
          val normalTemperature = normals(gridLocation)
          currentTemperature - normalTemperature
      }      
    }
  }
}