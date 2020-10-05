package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val (p1, l1) = (location.lat.toRadians, location.lon.toRadians)
    val distanceTemp: Iterable[(Double, Temperature)] = temperatures
      .map({ case (Location(lat, lon), temp) => ((lat.toRadians, lon.toRadians), temp)})
      .map({ case ((p2, l2), temp) => (computeDistance(p1, l1, p2, l2), temp)})
    val underOneKilometer: List[(Double, Temperature)] = distanceTemp.filter({ case (dist, _) => dist <= 1 }).toList
    val closestUnderOneKilometer: Option[(Double, Temperature)] =
      if (underOneKilometer.isEmpty) None else Some(underOneKilometer.sortBy(_._1).take(1).head)

    if (closestUnderOneKilometer.isDefined) closestUnderOneKilometer.get._2
    else inverseDistanceWeighting(distanceTemp)
  }

  def inverseDistanceWeighting(distanceTemp: Iterable[(Double, Temperature)])(implicit p: Int = 3): Temperature = {
    def weight(distance: Double): Double = 1 / Math.pow(distance, p)
    val (numerator, denominator) = distanceTemp
      .aggregate((0d, 0d))(
        { case ((numerator, denominator), (dist, temp)) => (numerator + weight(dist) * temp, denominator + weight(dist)) },
        { case ((numerator1, denominator1), (numerator2, denominator2)) => (numerator1 + numerator2, denominator1 + denominator2)})

    numerator / denominator
  }

  def computeDistance(p1: Double, l1: Double, p2: Double, l2: Double): Double = {
    import Math._

    lazy val areSameLocation = p1 == p2 && (l1 == l2 || (abs(l1) == 180 && abs(l2) == 180))
    lazy val areAntipodes = p1 == -p2 && (l1 == l2 - 180 || l1 == l2 + 180)

    val centralAngle =
      if (areSameLocation) 0
      else if (areAntipodes) PI
      else acos(sin(p1) * sin(p2) + cos(p1) * cos(p2) * cos(abs(l1 - l2)))
    val earthRadius = 6371 // [km]

    earthRadius * centralAngle
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    if (points.isEmpty) throw new Exception("no points")

    // is foldLeft more performant than max . filter ???
    val closestLowerOpt = points.foldLeft[Option[(Temperature, Color)]](None)((acc, curr) => (acc, curr) match {
      case (_, (temp, _)) if temp > value => acc
      case (Some((tempAcc, _)), (temp, _)) if tempAcc > temp => acc
      case _ => Some(curr)
    })

    val closestHigherOpt = points.foldLeft[Option[(Temperature, Color)]](None)((acc, curr) => (acc, curr) match {
      case (_, (temp, _)) if temp < value => acc
      case (Some((tempAcc, _)), (temp, _)) if tempAcc < temp => acc
      case _ => Some(curr)
    })

    if (closestLowerOpt.isEmpty) return closestHigherOpt.get._2
    if (closestHigherOpt.isEmpty) return closestLowerOpt.get._2

    val closestLower = closestLowerOpt.get
    val closestHigher = closestHigherOpt.get

    if (closestLower._1 == closestHigher._1) return closestLower._2

    val fraction = (value - closestLower._1) / (closestHigher._1 - closestLower._1)

    Color(
      interpolateSingleColor(closestHigher._2.red, closestLower._2.red, fraction),
      interpolateSingleColor(closestHigher._2.green, closestLower._2.green, fraction),
      interpolateSingleColor(closestHigher._2.blue, closestLower._2.blue, fraction)
    )
  }

  def interpolateSingleColor(color1: Int, color2: Int, fraction: Double): Int = {
    Math.rint((color1 - color2) * fraction + color2).toInt
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val (w, h) = (360, 180)
    val pixels = new Array[Pixel](w * h)

    for (lat <- 90 to -89 by -1) {
//      println("!!!!!!!!!!!!!!!! x", lat)
      for (lon <- -180 to 179) {
        val temperature = predictTemperature(temperatures, Location(lat, lon))
        val color = interpolateColor(colors, temperature)
        val arrayIdx = w * Math.abs(lat - 90) + (lon + 180)

        pixels(arrayIdx) = Pixel(color.red, color.green, color.blue, 255)
      }
    }

    Image(w, h, pixels)
  }

}

