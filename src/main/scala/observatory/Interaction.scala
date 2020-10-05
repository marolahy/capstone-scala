package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Visualization.{interpolateColor, predictTemperature}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    import scala.math._

    Location(
      toDegrees(atan(sinh(Pi * (1.0 - 2.0 * tile.y.toDouble / (1 << tile.zoom))))),
      tile.x.toDouble / (1 << tile.zoom) * 360.0 - 180.0
    )
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
//    val (imageWidth, imageHeight) = (256, 256)
    val dimension = 32
    val (imageWidth, imageHeight) = (dimension, dimension)
    val pixels = new Array[Pixel](imageWidth * imageHeight)

    for (imageY <- 0 until imageHeight) {
      println("!!!!!!!!!!!!!!!! imageX", imageY)
      for (imageX <- 0 until imageWidth) {
        val pixelLocation = tileLocation(Tile(tile.x * dimension + imageX, tile.y * dimension + imageY, tile.zoom + 5))
        val temperature = predictTemperature(temperatures, pixelLocation)
        val color = interpolateColor(colors, temperature)
        val arrayIdx = imageWidth * imageY + imageX

        pixels(arrayIdx) = Pixel(color.red, color.green, color.blue, 127)
      }
    }

    Image(imageWidth, imageHeight, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    import scala.math.pow
    for (zoom <- 0 to 3) {
      for ((year, yearData) <- yearlyData) {
        val maxXY = pow(2, zoom).toInt - 1
        for (
          x <- 0 to maxXY;
          y <- 0 to maxXY
        ) {
          generateImage(year, Tile(x, y, zoom), yearData)
        }
      }
    }
  }

}
