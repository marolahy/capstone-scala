package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.math.{floor, abs}
import java.nio.file.{Files, Paths}
import Visualization._

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
    d00 * (1 - point.x) * (1 - point.y) + d10 * point.x * (1 - point.y) + d01 * (1 - point.x) * point.y + d11 * point.x * point.y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {
    val size = 256 // IMPORTANT : CHANGE TO 256 DURING SUBMISSION
    val subTileZoomFactor = (scala.math.log(size) / scala.math.log(2)).toInt
    val targetSize = 256
    val pixels = Array.fill[Pixel](size * size)(new Pixel(getARGBPackedInt(Color(0, 0, 0))))
    var rowNum = 0

    val t2 = System.nanoTime
    (0 until size).map(x => (0 until size).map((x, _))).foreach {
      case v => 
        val t1 = System.nanoTime
        v.par.foreach {
          case (x, y) =>
            val requiredLocation: Location = Interaction.tileLocation(Tile(size * tile.x + x, size * tile.y + y, tile.zoom + subTileZoomFactor))    
            val predictedTemperature = predictTemperature(requiredLocation, grid)
            //println("Bilinear Interpolation for ", cellPoint, d00, d01, d10, d11, predictedTemperature)

            val interpolatedColor = Visualization.interpolateColor(colors, predictedTemperature)
            val arrIndex = y * size + x
            pixels(arrIndex) = new Pixel(getARGBPackedInt(interpolatedColor))
            //println("Generating Pixel at ", x, y, tile.zoom, requiredLocation, predictedTemperature, interpolatedColor)
        }
        rowNum += 1
        println("Elapsed time for row " + rowNum + " = " + (System.nanoTime - t1) / 1e9d)
    }
    println("Elapsed time for tile " + tile + " = " + (System.nanoTime - t2) / 1e9d)

    if (targetSize == size) Image(size, size, pixels)
    else Image(size, size, pixels).scale(targetSize / size)
  }

  def predictTemperature(requiredLocation: Location, grid: GridLocation => Temperature) = {
    val latInt = requiredLocation.lat.toInt
    val lonInt = requiredLocation.lon.toInt
    val cellPoint = CellPoint(requiredLocation.lon - lonInt, requiredLocation.lat - latInt)
    val d00 = grid(GridLocation(latInt, lonInt))
    val d01 = grid(GridLocation(latInt + 1, lonInt))
    val d10 = grid(GridLocation(latInt, lonInt + 1))
    val d11 = grid(GridLocation(latInt + 1, lonInt + 1))    
    bilinearInterpolation(cellPoint, d00, d01, d10, d11)

    // val latInt = requiredLocation.lat.toInt
    // val lonInt = requiredLocation.lon.toInt
    // val cellPoint = CellPoint(abs(requiredLocation.lon - lonInt), abs(requiredLocation.lat - latInt))
    // val d00 = grid(GridLocation(latInt, lonInt))
    // val d01 = grid(GridLocation(latInt - 1, lonInt))
    // val d10 = grid(GridLocation(latInt, lonInt + 1))
    // val d11 = grid(GridLocation(latInt - 1, lonInt + 1))    
    // bilinearInterpolation(cellPoint, d00, d01, d10, d11)
  }

  def generateTiles(
    year: Year,
    grid: GridLocation => Temperature,
    generateImage: (Year, Tile, GridLocation => Temperature) => Unit
  ): Unit = {
    val zooms = Seq(0, 1, 2, 3)
    zooms
      .flatMap(zoom => (0 until 1 << zoom)
        .flatMap(x => (0 until 1 << zoom)
          .map((x, _, zoom))))
      .grouped(3)
      .toList
      .foreach {
        case list =>          
          list.par.foreach {
            case (x, y, zoom) => 
              println("Generating Image at Tile : ", (x, y, zoom))
              generateImage(year, Tile(x, y, zoom), grid)
              println("Generated Image Successfully at Tile : ", (x, y, zoom))
          }          
      }
  }

  def generateImage(year: Year, tile: Tile, grid: GridLocation => Temperature) = {
    val colors = Seq[(Temperature, Color)](
      (-60, Color(0, 0, 0)),
      (-50, Color(33, 0, 107)),
      (-27, Color(255, 0, 255)),
      (-15, Color(0, 0, 255)),
      (0, Color(0, 255, 255)),
      (12, Color(255, 255, 0)),
      (32, Color(255, 0, 0)),
      (60, Color(255, 255, 255))
    )
    val image = Visualization2.visualizeGrid(grid, colors, tile)
    Files.createDirectories(Paths.get("target/deviations/" + year + "/" + tile.zoom));
    image.output(new java.io.File("target/deviations/" + year + "/" + tile.zoom + "/" + tile.x + "-" + tile.y + ".png"))
  }

  private def getARGBPackedInt(color: Color) = {
    (127 << 24) + (color.red << 16) + (color.green << 8) + color.blue
  }

}