package observatory

/**
  * @author sali
  */

class Grid(protected val grid: Map[(Int, Int), Double]) {

  def +(latitude: Int, longitude: Int, temperature: Double): Grid = {
    Grid(this.grid + ((latitude, longitude) -> temperature))
  }

  def ++(other: Grid): Grid = {
    Grid(this.grid ++ other.grid)
  }

  def temperature(latitude: Int, longitude: Int): Double = grid((latitude, longitude))

}

object Grid {

  def apply(): Grid = Grid(Map[(Int, Int), Double]())

  def apply(grid: Map[(Int, Int), Double]): Grid = new Grid(grid)
}
