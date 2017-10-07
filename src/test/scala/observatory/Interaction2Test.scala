package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import Interaction2._

trait Interaction2Test extends FunSuite with Checkers {

  test("yearBounds must be consistent with the selected layer") {
    assert(yearBounds(Signal(availableLayers.head))() === (1975 to 2015))
    assert(yearBounds(Signal(availableLayers.last))() === (1990 to 2015))
  }

  test("layerUrlPattern must be consistent with the selected layer id and the selected year") {
    val head = Signal(availableLayers.head)
    val year = Signal(2000)
    val url = layerUrlPattern(head, year)()
    assert(url === "target/temperatures/2000/{z}/{x}-{y}.png")
  }

  test("yearSelection must never be out of the selected layer bounds") {
    val head = Signal(availableLayers.head)
    assert(1975 === yearSelection(head, Signal(1975))())
    assert(2015 === yearSelection(head, Signal(2015))())
    assert(1990 === yearSelection(head, Signal(1990))())
    assert(1975 === yearSelection(head, Signal(1970))())
    assert(2015 === yearSelection(head, Signal(2020))())
  }

  test("availableLayers must contain temperatures and deviations") {
    val layers = availableLayers
    val all = layers.forall(layer => layer.layerName.id === "temperatures" || layer.layerName.id === "deviations")
    assert(all === true)
  }

  test("caption must be consistent with the selected layer and the selected year") {
    val last = Signal(availableLayers.last)
    val year = Signal(1980)
    assert(caption(last, year)() === "Deviations(1980)")
  }
}
