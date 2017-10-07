package observatory

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Interaction.{tileLocation, toTileXY}

/**
  * @author sali
  */
class DispatcherActor(master: ActorRef, width: Int, height: Int) extends Actor with ActorLogging {

  private val length = width * height
  private val pixels = new Array[Pixel](length)
  private val counter = new AtomicInteger(0)
  private val startTime = System.nanoTime()
  private val originalSender: AtomicReference[ActorRef] = new AtomicReference[ActorRef]()
  private val grid: AtomicReference[Grid] = new AtomicReference[Grid]() {
    {
      set(new Grid)
    }
  }

  override def receive: Receive = {
    case s: StartInterpolateColor =>
      originalSender.set(sender())
      for (x <- 0 until width) {
        for (y <- 0 until height) {
          val index = y * width + x
          val location = Location(DispatcherActor.yOrigin - y, x - DispatcherActor.xOrigin)
          master ! InterpolateColorWork(index, location, s.alpha)
        }
      }
    case cp: ComputedPixel =>
      // println(s"Got pixel ${cp.pixel} @ ${cp.index}")
      pixels(cp.index) = cp.pixel
      val count = counter.incrementAndGet()
      if (count >= length) {
        val image = Image(width, height, pixels)
        val time = Utils.convertTime(System.nanoTime() - startTime)
        log.info(s"Took $time to process image")
        originalSender.get() ! image
        // self ! PoisonPill
        val _ = context.system.terminate()
      }
    case s: StartTileGeneration =>
      originalSender.set(sender())
      val location = tileLocation(s.zoom, s.x, s.y)
      val (zoomedX, zoomedY) = toTileXY(s.zoom + 8, location)
      for (i <- 0 until width) {
        val ty = zoomedY + i
        for (j <- 0 until width) {
          val index = i * width + j
          val tx = zoomedX + j
          master ! InterpolateColorWork(index, tileLocation(s.zoom + 8, tx, ty), s.alpha)
        }
      }
    case StartMakeGrid =>
      originalSender.set(sender())
      for (x <- 0 until width) {
        for (y <- 0 until height) {
          master ! MakeGridWork(Location(DispatcherActor.yOrigin - y, x - DispatcherActor.xOrigin))
        }
      }
    case ct: ComputedTemperature =>
      val location = ct.location
      grid.set(grid.get() + (location.lat.toInt, location.lon.toInt, ct.temperature))
      val count = counter.incrementAndGet()
      if (count >= length) {
        val time = Utils.convertTime(System.nanoTime() - startTime)
        log.info(s"Took $time to make grid")
        originalSender.get() ! grid.get()
        val _ = context.system.terminate()
      }
    case o => log.warning("DispatcherActor: Unknown message", o)
  }
}

object DispatcherActor {

  private val xOrigin = 180
  private val yOrigin = 90

  def props(master: ActorRef, width: Int, height: Int): Props =
    Props(classOf[DispatcherActor], master, width, height)

}
