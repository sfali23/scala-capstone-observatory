package observatory

import akka.actor.{Actor, ActorLogging, PoisonPill, Props}
import com.sksamuel.scrimage.Pixel
import observatory.Visualization.{interpolateColor, predictTemperature}

/**
  * @author sali
  */
class WorkerActor(temperatures: Iterable[(Location, Double)],
                  colors: Iterable[(Double, Color)]) extends Actor with ActorLogging {

  override def receive: Receive = {
    case w: InterpolateColorWork =>
      val temperature = predictTemperature(temperatures, w.location)
      val color = interpolateColor(colors, temperature)
      sender() ! ComputedPixel(w.index, Pixel(color.red, color.green, color.blue, w.alpha))
    case w: MakeGridWork =>
      val location = w.location
      val temperature = predictTemperature(temperatures, location)
      sender() ! ComputedTemperature(location, temperature)
    case o => log.warning("WorkerActor: Unknown message", o)
  }
}

object WorkerActor {

  def props(temperatures: Iterable[(Location, Double)],
            colors: Iterable[(Double, Color)]): Props = Props(classOf[WorkerActor], temperatures, colors)

}
