package observatory

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.{Actor, ActorLogging, PoisonPill, Props}
import akka.routing.{ActorRefRoutee, RoundRobinRoutingLogic, Router}

/**
  * @author sali
  */
class MasterActor(temperatures: Iterable[(Location, Double)],
                  colors: Iterable[(Double, Color)]) extends Actor with ActorLogging {

  private val routerCount = new AtomicInteger(0)

  private val router = {
    val routees = Vector.fill(36) {
      val index = routerCount.incrementAndGet()
      val r = context.actorOf(WorkerActor.props(temperatures, colors), s"worker-$index")
      context watch r
      ActorRefRoutee(r)
    }
    // The Router is immutable and the RoutingLogic is thread safe; meaning that they can also be used outside of actors.
    Router(RoundRobinRoutingLogic(), routees)
  }

  override def receive: Receive = {
    case work: InterpolateColorWork => router.route(work, sender())
    case work: MakeGridWork => router.route(work, sender())
    case o => log.warning("MasterActor: Unknown message", o)
  }
}

object MasterActor {

  def props(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Props =
    Props(classOf[WorkerActor], temperatures, colors)

}
