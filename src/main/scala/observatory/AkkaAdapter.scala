package observatory

import akka.actor.ActorSystem
import akka.pattern._
import akka.stream.ActorMaterializer
import akka.util.Timeout
import com.sksamuel.scrimage.Image

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor}

/**
  * @author sali
  */
class AkkaAdapter {
  private implicit val system: ActorSystem = ActorSystem("Visualization")
  private implicit val materializer: ActorMaterializer = ActorMaterializer()
  private implicit val executionContext: ExecutionContextExecutor = system.dispatcher
  private implicit val timeout: Timeout = Timeout(3.minute)

  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)],
                width: Int, height: Int): Image = {
    val router = system.actorOf(MasterActor.props(temperatures, colors))
    val assembler = system.actorOf(DispatcherActor.props(router, width, height))

    val future = assembler ? StartInterpolateColor(255)
    val result = Await.result(future, timeout.duration).asInstanceOf[Image]
    result
  }

  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)],
           zoom: Int, x: Int, y: Int, tileSize: Int): Image = {
    val router = system.actorOf(MasterActor.props(temperatures, colors))
    val assembler = system.actorOf(DispatcherActor.props(router, tileSize, tileSize))

    val future = assembler ? StartTileGeneration(zoom, x, y, 127)
    val result = Await.result(future, timeout.duration).asInstanceOf[Image]
    result
  }

  def makeGrid(width: Int, height: Int, temperatures: Iterable[(Location, Double)]): (Int, Int) => Double = {
    val router = system.actorOf(MasterActor.props(temperatures, List()))
    val assembler = system.actorOf(DispatcherActor.props(router, width, height))

    val future = assembler ? StartMakeGrid
    val result = Await.result(future, timeout.duration).asInstanceOf[Grid]
    result.temperature
  }
}

object AkkaAdapter {

  def apply(): AkkaAdapter = new AkkaAdapter()

}