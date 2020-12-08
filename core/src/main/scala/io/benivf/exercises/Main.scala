package io.benivf.exercises

import cats.effect.IOApp
import cats.effect.IO
import scala.concurrent.duration._
import cats.syntax.all._

object Main extends IOApp {
  val max: Int = 10
  import exercises.io._

  def run(args: List[String]): IO[Int] =
    parTraverse((1 to 10).toList)(x =>
      simulateTask(x, ((max - x) * 100).millis)
    ) >>= { x =>
      IO(println(x)).as(0)
    }

}
