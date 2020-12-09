package io.benivf.exercises

import cats.effect.IOApp
import cats.effect._
import cats.syntax.all._

import scala.concurrent.duration._

object Main extends IOApp {
  val max: Int = 10
  import exercises.io._

  def run(args: List[String]): IO[Int] =
    putStrLn(s"Create semaphore") >>
      Semaphore(1) >>= { s =>
      parTraverse((1 to 10).toList) { x =>
        s.acquire.bracket { _ =>
          putStrLn(s"Acquire $x") >>
            simulateTask(x, ((max - x) * 100).millis)
        } { _ =>
          s.release >>
            putStrLn(s"Release $x")
        }
      }
    } >>= { result =>
      putStrLn(s"Done! [${result.mkString(",")}]").as(0)
    }

}
