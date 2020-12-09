package io.benivf.exercises

import cats.effect.IOApp
import cats.effect._

import scala.concurrent.duration._

object Main extends IOApp {
  val max: Int = 10
  import exercises.io._

  def run(args: List[String]): IO[Int] =
    for {
      _ <- putStrLn(s"Create semaphore")
      s <- Semaphore(1)
      result <- parTraverse((1 to 10).toList) { x =>
        s.acquire.bracket { _ =>
          putStrLn(s"Acquire $x") >>
            simulateTask(x, ((max - x) * 100).millis)
        } { _ =>
          s.release >>
            putStrLn(s"Release $x")
        }
      }
      _ <- putStrLn(s"Done! [${result.mkString(",")}]")
    } yield 0

}
