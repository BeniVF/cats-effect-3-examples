package io.benivf.exercises

import java.time._
import java.util.concurrent.TimeoutException

import scala.concurrent.duration._

import cats.Show
import cats.effect.IO
import cats.syntax.all._

package object core {

  def time: IO[Instant] = IO(Instant.now)

  def putStrLn[A: Show](a: A): IO[Unit] = time >>= { current =>
    IO {
      val name = Thread.currentThread().getName()
      println(s"[${current.toEpochMilli()}:$name] ${a.show}")
    }
  }

  def simulateTask(id: Int, time: FiniteDuration): IO[String] =
    putStrLn(s"Starting $id (expected time $time)") >>
      IO.sleep(time) >>
      putStrLn(s"Done $id").as(id.show)

  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] =
    kernel.timeout[IO, A](io, duration)

  def parTraverse[A, B](as: List[A])(f: A => IO[B]): IO[List[B]] =
    kernel.parTraverse[IO, A, B](as)(f)

}
