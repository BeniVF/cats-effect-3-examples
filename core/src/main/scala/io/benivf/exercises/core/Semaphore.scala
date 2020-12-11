package io.benivf.exercises.core

import cats.effect._
import io.benivf.exercises.kernel

trait Semaphore {
  def acquire: IO[Unit]
  def release: IO[Unit]
}
object Semaphore {

  def apply(permits: Int): IO[Semaphore] =
    kernel.Semaphore[IO](permits).map { x =>
      new Semaphore {
        def acquire: IO[Unit] = x.acquire
        def release: IO[Unit] = x.release
      }
    }
}
