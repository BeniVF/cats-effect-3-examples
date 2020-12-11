package io.benivf.exercises.core

import cats.effect._
import io.benivf.exercises.kernel

trait Queue[A] {
  def put(a: A): IO[Unit]
  def tryPut(a: A): IO[Boolean]
  def take: IO[A]
  def tryTake: IO[Option[A]]
  def peek: IO[Option[A]]
}
object Queue {
  def apply[A](length: Int): IO[Queue[A]] =
    kernel.Queue[IO, A](length).map { x =>
      new Queue[A] {
        def put(a: A): IO[Unit] = x.put(a)
        def tryPut(a: A): IO[Boolean] = x.tryPut(a)
        def take: IO[A] = x.take
        def tryTake: IO[Option[A]] = x.tryTake
        def peek: IO[Option[A]] = x.peek
      }
    }

}
