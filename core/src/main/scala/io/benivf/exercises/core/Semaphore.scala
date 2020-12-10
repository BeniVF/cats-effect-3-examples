package io.benivf.exercises.core

import cats.effect._
import cats.effect.concurrent._
import cats.syntax.all._

trait Semaphore {
  def acquire: IO[Unit]
  def release: IO[Unit]
}
object Semaphore {

  def apply(permits: Int): IO[Semaphore] = Ref
    .of[IO, State](State(permits))
    .map(new Impl(_))

  private[this] final case class State(
      count: Int,
      waiting: List[Deferred[IO, Unit]]
  ) {
    def wait(process: Deferred[IO, Unit]): State =
      State(count, waiting.+:(process))
    def dec(): State = State(count - 1, waiting)
    private def inc: State = State(count + 1, waiting)
    def pop(): (State, Option[Deferred[IO, Unit]]) =
      if (waiting.nonEmpty)
        (State(count, waiting.drop(1)), waiting.headOption)
      else
        (this.inc, None)
  }

  private[this] object State {
    def apply(permits: Int): State = State(permits, List.empty)
  }

  private[this] final class Impl(state: Ref[IO, State]) extends Semaphore {
    def acquire: IO[Unit] = Deferred[IO, Unit] >>= { process =>
      state.modify {
        case x if (x.count == 0) =>
          x.wait(process) -> process.get
        case x =>
          x.dec() -> IO.unit
      }.flatten
    }

    def release: IO[Unit] = state.modify {
      _.pop()
    } >>= {
      _.fold(IO.unit) { _.complete(()) }
    }
  }
}
