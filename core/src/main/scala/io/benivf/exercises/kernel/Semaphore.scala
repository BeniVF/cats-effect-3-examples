package io.benivf.exercises.kernel

import cats.effect._
import cats.effect.concurrent._
import cats.syntax.all._
import cats.Applicative

trait Semaphore[F[_]] {
  def acquire: F[Unit]
  def release: F[Unit]
}
object Semaphore {

  def apply[F[_]: Async](permits: Int): F[Semaphore[F]] = Ref
    .of[F, State[F]](State(permits))
    .map(new Impl[F](_))

  private[this] final case class State[F[_]](
      count: Int,
      waiting: List[Deferred[F, Unit]]
  ) {
    def wait(process: Deferred[F, Unit]): State[F] =
      State(count, waiting.+:(process))
    def dec(): State[F] = State(count - 1, waiting)
    private def inc: State[F] = State(count + 1, waiting)
    def pop(): (State[F], Option[Deferred[F, Unit]]) =
      if (waiting.nonEmpty)
        (State(count, waiting.drop(1)), waiting.headOption)
      else
        (this.inc, None)
  }

  private[this] object State {
    def apply[F[_]](permits: Int): State[F] = State(permits, List.empty)
  }

  private[this] final class Impl[F[_]: Async](state: Ref[F, State[F]])
      extends Semaphore[F] {
    def acquire: F[Unit] = Deferred[F, Unit] >>= { process =>
      state.modify {
        case x if (x.count == 0) =>
          x.wait(process) -> process.get
        case x =>
          x.dec() -> Applicative[F].unit
      }.flatten
    }

    def release: F[Unit] = state.modify {
      _.pop()
    } >>= {
      _.fold(Applicative[F].unit) { _.complete(()) }
    }
  }
}
