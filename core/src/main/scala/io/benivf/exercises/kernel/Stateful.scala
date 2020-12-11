package io.benivf.exercises.kernel

import cats.effect.concurrent.Ref
import cats.effect.kernel.Async
import cats.syntax.all._

trait Stateful[F[_], S] {
  def get: F[S]
  def set(s: S): F[Unit]
}

object Stateful {

  def apply[F[_], S](implicit evidence: Stateful[F, S]): Stateful[F, S] = evidence

  def build[F[_]: Async, S](s: S): F[Stateful[F, S]] =
    Ref.of[F, S](s).map(Stateful.from)

  private def from[F[_], S](ref: Ref[F, S]): Stateful[F, S] = new Impl(ref)

  private final class Impl[F[_], S](ref: Ref[F, S]) extends Stateful[F, S] {
    def get: F[S] = ref.get

    def set(s: S): F[Unit] = ref.set(s)
  }
}
