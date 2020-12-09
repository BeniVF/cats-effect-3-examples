package io.benivf.exercises

import java.util.concurrent.TimeoutException

import scala.concurrent.duration._

import cats.effect.kernel._
import cats.syntax.all._

package object kernel {

  def timeout[F[_]: Concurrent[*[_], Throwable]: Temporal[*[_], Throwable], A](
      fa: F[A],
      duration: FiniteDuration
  ): F[A] = Concurrent[F, Throwable].race(
    Temporal[F]
      .sleep(duration)
      .as(
        new TimeoutException(
          s"Unable to execute io because timeout $duration has reached!"
        )
      ),
    fa
  ) >>= (_.liftTo[F])

  def parTraverse[F[_]: Concurrent[*[_], Throwable], A, B](as: List[A])(
      f: A => F[B]
  ): F[List[B]] =
    as.foldRight(List.empty[B].pure[F]) { case (n, acc) =>
      Concurrent[F, Throwable].both(acc, f(n)).map { case (xs, x) => xs.+:(x) }
    }

}
