package io.benivf.exercises

import cats.syntax.all._

import scala.concurrent.duration._
import java.util.concurrent.TimeoutException
import cats.effect.kernel._

object exercises {

  object io {
    import cats.effect.IO
    def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] =
      IO.race(
        IO.sleep(duration)
          .as(
            new TimeoutException(
              s"Unable to execute io because timeout $duration has reached!"
            )
          ),
        io
      ) >>= (IO.fromEither)
  }

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

}
