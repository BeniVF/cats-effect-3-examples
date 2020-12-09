package io.benivf.exercises

import cats.syntax.all._

import scala.concurrent.duration._
import java.util.concurrent.TimeoutException
import cats.effect.kernel._
import java.time.Instant
import cats.Show

object exercises {

  object io {
    import cats.effect.IO
    import cats.effect.concurrent.Ref
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
      IO.race(
        IO.sleep(duration)
          .as(
            new TimeoutException(
              s"Unable to execute io because timeout $duration has reached!"
            )
          ),
        io
      ) >>= (IO.fromEither)

    def parTraverse[A, B](as: List[A])(f: A => IO[B]): IO[List[B]] =
      as.foldRight(IO.pure(List.empty[B])) { case (n, acc) =>
        IO.both(acc, f(n)).map { case (xs, x) => xs.+:(x) }
      }

    trait Semaphore {
      def acquire: IO[Unit]
      def release: IO[Unit]
    }
    object Semaphore {
      def apply(permits: Int): IO[Semaphore] = Ref
        .of[IO, Int](permits)
        .map(ref =>
          new Semaphore {
            def acquire: IO[Unit] = ref
              .modify[Boolean](x => if (x == 0) (x, false) else (x - 1, true))
              .ifM(
                IO.unit,
                acquire
              )
            def release: IO[Unit] = ref.update(_ + 1)
          }
        )
    }

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

  def parTraverse[F[_]: Concurrent[*[_], Throwable], A, B](as: List[A])(
      f: A => F[B]
  ): F[List[B]] =
    as.foldRight(List.empty[B].pure[F]) { case (n, acc) =>
      Concurrent[F, Throwable].both(acc, f(n)).map { case (xs, x) => xs.+:(x) }
    }

}
