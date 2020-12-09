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
    import cats.effect.concurrent._
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
      def apply(permits: Int): IO[Semaphore] =
        Ref
          .of[IO, State](State(permits))
          .map { state =>
            new Semaphore {
              def acquire: IO[Unit] =
                Deferred[IO, Unit] >>= { process =>
                  state
                    .modify { x =>
                      if (x.count == 0)
                        (x.wait(process), false)
                      else
                        (x.dec(), true)
                    }
                    .ifM(
                      process.complete(()),
                      process.get
                    )
                }

              def release: IO[Unit] = state.modify {
                _.pop()
              } >>= {
                _.fold(IO.unit) { _.complete(()) }
              }
            }
          }
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
