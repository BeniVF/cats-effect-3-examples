package io.benivf.exercises

import core._
import cats.effect._
import cats.syntax.all._
import scala.concurrent.duration._

class SemaphoreSpec extends munit.FunSuite {
  implicit val x = unsafe.IORuntime.global

  test("it should be able to acquire and release") {
    withSemaphore(1) { s =>
      val expected = "hello"
      s.acquire >>
        expected.pure[IO].flatTap(_ => s.release) >>= { actual =>
        assertEquals(
          expected,
          actual,
          "it should get here"
        ).pure[IO]
      }

    }
  }

  test(
    "it should be able to acquire and release multiple times when the semaphore allows more"
  ) {
    withSemaphore(10) { s =>
      val expected = "hello"
      (1 to 10).toList.traverse(_ => s.acquire) >>
        expected
          .pure[IO]
          .flatTap(_ => (1 to 10).toList.traverse(_ => s.release)) >>= {
        actual =>
          assertEquals(
            expected,
            actual,
            "it should get here"
          ).pure[IO]
      }

    }
  }

  test("it should block when it tries to acquire twice") {
    withSemaphore(1) { s =>
      val expected = "hello"
      val duration = 10.millis
      s.acquire >>
        s.acquire.timeout(duration).attempt.map { x =>
          assert(x.isLeft)
          assertEquals(
            x.left.getOrElse(sys.error("not possible")).getMessage(),
            duration.show
          )
        } >>
        expected.pure[IO].flatTap(_ => s.release) >>= { actual =>
        assertEquals(
          expected,
          actual,
          "it should get here"
        ).pure[IO]
      }
    }
  }

  test("it should be able to release with no effect") {
    withSemaphore(1) { s =>
      val expected = "foo"
      s.release >>
        s.release >>
        s.acquire >>
        expected.pure[IO].flatTap(_ => s.release) >>= { actual =>
        assertEquals(expected, actual).pure[IO]
      }
    }
  }

  def withSemaphore(size: Int)(f: Semaphore => IO[Unit]): Unit =
    (Semaphore(size) >>= (f)).unsafeRunSync()

}
