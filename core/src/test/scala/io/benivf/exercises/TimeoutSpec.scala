package io.benivf.exercises

import scala.concurrent.duration._
import cats.effect._
import cats.syntax.all._
import java.util.concurrent.TimeoutException

class TimeoutSpec extends munit.FunSuite {
  import core._
  implicit val x = unsafe.IORuntime.global
  test(
    "it should get the result when the io executes before the timeout duration"
  ) {
    timeout(10.pure[IO], 10.millis)
      .map(assertEquals(_, 10, "it should not failed when timeout is not reached"))
      .unsafeRunSync()
  }

  test(
    "it should throw an exception when timeout duration is reached"
  ) {
    intercept[TimeoutException](
      timeout(IO.never, 10.millis).unsafeRunSync()
    )
  }

}
