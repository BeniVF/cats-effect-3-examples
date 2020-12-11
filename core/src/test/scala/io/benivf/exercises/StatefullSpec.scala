package io.benivf.exercises

import cats.effect._
import cats.syntax.all._
import io.benivf.exercises.kernel.Stateful

class StatefulSpec extends munit.FunSuite {
  implicit val x = unsafe.IORuntime.global

  test("it should get the init state") {
    val init = 10
    withStateful(init) {
      _.get.map(assertEquals(_, init))
    }
  }

  test("it should be able set and get") {
    val init = 10
    withStateful(init) { s =>
      val expected = 100
      s.set(expected) >>
        s.get.map(assertEquals(_, expected)) >>
        s.set(init) >>
        s.get.map(assertEquals(_, init))
    }
  }

  def withStateful[S](init: S)(f: Stateful[IO, S] => IO[Unit]): Unit =
    (Stateful.build[IO, S](init) >>= (f)).unsafeRunSync()
}
