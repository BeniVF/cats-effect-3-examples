package io.benivf.exercises

import cats.effect._
import cats.syntax.all._

class QueueSpec extends munit.FunSuite {
  import core._
  implicit val x = unsafe.IORuntime.global
  test(
    "it should be able to put and take"
  ) {
    (Queue[String](1) >>= { queue =>
      val expected = "Hello"
      queue.put(expected) >>
        queue.take.map { actual =>
          assertEquals(expected, actual, s"$actual === $expected")
        }
    }).unsafeRunSync()
  }

  test(
    "it should be able to put and take multiple elements when the queue size is 1"
  ) {
    (Queue[String](1) >>= { queue =>
      val expected = (1 to 10).toList.map(_.toString())
      expected.traverse { queue.put } >>
        expected.traverse(_ => queue.take).map { actual =>
          assertEquals(
            expected.toSet,
            actual.toSet,
            s"${actual} === $expected"
          ) // Needs to fix this
        }
    }).unsafeRunSync()
  }

  test(
    "it should be able to put and take multiple elements queue size is the same as the elements"
  ) {
    (Queue[String](10) >>= { queue =>
      val expected = (1 to 10).toList.map(_.toString())
      expected.traverse { queue.put } >>
        expected.traverse(_ => queue.take).map { actual =>
          assertEquals(expected, actual, s"${actual} === $expected")
        }
    }).unsafeRunSync()
  }

  test(
    "it should try put when the queue is not full"
  ) {
    (Queue[String](1) >>= { queue =>
      val expected = "foo"
      queue.tryPut(expected).map(assert(_, "it should be able to put")) >>
        queue.take.map(actual =>
          assertEquals(expected, actual, s"$expected === $actual")
        )
    }).unsafeRunSync()
  }

  test(
    "it should try put when the queue is full"
  ) {
    (Queue[String](1) >>= { queue =>
      queue.put("foo") >>
        queue
          .tryPut("boo")
          .map(x => assert(!x, s"expected `false` because queue is full"))
    }).unsafeRunSync()
  }

  test(
    "it should try take when the queue is empty"
  ) {
    (Queue[String](1) >>= { queue =>
      queue.tryTake.map(x =>
        assert(x.isEmpty, "it should not be elements in the queue")
      )
    }).unsafeRunSync()
  }

  test(
    "it should try take when the queue is not empty"
  ) {
    (Queue[String](1) >>= { queue =>
      val expected = "foo"
      queue.put(expected) >>
        queue.tryTake.map(actual =>
          assertEquals(expected.some, actual, s"$expected === $actual")
        )
    }).unsafeRunSync()
  }

  test(
    "it should peek when the queue has multiple elements"
  ) {
    (Queue[String](1) >>= { queue =>
      val expected = (1 to 10).toList.map(_.toString())
      expected.traverse(queue.put) >>
        queue.peek.map(actual =>
          assertEquals(expected.headOption, actual, s"$expected === $actual")
        )
    }).unsafeRunSync()
  }

}
