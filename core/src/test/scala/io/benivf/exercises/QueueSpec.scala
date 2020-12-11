package io.benivf.exercises

import cats.effect._
import cats.effect.syntax.all._
import cats.syntax.all._
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._

class QueueSpec extends ScalaCheckSuite {
  import core._
  implicit val x = unsafe.IORuntime.global
  val positiveInt = Gen.choose(1, Int.MaxValue)

  property(
    "it should be able to put and take"
  ) {
    forAll(positiveInt, Gen.alphaStr) { (queueSize: Int, expected: String) =>
      (Queue[String](queueSize) >>= { queue =>
        queue.put(expected) >>
          queue.take.map { actual =>
            assertEquals(actual, expected, s"$actual === $expected")
          }
      }).unsafeRunSync()
    }
  }

  property(
    "it should be able to put and take multiple elements"
  ) {
    forAll(positiveInt, Gen.listOf[Int](positiveInt)) { (queueSize: Int, expected: List[Int]) =>
      (Queue[Int](queueSize) >>= { queue =>
        expected.traverse(queue.put) >>
          expected.traverse(_ => queue.take).map { actual =>
            assertEquals(
              actual,
              expected,
              s"${actual} === $expected"
            )
          }
      }).unsafeRunSync()
    }
  }

  property(
    "it should be able to put and take multiple elements concurrently"
  ) {
    val positiveInt = Gen.choose(1, 1000)
    forAll(positiveInt, Gen.listOf[Int](positiveInt)) { (queueSize: Int, expected: List[Int]) =>
      (Queue[Int](queueSize) >>= { queue =>
        IO.both(
          expected.traverse(queue.put(_)),
          expected.traverse(_ => IO.cede >> queue.take)
        ).map { case (_, actual) =>
          assertEquals(
            actual.sorted,
            expected.sorted,
            s"${actual} should contain same elements as $expected"
          )
        }
      }).unsafeRunSync()
    }
  }

  property(
    "it should try put when the queue is not full"
  ) {
    forAll(positiveInt, Gen.alphaStr) { (queueSize: Int, expected: String) =>
      (Queue[String](queueSize) >>= { queue =>
        queue.tryPut(expected).map(assert(_, "it should be able to put")) >>
          queue.take.map(actual => assertEquals(actual, expected, s"$expected === $actual"))
      }).unsafeRunSync()
    }
  }

  property(
    "it should try put when the queue is full"
  ) {
    val positiveInt = Gen.choose(1, 1000)
    forAll(positiveInt) { (queueSize: Int) =>
      (Queue[Int](queueSize) >>= { queue =>
        (1 to queueSize).toList.parTraverse { i =>
          queue.put(i)
        } >>
          queue
            .tryPut(queueSize + 1)
            .map(x => assert(!x, s"expected `false` because queue is full"))
      }).unsafeRunSync()
    }
  }

  property(
    "it should try take when the queue is empty"
  ) {
    forAll(positiveInt) { (queueSize: Int) =>
      (Queue[Float](queueSize) >>= { queue =>
        queue.tryTake.map(x => assert(x.isEmpty, "it should not be elements in the queue"))
      }).unsafeRunSync()
    }
  }

  property(
    "it should try take when the queue is not empty"
  ) {
    forAll(positiveInt, Gen.long) { (queueSize: Int, expected: Long) =>
      (Queue[Long](queueSize) >>= { queue =>
        queue.put(expected) >>
          queue.tryTake.map(actual => assertEquals(actual, expected.some, s"$expected === $actual"))
      }).unsafeRunSync()
    }
  }

  property(
    "it should peek when the queue has multiple elements"
  ) {
    forAll(positiveInt, Gen.listOf(positiveInt)) { (queueSize: Int, expected: List[Int]) =>
      (Queue[Int](queueSize) >>= { queue =>
        expected.parTraverse(queue.put) >>
          queue.peek.map(actual =>
            assertEquals(
              actual,
              expected.headOption,
              s"$expected === $actual"
            )
          )
      }).unsafeRunSync()
    }
  }

}
