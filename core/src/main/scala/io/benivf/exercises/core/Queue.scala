package io.benivf.exercises.core

import cats.effect._
import scala.collection.immutable.{Queue => SQueue}
import cats.effect.concurrent._
import cats.syntax.all._

trait Queue[A] {
  def put(a: A): IO[Unit]
  def tryPut(a: A): IO[Boolean]
  def take: IO[A]
  def tryTake: IO[Option[A]]
  def peek: IO[Option[A]]
}
object Queue {
  def apply[A](length: Int): IO[Queue[A]] =
    Ref.of[IO, State[A]](State.empty).map(new Impl(length, _))

  private[this] final case class State[A](
      size: Int,
      queue: SQueue[A],
      producers: SQueue[(A, Deferred[IO, Unit])],
      consumers: SQueue[Deferred[IO, A]]
  ) {
    def hasPendingConsumers: Boolean = consumers.nonEmpty
    def hasPendingProducers: Boolean = producers.nonEmpty
    def nonEmpty: Boolean = queue.nonEmpty

    def headOption: (State[A], Option[A]) =
      queue.headOption.fold(this -> none[A]) { case h =>
        State(
          size - 1,
          queue.tail,
          producers,
          consumers
        ) -> h.some
      }

    def addPendingProducer(producer: (A, Deferred[IO, Unit])): State[A] =
      State(
        size,
        queue,
        producers.enqueue(producer),
        consumers
      )

    def nextConsumer: (State[A], Deferred[IO, A]) = {
      val (consumer, newConsumers) = consumers.dequeue
      State(size, queue, producers, newConsumers) -> consumer
    }

    def nextProducer: (State[A], (A, Deferred[IO, Unit])) = {
      val (producer, newProducers) = producers.dequeue
      State(size, queue, newProducers, consumers) -> producer
    }

    def enqueue(a: A): State[A] =
      State(
        size + 1,
        queue.enqueue(a),
        producers,
        consumers
      )

    def dequeue(): (State[A], A) = {
      val (a, newQueue) = queue.dequeue
      State(
        size - 1,
        newQueue,
        producers,
        consumers
      ) -> a
    }

    def addPendingConsumer(consumer: Deferred[IO, A]): State[A] =
      State(
        size,
        queue,
        producers,
        consumers.enqueue(consumer)
      )
  }

  private[this] object State {
    def empty[A]: State[A] =
      State(0, SQueue.empty, SQueue.empty, SQueue.empty)
  }

  private[this] final class Impl[A](total: Int, state: Ref[IO, State[A]])
      extends Queue[A] {
    def put(a: A): IO[Unit] = Deferred[IO, Unit] >>= { producer =>
      state.modify {
        case s if s.hasPendingConsumers =>
          s.nextConsumer.map(_.complete(a))
        case s if s.size < total =>
          s.enqueue(a) -> IO.unit
        case s =>
          s.addPendingProducer(a -> producer) -> IO.unit
      }.flatten
    }

    def tryPut(a: A): IO[Boolean] =
      state.modify {
        case s if s.hasPendingConsumers =>
          s.nextConsumer.map(_.complete(a).as(true))
        case s if s.size < total =>
          s.enqueue(a) -> true.pure[IO]
        case s =>
          s -> false.pure[IO]
      }.flatten

    def take: IO[A] = Deferred[IO, A] >>= { consumer =>
      state.modify {
        case s if s.hasPendingProducers =>
          s.nextProducer.map { case (a, producer) =>
            producer.complete(()).as(a)
          }
        case s if s.queue.nonEmpty =>
          s.dequeue().map(_.pure[IO])
        case s =>
          s.addPendingConsumer(consumer) -> consumer.get
      }.flatten
    }

    def tryTake: IO[Option[A]] = state.modify {
      case s if s.hasPendingProducers =>
        s.nextProducer.map { case (a, producer) =>
          producer.complete(()).as(a.some)
        }
      case s if s.size > 0 =>
        s.dequeue().map(_.some.pure[IO])
      case s =>
        s -> none[A].pure[IO]
    }.flatten

    def peek: IO[Option[A]] = state modify {
      _.headOption
    }
  }
}
