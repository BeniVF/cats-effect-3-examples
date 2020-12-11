package io.benivf.exercises.kernel

import scala.collection.immutable.{Queue => SQueue}

import cats.effect._
import cats.effect.concurrent._
import cats.syntax.all._
import cats.Applicative

trait Queue[F[_], A] {
  def put(a: A): F[Unit]
  def tryPut(a: A): F[Boolean]
  def take: F[A]
  def tryTake: F[Option[A]]
  def peek: F[Option[A]]
}
object Queue {
  def apply[F[_]: Async, A](length: Int): F[Queue[F, A]] =
    Ref.of[F, State[F, A]](State.empty).map(new Impl[F, A](length, _))

  private[this] final case class State[F[_], A](
      size: Int,
      queue: SQueue[A],
      producers: SQueue[(A, Deferred[F, Unit])],
      consumers: SQueue[Deferred[F, A]]
  ) {
    def hasPendingConsumers: Boolean = consumers.nonEmpty
    def hasPendingProducers: Boolean = producers.nonEmpty
    def nonEmpty: Boolean = queue.nonEmpty

    def headOptFn: (State[F, A], Option[A]) =
      queue.headOption.fold(this -> none[A]) { case h =>
        State(
          size - 1,
          queue.tail,
          producers,
          consumers
        ) -> h.some
      }

    def addPendingProducer(producer: (A, Deferred[F, Unit])): State[F, A] =
      State(
        size,
        queue,
        producers.enqueue(producer),
        consumers
      )

    def nextConsumer: (State[F, A], Deferred[F, A]) = {
      val (consumer, newConsumers) = consumers.dequeue
      State(size, queue, producers, newConsumers) -> consumer
    }

    def nextProducer: (State[F, A], (A, Deferred[F, Unit])) = {
      val (producer, newProducers) = producers.dequeue
      State(size, queue, newProducers, consumers) -> producer
    }

    def enqueue(a: A): State[F, A] =
      State(
        size + 1,
        queue.enqueue(a),
        producers,
        consumers
      )

    def dequeue(): (State[F, A], A) = {
      val (a, newQueue) = queue.dequeue
      State(
        size - 1,
        newQueue,
        producers,
        consumers
      ) -> a
    }

    def addPendingConsumer(consumer: Deferred[F, A]): State[F, A] =
      State(
        size,
        queue,
        producers,
        consumers.enqueue(consumer)
      )
  }

  private[this] object State {
    def empty[F[_], A]: State[F, A] =
      State(0, SQueue.empty, SQueue.empty, SQueue.empty)
  }

  private[this] final class Impl[F[_]: Async, A](
      total: Int,
      state: Ref[F, State[F, A]]
  ) extends Queue[F, A] {
    def put(a: A): F[Unit] = Deferred[F, Unit] >>= { producer =>
      state.modify {
        case s if s.size < total =>
          s.enqueue(a) -> Applicative[F].unit
        case s if s.hasPendingConsumers =>
          s.nextConsumer.map(_.complete(a))
        case s =>
          s.addPendingProducer(a -> producer) -> Applicative[F].unit
      }.flatten
    }

    def tryPut(a: A): F[Boolean] =
      state.modify {
        case s if s.size < total =>
          s.enqueue(a) -> true.pure[F]
        case s if s.hasPendingConsumers =>
          s.nextConsumer.map(_.complete(a).as(true))
        case s =>
          s -> false.pure[F]
      }.flatten

    def take: F[A] = Deferred[F, A] >>= { consumer =>
      state.modify {
        case s if s.hasPendingProducers && s.queue.nonEmpty =>
          val (s1, (a, producer)) = s.nextProducer
          val (s2, r) = s1.dequeue()
          s2.enqueue(a) -> producer.complete(()).as(r)
        case s if s.queue.nonEmpty =>
          s.dequeue().map(_.pure[F])
        case s if s.hasPendingConsumers =>
          s.nextProducer.map { case (a, producer) =>
            producer.complete(()).as(a)
          }
        case s =>
          s.addPendingConsumer(consumer) -> consumer.get
      }.flatten
    }

    def tryTake: F[Option[A]] = state.modify {
      case s if s.queue.nonEmpty =>
        s.dequeue().map(_.some.pure[F])
      case s if s.hasPendingProducers =>
        s.nextProducer.map { case (a, producer) =>
          producer.complete(()).as(a.some)
        }
      case s =>
        s -> none[A].pure[F]
    }.flatten

    def peek: F[Option[A]] = state modify {
      _.headOptFn
    }
  }
}
