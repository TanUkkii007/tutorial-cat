package com.danielasfregola.tutorial.cat.monad

import com.danielasfregola.tutorial.cat._
import com.danielasfregola.tutorial.cat.applicative.ApplicativeInstances._

// See solution at https://gist.github.com/DanielaSfregola/ddf48f6c5638f6284b563798c55d5ebd

object MonadInstances {

  implicit val maybeMonad: Monad[Maybe] = new Monad[Maybe] {
    override def flatMap[A, B](boxA: Maybe[A])(f: (A) => Maybe[B]): Maybe[B] = boxA match {
      case Just(a) =>
        f(a)
      case Empty => Empty
    }

    override def pure[A](a: A) = Just(a)
  }

  implicit val zeroOrMoreMonad: Monad[ZeroOrMore] = new Monad[ZeroOrMore] {
    override def flatMap[A, B](boxA: ZeroOrMore[A])(f: (A) => ZeroOrMore[B]) = boxA match {
      case Zero => Zero
      case OneOrMore(headA, tailA) => f(headA).append(flatMap(tailA)(f))

    }

    override def pure[A](a: A) = OneOrMore(a, Zero)
  }

}
