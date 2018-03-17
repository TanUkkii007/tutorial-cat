package com.danielasfregola.tutorial.cat.applicative

import com.danielasfregola.tutorial.cat._

// See solution at https://gist.github.com/DanielaSfregola/ddf48f6c5638f6284b563798c55d5ebd

object ApplicativeInstances {

  implicit val maybeApplicative: Applicative[Maybe] = new Applicative[Maybe] {

    override def pure[A](a: A) = Just(a)

    override def ap[A, B](boxF: Maybe[(A) => B])(boxA: Maybe[A]): Maybe[B] = boxF match {
      case Just(f) => boxA match {
        case Just(a) => Just(f(a))
        case Empty => Empty
      }
      case Empty => Empty
    }

  }

  implicit val zeroOrMoreApplicative: Applicative[ZeroOrMore] = new Applicative[ZeroOrMore] {
    override def pure[A](a: A) = OneOrMore(a, Zero)

    override def ap[A, B](boxF: ZeroOrMore[(A) => B])(boxA: ZeroOrMore[A]) = boxF match {
      case Zero => Zero
      case OneOrMore(headF, tailF) =>
        boxA match {
          case Zero => Zero
          case OneOrMore(headA, tailA) => OneOrMore(headF(headA), ap(tailF)(tailA))
        }
    }
  }

}
