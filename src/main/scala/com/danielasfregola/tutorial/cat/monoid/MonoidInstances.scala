package com.danielasfregola.tutorial.cat.monoid

// See solution at https://gist.github.com/DanielaSfregola/ddf48f6c5638f6284b563798c55d5ebd

object MonoidInstances {

  implicit val intMonoid: Monoid[Int] = new Monoid[Int] {

    override def compose(x: Int, y: Int) = x + y

    override def identity = 0
  }

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {

    override def compose(x: String, y: String) = x + y

    override def identity = ""
  }

}
