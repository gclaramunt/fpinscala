package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Branch(t1,t2) => size(t1)+size(t2)+1
    case Leaf(_) => 1
  }

  def maximun(t: Tree[Int]): Int = t match {
    case Branch(t1,t2) => maximun(t1) max maximun(t2)
    case Leaf(l) => l
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Branch(t1,t2) => 1 + depth(t1) max depth(t2)
    case Leaf(l) => 1
  }

  def map[A,B](t: Tree[A])(f:A => B): Tree[B] = t match {
    case Branch(t1,t2) => Branch(map(t1)(f),map(t2)(f))
    case Leaf(l) =>  Leaf(f(l))
  }

  def fold[A,B](t: Tree[A], lf:A=>B )(bf:(B,B) => B): B = t match {
    case Branch(t1,t2) => bf(fold(t1,lf)(bf),fold(t2,lf)(bf))
    case Leaf(l) =>  lf(l)
  }

}

