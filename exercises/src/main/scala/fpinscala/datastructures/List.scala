package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_,xs) => xs
    case Nil  => sys.error("tail of empty list")
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_,xs) => Cons(h,xs)
    case Nil  => sys.error("setHead of empty list")
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_,xs) if n>0 => drop(xs,n-1)
    case Nil  => sys.error("drop of empty list")
    case _ => l 
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x,xs) if f(x) => dropWhile(xs,f)
    case _ => l 
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(x,Nil) => Nil
    case Cons(x,xs) => Cons(x,init(xs))
    case Nil  => Nil
  }

  def length[A](l: List[A]): Int = foldRight(l,0)((xs,length)=>length+1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x,xs) => foldLeft(xs,f(z,x))(f) 
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Cons(x,xs) => Cons(f(x), map(xs)(f))
    case Nil => Nil
  }

  // or with foldRight 
  def map1[A,B](l: List[A])(f: A => B): List[B] = foldRight(l,Nil:List[B])((x,xs)=>Cons(f(x),xs)) 

  // not signatures in file
  def reverse[A](l:List[A]):List[A]= foldLeft(l,Nil:List[A])( (xs,x) => Cons(x,xs))

  //foldRight based on fold left 
  def foldRight1[A,B](l:List[A],z:B)(f:(A,B)=>B):B= foldLeft(l,(x:B)=>x )((g,x)=>(a)=>g (f(x,a)))(z)  // see http://www.cs.nott.ac.uk/~gmh/fold.pdf  

  //foldLeft based on fold right
  def foldLeft1[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(l, (x: B) => x)((x, g) => a => g(f(a, x)))(z) //see http://www.cs.nott.ac.uk/~gmh/fold.pdf

  def append1[A](l1:List[A], l2:List[A]) = foldRight(l1,l2)(Cons(_,_))

  def flatten[A](ls:List[List[A]]):List[A] = foldLeft(ls,Nil:List[A])(append(_,_)) // O(...) ?

  def flatMap[A,B](ls:List[A])(f:A=>List[B]):List[B] = foldRight(ls,Nil:List[B])((a,acc)=>append(f(a), acc))

  // w/o map for pedagogical purposes
  def addOne(l:List[Int]):List[Int] = l match {
    case Cons(x,xs) => Cons(x+1, addOne(xs))
    case Nil => Nil
  }

  // w/o map for pedagogical purposes
  def toString(l:List[Double]):List[String] = l match {
    case Cons(x,xs) => Cons(x.toString, toString(xs))
    case Nil => Nil
  }

  //filter with flatMap
  def filter[A](l:List[A])(f:A=>Boolean):List[A] = flatMap(l)(x => if (f(x)) Cons(x,Nil) else Nil )

  //assumes l1.length == l2.length 
  def sum(l1: List[Int], l2: List[Int]):List[Int]= (l1,l2) match {
    case (Cons(e1,es1), Cons(e2,es2)) => Cons(e1+e2, sum(es1,es2))
    case (Nil,Nil) => Nil
    case _ => sys.error("lists of different length")
  }


  def zipWith[A,B,C](l1: List[A], l2: List[B])(f:(A,B)=>C):List[C]= (l1,l2) match {
    case (Cons(e1,es1), Cons(e2,es2)) => Cons(f(e1,e2), zipWith(es1,es2)(f))
    case (Nil,Nil) => Nil
    case _ => sys.error("lists of different length")
  }

  def hasSubsequence[A](list: List[A], subseq: List[A]): Boolean ={
    def hasSubsequenceRec[A](sup: List[A], sub: List[A]): Boolean = (sup,sub) match {
      case (Cons(sup1,sups1), Cons(sub1,subs1)) => (sup1 == sub1) && hasSubsequenceRec(sups1,subs1) || hasSubsequenceRec(sups1, subseq)
      case (Cons(_, _), Nil) => true   
      case (Nil, Cons(_, _)) => false 
      case (Nil,Nil) => true 
    }
    hasSubsequenceRec(list, subseq)
  }


}
