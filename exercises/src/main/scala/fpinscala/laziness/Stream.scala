package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }


  def take(n: Int): Stream[A] = this match {
    case Empty => Empty 
    case Cons(h,t)  =>  
      if (n == 0 ) Empty
      else Cons(h, ()=>t().take(n-1)) 
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty 
    case Cons(h,t)  => 
      if (n == 0) this
      else t().drop(n-1)
  }
  
  def takeWhile(p: A => Boolean): Stream[A] = foldRight(empty[A])((x,acc) => if (p(x)) Cons( () =>x, () => acc) else Empty ) 
 
  def forAll(p: A => Boolean): Boolean = foldRight(true)( (a,b) => p(a) && b )  

  def headOption: Option[A] = foldRight(Option.empty[A])( (a,b) => Some(a)) 

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f:A=>B):Stream[B] = foldRight(empty[B])( (x,xs) => Cons(() => f(x), () => xs ))

  def filter(p:A=>Boolean):Stream[A] = foldRight(empty[A])( (x,xs) => if (p(x)) Cons(() => x, () => xs ) else xs )

  def append[AA >:A](that: => Stream[AA]):Stream[AA] = foldRight(that)( (x,xs) => Cons(() => x, () => xs ))

  def flatMap[B](f:A=>Stream[B]):Stream[B] = foldRight(empty[B])( (x,xs) => xs.append(f(x)))
 
  def startsWith[B](that: Stream[B]): Boolean = (this,that)  match {
    case (Cons(h1,t1),Cons(h2,t2)) if h1() == h2() => t1().startsWith(t2())
    case (_, Empty) => true
    case _ => false
  }

  def toList: List[A] = foldRight(List.empty[A])(_::_)

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](e:A): Stream[A] = cons(e,constant(e))

  def from(n: Int): Stream[Int] = cons(n,from(n+1))


  val fibs = {
    def fibAux(a:Int,b:Int): Stream[Int] = cons(a, fibAux(b,a+b))
    fibAux(0,1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = 
    f(z).map { case (a,newz) => Cons( () => a, () => unfold(newz)(f))}.getOrElse(empty)


  def constant1[A](e:A): Stream[A] = unfold(e)(s => Some(e,s))   

  def from1(n:Int): Stream[Int] = unfold(n)(s => Some(s,s+1))

  val fibs1 = unfold((0,1)) { case (x,y) => Some(x,(y,x+y)) }

  def map[A,B](s: Stream[A])(f: A => B): Stream[B] = unfold(s) { 
    case Cons(h,t) => Some(f(h()),t())
    case Empty => None 
  }

  def take[A](s: Stream[A],n: Int): Stream[A] = unfold((s,n)) { 
    case (_,0) => None
    case (Empty,_) => None
    case (Cons(a,st), c) => Some(a(),(st(),c-1))  
  }

  def takeWhile[A,B](s: Stream[A])(p: A => Boolean): Stream[A] = unfold((s,true)) { 
    case (_,false)  => None
    case (Empty,true) => None
    case (Cons(a,st), true) => Some((a(),(st(),p(a()))))  
  }

  def zipWith[A,B,C](s1: Stream[A],s2: Stream[B])(f: (A,B) => C): Stream[C] = unfold((s1,s2)) { 
    case (Cons(a,as), Cons(b,bs)) => Some(f(a(),b()),(as(),bs()))  
    case _ => None
  }

  //safeTail aux method for zip all
  def safeTail[A](s: Stream[A]) = s match {
    case Empty => Empty
    case Cons(_,ts) => ts() 
  }

  def zipAll[A,B](s1: Stream[A],s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold((s1,s2)) { 
    case (Empty,Empty)  => None
    case _  => Some((s1.headOption,s2.headOption), (safeTail(s1), safeTail(s2))) 
  }

  def startsWith[A](s1: Stream[A],s2: Stream[A]): Boolean = zipWith(s1,s2)(_==_).forAll(x=> x)  

}
