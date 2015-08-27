package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) ={
    val (i,rng1) = rng.nextInt
    val r=  if (i == Int.MinValue) Int.MaxValue
            else math.abs(i)
    (r,rng1)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n,rng1) = rng.nextInt
    val d = n.toDouble / Int.MaxValue.toDouble
    (d,rng1)
  }

  //with map
  def double1(rng: RNG): (Double, RNG) = map(int)(_/ Int.MaxValue.toDouble)(rng)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i,rng1) = rng.nextInt
    val (d,rng2) = double(rng1)
    ((i,d),rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d,rng1) = double(rng) 
    val (i,rng2) = rng1.nextInt
    ((d,i),rng2)
  }

  //give me the monad already!
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1,rng1) = double(rng) 
    val (d2,rng2) = double(rng1) 
    val (d3,rng3) = double(rng2) 
    ((d1,d2,d3),rng3)
  }
  
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = 
    if (count ==0) (Nil, rng)
    else {
      val (i,rng1) = rng.nextInt
      val (is,rng2) = ints(count-1)(rng1)
      (i :: is, rng2) 
    }

  def ints1(count: Int)(rng: RNG): (List[Int], RNG) = 
    (1 to count).foldLeft((Nil:List[Int], rng))((acc,_) => { 
      val (xs,rng1) = acc
      val (x,rng2) = rng1.nextInt
      (x::xs,rng2)
    } )

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    rng => {
      val (a,rng1) = ra(rng)
      val (b,rng2) = rb(rng1)
      (f(a,b), rng2)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => fs.foldRight((Nil:List[A], rng))((rand,acc) => { 
      val (xs,rng1) = acc
      val (x,rng2) = rand(rng1)
      (x::xs,rng2)
    } )

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a,rng1)=f(rng)
    g(a)(rng1)
  }

  // adapted from the book
  def nonNegativeLessThan(n: Int): Rand[Int] = 
    flatMap(nonNegativeInt _ )( i => {
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
      })

  //using flatMap
  def map_1[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))
  def map2_1[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map_1(rb)(b => f(a,b)))

}

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State(
    s => {
      val (a,newS) = run(s)
      (f(a), newS)
    }
  )

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(
      s => {
        val (a,newS1) = run(s)
        val (b,newS2) = sb.run(newS1)
        (f(a,b), newS2)
      }
    )


  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(
      s => {
        val (a,newS1) = run(s)
        val (b,newS2) = f(a).run(newS1)
        (b, newS2)
      }
    )
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {

  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] = State(s => {
    val (as,finalS)=fs.foldLeft((Vector.empty[A], s))((acc,state) => {   
        val (xs,s1) = acc
        val (x,s2) = state.run(s1)
        (xs:+x,s2)
      } )
   (as.toList,finalS) 
  })

  type Rand[A] = State[RNG, A]

/*
 Inserting a coin into a locked machine will cause it to unlock if there’s any
 candy left.
 
 Turning the knob on an unlocked machine will cause it to dispense candy and
 become locked.
 
 Turning the knob on a locked machine or inserting a coin into an unlocked
 machine does nothing.
 
 A machine that’s out of candy ignores all inputs.
*/

  def transition(i:Input)(m:Machine): ((Int,Int),Machine) = i match {
        case Coin if m.locked && m.candies >0 => ((m.candies, m.coins+1),Machine(true, m.candies, m.coins+1))
        case Turn if !m.locked => ((m.candies-1, m.coins),Machine(false,m.candies-1,m.coins))
        case _ => ((m.candies, m.coins),Machine(false,m.candies,m.coins)) 
      }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = 
    inputs.foldLeft(State( (s:Machine) => ((10,1),Machine(true,10,1))))( (s,i) => s.flatMap(x => State(transition(i) _ )))
  
}
