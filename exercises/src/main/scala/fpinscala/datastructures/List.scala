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

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def sum3(l: List[Int]) = {
    foldLeft(l, 0)(_ + _)
  }

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def product3(l: List[Double]) =
    foldLeft(l, 1.0)(_ * _)

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("It cannot be applied to empty list!")
    case Cons(h, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(h, t) if n > 0 =>  drop(t, n - 1)
    case _ => l
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h)=> dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((x, z) => 1 + z)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((z, h) => z + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, e) => Cons(e, acc))

  def concatenate[A](lists: List[List[A]]): List[A] = lists match {
    case Nil => Nil
    case Cons(h, t) => foldRight(h, concatenate(t))(Cons(_, _))
  }

  def map_1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h, acc) => Cons(h + 1, acc))

  def map_2(d: List[Double]): List[String] =
    foldRight(d, Nil: List[String])((h, acc) => Cons(h.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, acc) => Cons(f(h), acc))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    def go(as: List[A]): Unit = as match {
      case Nil => ()
      case Cons(h, t) if f(h) => buf += h; go(t)
      case Cons(h, t) => go(t)
    }
    go(l)
    List(buf.toList: _*)
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil: List[B])((h, acc) => append(f(h), acc))

  def filter_2[A](l: List[A])(f: A => Boolean): List[A] = {
    val g = (a: A) => if(f(a)) List(a) else Nil
    flatMap(l)(g)
  }

  def zipWith_1[Int](l1: List[Int], l2: List[Int]): List[Int] = ???
  //    l1 match {
  //    case Nil => Nil
  //    case Cons(h1, t1) => l2 match {
  //      case Nil => Nil
  //      case Cons(h2, t2) => Cons(h1 + h2, zipWith_1(t1, t2))
  //    }
  //  }

}
