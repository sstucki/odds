package ch.epfl.lamp.odds

import org.scalatest.FlatSpecLike
import org.scalatest.Matchers

import inference._

trait CListOddsLang extends OddsLang {

  import Rand.ToScalaMonadic

  // A list where the tail itself is a random var.
  sealed abstract class CList[+A] {
    def length: Rand[Int] = this match {
      case CNil          => 0
      case CCons(hd, tl) => tl.length + 1
    }
    def isEmpty: Boolean = this match {
      case CNil          => true
      case CCons(hd, tl) => false
    }
    def splitAt(n: Int): Rand[(CList[A], CList[A])] = {
      if (n==0) always((CNil, this)) else this match {
        case CCons(head, tail) =>
          for (tl <- tail; (s1, s2) <- tl.splitAt(n-1))
          yield (CCons(head, always(s1)), s2)
      }
    }
    def uniformSplit: Rand[(CList[A], CList[A])] =
      for (l <- length; i <- uniform(0 to l: _*); r <- splitAt(i))
      yield r
  }
  case object CNil extends CList[Nothing]
  case class CCons[+A](head: A, taill: Rand[CList[A]]) extends CList[A]

  def infix_head[A](rs: Rand[CList[A]]): Rand[A] =
    for (s <- rs) yield (s match {
      case CCons(head, tail) => head
    })
  def infix_tail[A](rs: Rand[CList[A]]): Rand[CList[A]] =
    for (s <- rs; tl <- s match {
      case CCons(head, tail) => tail
    }) yield tl
  def nil[A]: Rand[CList[A]] = always(CNil)
  type PTransform[A] = CList[A] => Rand[CList[A]]
  def lmap[A](f: A => Rand[A]): PTransform[A] = x => x match {
    case CNil => nil
    case CCons(head, tail) =>
      for (hd <- f(head); tl <- tail) yield CCons(hd, lmap(f)(tl))
  }
  def lappend[A](xs: Rand[CList[A]], ys: Rand[CList[A]]): Rand[CList[A]] = xs flatMap {
    case CNil => ys
    case CCons(head, tail) => always(CCons(head, lappend(tail, ys)))
  }
  def lobserve[A](s: Rand[CList[A]], l: List[A]): Rand[Boolean] = l match {
    case Nil    => true when s.isEmpty
    case hd::tl =>
      s flatMap {
        case CNil => never //always(false)
        case CCons(rhd, rtail) =>
          if (rhd == hd) lobserve(rtail, tl)
          else never //always(false)
      }
  }
  def asCList[A](x: List[A]): CList[A] = x match {
    case Nil => CNil
    case x::xs => CCons(x, always(asCList(xs)))
  }
  def asLists[A](x: Rand[CList[A]]): Rand[List[A]] = x flatMap {
    case CNil => always(Nil)
    case CCons(x, xs) => asLists(xs).map(xs=>x::xs)
  }
}

// Translated from
// http://okmij.org/ftp/kakuritu/index.html#music
// http://okmij.org/ftp/kakuritu/music1a.ml

trait CMusicWarmUpModel extends CListOddsLang with Notes {

  import Rand.ToScalaMonadic

  // Note Transformations

  // Transpose a note by 1 interval
  def transpose1(n: Note) = n match {
    case C      => choice(Csharp -> 0.3, D -> 0.6, Dsharp -> 0.1)
    case Csharp => choice(D -> 0.4, Dsharp -> 0.6)
    case D      => choice(Dsharp -> 0.3, E -> 0.7)
    case Dsharp => choice(E -> 0.7, F -> 0.3)
    case E      => choice(F -> 0.6, Fsharp -> 0.4)
    case F      => choice(Fsharp -> 0.3, G -> 0.6, Gsharp -> 0.1)
    case Fsharp => choice(G -> 0.4, Gsharp -> 0.6)
    case G      => choice(Gsharp -> 0.3, A -> 0.6, Asharp -> 0.1)
    case Gsharp => choice(A -> 0.4, Asharp -> 0.6)
    case A      => choice(Asharp -> 0.3, B -> 0.7)
    case Asharp => choice(B -> 0.7, C -> 0.3)
    case B      => choice(C -> 0.6, Csharp -> 0.4)
  }

  // Transpose a note by 5 intervals
  def transpose5(n: Note) = n match {
    case C      => choice(F -> 0.3, Fsharp -> 0.1, G -> 0.55, Gsharp -> 0.05)
    case Csharp => choice(Fsharp -> 0.3, G -> 0.4, Gsharp -> 0.3)
    case D      => choice(G -> 0.3, Gsharp -> 0.1, A -> 0.55, Asharp -> 0.05)
    case Dsharp => choice(Gsharp -> 0.3, A -> 0.4, Asharp -> 0.3)
    case E      => choice(A -> 0.3, Asharp -> 0.1, B -> 0.55, C -> 0.05)
    case F      => choice(Asharp -> 0.1, B -> 0.2, C -> 0.6, Csharp -> 0.1)
    case Fsharp => choice(B -> 0.3, C -> 0.4, Csharp -> 0.3)
    case G      => choice(C -> 0.3, Csharp -> 0.1, D -> 0.55, Dsharp -> 0.05)
    case Gsharp => choice(Csharp -> 0.3, D -> 0.4, Dsharp -> 0.3)
    case A      => choice(D -> 0.3, Dsharp -> 0.1, E -> 0.55, F -> 0.05)
    case Asharp => choice(Dsharp -> 0.3, E -> 0.3, F -> 0.4)
    case B      => choice(E -> 0.3, F -> 0.3, Fsharp -> 0.3, G -> 0.1)
  }

  val f_ide: PTransform[Note] = x => always(x)
  val f_del: PTransform[Note] = x => nil
  val f_tr1: PTransform[Note] = lmap(transpose1)
  val f_tr5: PTransform[Note] = lmap(transpose5)
  val transform: PTransform[Note] = x => x match {
    case CNil => nil
    case CCons(headd, tail) =>
      for (
        input <- always(x);
        f1 <- choice(
          f_ide -> 0.5,
          f_del -> 0.2,
          f_tr1 -> 0.2,
          f_tr5 -> 0.1);
        f2 <- choice(
          f_ide -> 0.5,
          f_del -> 0.2,
          f_tr1 -> 0.2,
          f_tr5 -> 0.1);
        s <- input.uniformSplit;
        r <- lappend(f1(s._1), f2(s._2)))
      yield r
  }

  def main = {
    val input = asCList(List(A, B, C))
    val x = transform(input)
    x.tail.head when (x.length == 3 && x.head == Asharp)
  }
}

class CMusicWarmUpModelExactTest
    extends CMusicWarmUpModel
    with ExactInference
    with OddsPrettyPrint
    with FlatSpecLike
    with Matchers {

  behavior of "CMusicWarmUpModel with Exact Inference"

  it should "show the results of exactly inferring main" in {
    val r = normalize(reify(main))
    show(r, "exact main")
    r.toMap foreach {
      case (G, p)      => p should be (0.002777777 plusOrMinus 1e-9)
      case (Fsharp, p) => p should be (0.008333333 plusOrMinus 1e-9)
      case (F, p)      => p should be (0.008333333 plusOrMinus 1e-9)
      case (E, p)      => p should be (0.008333333 plusOrMinus 1e-9)
      case (C, p)      => p should be (0.5         plusOrMinus 1e-9)
      case (Csharp, p) => p should be (0.333333333 plusOrMinus 1e-9)
      case (B, p)      => p should be (0.138888888 plusOrMinus 1e-9)
      case x => throw new Exception("unexpected note: " + x)
    }
  }
}

class CMusicWarmUpModelLISTest
    extends CMusicWarmUpModel
    with LocalImportanceSampling
    with OddsPrettyPrint
    with FlatSpecLike {

  behavior of "CMusicWarmUpModel with LIS"

  it should "show the results of LIS inferring main" in {
    val r = sample(1000, 3)(main)
    show(r, "LIS exact main")
  }
}
