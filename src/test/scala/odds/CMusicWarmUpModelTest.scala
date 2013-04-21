package odds

import org.scalatest.FlatSpec

trait CListOddsLang extends OddsLang {
  // A list where the tail itself is a random var.
  sealed abstract class CList[+A] {
    def length: Rand[Int] = this match {
      case CNil => always(0)
      case CCons(hd, tl) => always(1)+tl.length
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

  def infix_length[A](rs: Rand[CList[A]]): Rand[Int] =
    for (s <- rs; n <- s.length) yield n
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
    case Nil => s.length===always(0)
    case hd::tl =>
      s flatMap {
        case CNil => always(false)
        case CCons(rhd, rtail) =>
          if (rhd==hd) lobserve(rtail, tl)
          else always(false)
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
    (x.length===always(3) && x.head===always(Asharp)) flatMap {
      case true => x.tail.head
      case false => never
    }
  }
}

class CMusicWarmUpModelExactTest
    extends CMusicWarmUpModel
    with ExactInference
    with OddsPrettyPrint
    with FlatSpec {

  behavior of "CMusicWarmUpModel with Exact Inference"

  it should "show the results of exactly inferring main" in {
    val r = normalize(reify(main))
    show(r, "exact main")
    expectResult(Map(
      G -> 0.002777777777777779,
      Fsharp -> 0.008333333333333337,
      F -> 0.008333333333333337,
      E -> 0.008333333333333337,
      C -> 0.5,
      Csharp -> 0.3333333333333333,
      B -> 0.13888888888888892))(r.toMap)
  }
}

class CMusicWarmUpModelLISTest
    extends CMusicWarmUpModel
    with LocalImportanceSampling
    with OddsPrettyPrint
    with FlatSpec {

  behavior of "CMusicWarmUpModel with LIS"

  it should "show the results of LIS inferring main" in {
    val r = sample(1000, 3)(main)
    show(r, "LIS exact main")
  }
}
