package odds

import org.scalatest.FlatSpec

trait MusicWarmUpModel extends OddsLang {
  // Lazy Lists
  def infix_uniformSplit[A](s: Stream[A]): Rand[(Stream[A], Stream[A])] =
    for (i <- uniform(0 to s.length : _*)) yield s.splitAt(i)
  def infix_tail[A](rs: Rand[Stream[A]]): Rand[Stream[A]] =
    for (s <- rs) yield s.tail
  def infix_head[A](rs: Rand[Stream[A]]): Rand[A] =
    for (s <- rs) yield s.head
  def infix_length[A](rs: Rand[Stream[A]]): Rand[Int] =
    for (s <- rs) yield s.length

  // Notes and Note Transformations

  sealed abstract class Note
  case object A extends Note
  case object Asharp extends Note
  case object B extends Note
  case object C extends Note
  case object Csharp extends Note
  case object D extends Note
  case object Dsharp extends Note
  case object E extends Note
  case object F extends Note
  case object Fsharp extends Note
  case object G extends Note
  case object Gsharp extends Note

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

  type PTransform[A] = Stream[A] => Rand[Stream[A]]
  def nil[A]: Rand[Stream[A]] = always(Nil.toStream)
  def lmap[A](f: A => Rand[A]): PTransform[A] = x => {
    if (x.isEmpty) nil else
    lmap(f)(x.tail).flatMap(xs => f(x.head).map(h => h #:: xs))
  }
  def lappend[A](xs: Rand[Stream[A]], ys: Rand[Stream[A]]): Rand[Stream[A]] =
    xs flatMap { x =>
      if (x.isEmpty) ys else
      lappend(always(x.tail), ys).map(zs => x.head #:: zs)
    }
  val f_ide: PTransform[Note] = x => always(x)
  val f_del: PTransform[Note] = x => nil
  val f_tr1: PTransform[Note] = lmap(transpose1)
  val f_tr5: PTransform[Note] = lmap(transpose5)
  val transform: PTransform[Note] = x => {
    if (x.isEmpty) nil else {
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
  }

  def main = {
    val input = List(A, B, C).toStream
    val x = transform(input)
    (x.length===always(3) && x.head===always(Asharp)) flatMap {
      case true => x.tail.head
      case false => never
    }
  }
}

class MusicWarmUpModelSampleTest
    extends MusicWarmUpModel
    with RejectionSampling
    with OddsPrettyPrint
    with FlatSpec {

  behavior of "MusicWarmUpModel with RejectionSampling"

  it should "show the results of sampling main" in {
    val r = sample(1000){main}
    show(r, "sampled main")
  }
}

class MusicWarmUpModelExactTest
    extends MusicWarmUpModel
    with ExactInference
    with OddsPrettyPrint
    with FlatSpec {

  behavior of "MusicWarmUpModel with Exact Inference"

  it should "show the results of exactly inferring main" in {
    val r = normalize(main.reify)
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
