package ch.epfl.lamp.odds

import org.scalatest.FlatSpecLike

import inference._

// Translated from
// http://okmij.org/ftp/kakuritu/index.html#music
// http://okmij.org/ftp/kakuritu/music2.ml

trait CMusicModel extends CListOddsLang with Notes {

  import Rand.ToScalaMonadic

  val octave = List(
    A, Asharp, B, C, Csharp, D, Dsharp, E, F, Fsharp, G, Gsharp,
    A, Asharp, B, C, Csharp, D, Dsharp, E, F, Fsharp, G, Gsharp,
    A, Asharp, B, C, Csharp, D, Dsharp, E, F, Fsharp, G, Gsharp)

  def note_to_int(x: Note) = x match {
    case A      => 0
    case Asharp => 1
    case B      => 2
    case C      => 3
    case Csharp => 4
    case D      => 5
    case Dsharp => 6
    case E      => 7
    case F      => 8
    case Fsharp => 9
    case G      => 10
    case Gsharp => 11
  }

  def transpose(probs: List[Double], n: Int): Note => Rand[Note] = {
    val arr = for (i <- 0 to 11) yield (octave.drop(n+i).zip(probs))
    (x: Note) => choice(arr(note_to_int(x)) : _*)
  }

  // pre-compute the transformations
  val transpose2 = transpose(List(0.4, 0.5, 0.1), 1)
  val transpose3 = transpose(List(0.1, 0.4, 0.4, 0.1), 2)
  val transpose4 = transpose(List(0.1, 0.5, 0.3, 0.1), 4)
  val transpose5 = transpose(List(0.1, 0.3, 0.5, 0.1), 5)
  val transpose6 = transpose(List(0.1, 0.4, 0.4, 0.1), 7)
  val transpose7 = transpose(List(0.1, 0.5, 0.4), 9)

  def maptranspose(t: Note => Rand[Note]): PTransform[Note] =
    l => lmap(t)(l).flatMap(transform)
  // the main transformation
  def maptranspose2 = maptranspose(transpose2)
  def maptranspose3 = maptranspose(transpose3)
  def maptranspose4 = maptranspose(transpose4)
  def maptranspose5 = maptranspose(transpose5)
  def maptranspose6 = maptranspose(transpose6)
  def maptranspose7 = maptranspose(transpose7)
  val id: PTransform[Note] = x => always(x)
  val delete: PTransform[Note] = x => nil
  def choose_op() =
    choice(
      id -> 0.4,
      delete -> 0.2,
      maptranspose2 -> 0.1,
      maptranspose3 -> 0.05,
      maptranspose4 -> 0.05,
      maptranspose5 -> 0.05,
      maptranspose6 -> 0.05,
      maptranspose7 -> 0.1)
  def transform(x: CList[Note]): Rand[CList[Note]] = x match {
    case CNil => nil
    case CCons(head, tail) =>
      for (
        input <- always(x);
        f1 <- choose_op();
        f2 <- choose_op();
        s <- input.uniformSplit;
        r <- lappend(f1(s._1), f2(s._2)))
      yield r
  }

  def driver(src: List[Note], dst: List[Note]) = {
    lobserve(transform(asCList(src)), dst)
  }
  def main_simple = driver(List(A, B, C), List(Asharp, C))
  def main = driver(List(E, A, C, E, A, C, B, A, Gsharp, A),
                    List(E, D, C, B, A, B))
}

class CMusicModelLocalImportanceSamplingTest
    extends CMusicModel
    with LocalImportanceSampling
    with OddsPrettyPrint
    with FlatSpecLike {

  behavior of "MusicModel with local important sampling"

  it should "show the results of local-important sampled main simple" in {
    val r = sample(10, depth = 1, initDepth = 5, error = 0.0)(main_simple)
    show(r, "local-important sampled main simple")
  }

  it should "show the results of local-important sampled main" in {
    val r = sample(10000, depth = 2, initDepth = 5, error = 0.0)(main)
    show(r, "local-important sampled main")
  }
}

class CMusicModelTreeGenTest
    extends CMusicModel
    with DelayedChoiceTreeGen
    with OddsPrettyPrint
    with FlatSpecLike {

  behavior of "MusicModel with local important sampling"

  it should "show the first 3 layers of the main_simple tree" in {
    val t = reify(main_simple, Some(3))
    println("\n\ntree for main_simple:\n\n" + t.mkTikzString)
  }

  it should "show the first 3 layers of the main tree" in {
    val t = reify(main, Some(3))
    println("\n\ntree for main:\n\n" + t.mkTikzString)
  }
}
