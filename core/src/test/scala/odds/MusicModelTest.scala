package ch.epfl.lamp.odds

import org.scalatest.FlatSpecLike

import inference._

// Translated from
// http://okmij.org/ftp/kakuritu/index.html#music
// http://okmij.org/ftp/kakuritu/music2.ml

trait ListOddsLang extends OddsLang {

  import Rand.ToScalaMonadic

  // Lazy Lists
  def infix_uniformSplit[A](s: List[A]): Rand[(List[A], List[A])] =
    for (i <- uniform(0 to s.length : _*)) yield s.splitAt(i)

  def nil[A]: Rand[List[A]] = always(Nil.toList)
  type PTransform[A] = List[A] => Rand[List[A]]
  def lmap[A](f: A => Rand[A]): PTransform[A] = x => {
    if (x.isEmpty) nil else
    lmap(f)(x.tail).flatMap(xs => f(x.head).map(h => h :: xs))
  }
}

trait MusicModel extends ListOddsLang with Notes {

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
    { (l: List[Note]) => (l.map(t): Rand[List[Note]]) } andThen
      transform

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
  def transform(x: List[Note]): Rand[List[Note]] =
    if (x.isEmpty) nil else {
      val f1 = choose_op();
      val f2 = choose_op();
      val s = x.uniformSplit;
      //f1(s._1) ::: f2(s._2)  // FIXME: SI-7777 ?
      val s1 = s._1
      val s2 = s._2
      val fs1 = f1(s1)
      val fs2 = f1(s2)
      fs1 ::: fs1
    }

  def driver(src: List[Note], dst: List[Note]) = {
    val output = transform(src)
    always(true) when (output == dst)
  }
  def main_simple = driver(List(A, B, C), List(Asharp, C))
  def main = driver(List(E, A, C, E, A, C, B, A, Gsharp, A),
                    List(E, D, C, B, A, B))
}

class MusicModelSampleTest
    extends MusicModel
    with RejectionSampling
    with OddsPrettyPrint
    with FlatSpecLike {

  behavior of "MusicModel with RejectionSampling"

  def run[A](msg: String, body: => Rand[A], samples: Int) {
    it should "show the results of sampling " + msg + " " + samples in {
      val r = sample(samples){body}
      show(r, "sampled " + msg + " " + samples)
    }
  }
  run("main simple", main_simple, 1000)
  run("main simple", main_simple, 10000)
  run("main", main, 1000)
  run("main", main, 10000)
  run("main", main, 100000)
}

class MusicModelDepthBoundTest
    extends MusicModel
    with DepthBoundInference
    with OddsPrettyPrint
    with FlatSpecLike {

  behavior of "MusicModel with depth bound Inference"

  it should "show the results of dept-bound inferring main simple" in {
    val (r, p) = reify(10000, error = 0.4)(main_simple)
    show(r, "depth-bound main simple, err < " + p)
  }
}

class MusicModelLocalImportanceSamplingTest
    extends MusicModel
    with LocalImportanceSampling
    with OddsPrettyPrint
    with FlatSpecLike {

  behavior of "MusicModel with local important sampling"

  it should "show the results of local-important sampled main simple" in {
    val r = sample(1000, depth = 1, initDepth = 5, error = 0.0)(main_simple)
    show(r, "local-important sampled main simple")
  }

  it should "show the results of local-important sampled main" in {
    val r = sample(1000, depth = 1, initDepth = 5, error = 0.0)(main)
    show(r, "local-important sampled main")
  }
}
