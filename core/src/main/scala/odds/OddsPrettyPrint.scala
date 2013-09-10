package ch.epfl.lamp.odds

/** Functions for pretty printing. */
trait OddsPrettyPrint {
  this: DistIntf =>

  def pp[A](dist: Dist[A], normalize: Boolean) = {
    val d = if (normalize) this.normalize(dist) else dist
    val dSorted = d.toList.sortBy{ case (x, p) => (-p, x.toString) }
    (dSorted map {
      case (x, p) => x + " : " + p
    }).mkString("\n")
  }

  def show[A](dist: Dist[A], desc: String = "", normalize: Boolean = false) = {
    println(desc)
    println(pp(dist, normalize))
    println("")
  }
}
