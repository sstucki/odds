package odds
/*
trait OddsCoreLazy extends OddsIntf {

  import OddsCore.{Dist, CommittedChoice}

  type Rand[+A] = RandVar[A]

  abstract class RandVar[+A] extends RandImpl[A] { self =>
    def flatMap[B](f: A => Rand[B]): Rand[B] =
      RandVarFlatMap(this, f)
    def orElse[B >: A](that: Rand[B]): Rand[B] =
      RandVarOrElse(this, that)

    def reify: Dist[A] = {
  }

  case class RandVarChoice[+A](dist: Dist[A])
      extends RandVar[A] with CommittedChoice[A]
  case class RandVarFlatMap[A,+B](x: RandVar[A], f: A => Rand[B])
      extends RandVar[B]
  case class RandVarOrElse[+A](x: RandVar[A], y: RandVar[A]) extends RandVar[A]


  import OddsCore.{scale, consolidate, normalize}

  def choice[A](xs: (A, Prob)*): Rand[A] = new RandVarChoice[A](xs)

  def collapse[A](r: Rand[A]): List[(A,Prob)] = {
    type R = List[(A,Prob)]
    def prob[B](path: RandVar[B], p: Prob, env: Map[Int,Any] = Map.empty)(next: (B,Prob,Map[Int,Any]) => R): R = path match {
      case RandVarChoice(id,dist) =>
        env.get(id) match {
          case Some(x) =>
            assert(dist exists (_._1 == x), x+" not in "+dist+" for "+id)
            next(x.asInstanceOf[B],p,env)
          case None => 
            dist flatMap { case (x,q) =>
              next(x, p*q,env + (id -> x))
            }
        }
      case RandVarFlatMap(x,f) =>
        prob(x,p,env) { (y,q,e) => prob(f(y),q,e)(next) }
      case RandVarOrElse(x,y) =>
        prob(x,p,env)(next) ++ prob(y,p,env)(next)
    }
    normalize(consolidate(prob(r,1)((x,p,e)=>List(x->p))))
  }

  def collapse2[A](r: Rand[A], strategy: String, solutions: Int): List[(A,Prob)] = {
    println("searching for min "+solutions+" solutions")
    type R = List[(A,Prob)]
    var more = true
    type Mem = List[(Int, AnyRef, Any, Any)] // memo table: (id hash, function, arg, res)
    type Env = Map[Int,Any]

    def prob[B](path: RandVar[B], p: Prob, mem: Mem, env: Env, budget: Int)(next: (B,Prob,Mem,Env,Int) => R): R = 
    if (budget < 0) { more = true; Nil } else path match {
      case RandVarChoice(id,dist) =>
        env.get(id) match {
          case Some(x) =>
            assert(dist exists (_._1 == x), x+" not in "+dist+" for "+id)
            next(x.asInstanceOf[B],p,mem,env,budget)
          case None => 
            val budget1 = if (dist.lengthCompare(1) <= 0) budget else budget-1 // certain choice doesn't count for depth
            dist flatMap { case (x,q) =>
              next(x, p*q, mem, env + (id -> x), budget1)
            }
        }
      case RandVarFlatMap(x,f) =>
        prob(x,p,mem,env,budget) { (y,q,m,e,k) => 
          // we memoize (continuation,value) pairs.
          // thankfully, closures have identity in Scala.
          // we could also attach ids to flatMap objects?
          m.find(el => (el._2 eq f) && (el._3 == y)) match {
            case Some((id,el,arg,res)) => prob(res.asInstanceOf[RandVar[B]],q,m,e,k)(next)
            case None => val res = f(y); prob(res,q,(System.identityHashCode(f),f,y,res)::m,e,k)(next) 
          }
        }
      case RandVarOrElse(x,y) =>
        prob(x,p,mem,env,budget)(next) ++ prob(y,p,mem,env,budget)(next)
    }

    var res: R = Nil
    var depth = 1
    while (res.length < solutions && more) {
      println("trying depth "+depth)
      more = false
      res = prob(r,1,Nil,Map.empty,depth)((x,p,m,e,k)=>List(x->p))
      depth += 1
    }
    //println(ids.sorted.mkString("\n"))
    // todo: don't throw away all solutions each time, print them as 
    // they are discovered (solutions=5 will never give an answer if 
    // there are only 3)
    normalize(consolidate(res))
  }

}
*/
