package odds

import org.scalatest.FlatSpec
import language.implicitConversions

trait AircraftModel extends OddsLang {

  sealed abstract class Dir
  case object North extends Dir
  case object East extends Dir
  case object South extends Dir
  case object West extends Dir

  val number_aircrafts : Rand[Int] = geometric(0.85)

  //size of grid
  val npos = 10

  //initial position of plane i
  def xpos0(i: Int) = uniform(0 until npos: _*)
  def ypos0(i: Int) = uniform(0 until npos: _*)

  //initial direction of plane i
  def dir0(i: Int) = uniform[Dir](North, East, South, West)

  //x,y derivatives for plane i travelling at time t
  def xderiv(i: Int, t: Int)(d: Dir) : Rand[Int] = d match {
    case North => choice(-1 -> 0.1,  0 -> 0.8, 1 -> 0.1)
    case East  => choice( 0 -> 0.2,  1 -> 0.7, 2 -> 0.1)
    case South => choice(-1 -> 0.1,  0 -> 0.8, 1 -> 0.1)
    case West  => choice( 0 -> 0.2, -1 -> 0.7, 2 -> 0.1)
  }

  def yderiv(i: Int, t: Int)(d: Dir): Rand[Int] = d match {
    case North => choice( 0 -> 0.2,  1 -> 0.7, 2 -> 0.1)
    case East  => choice(-1 -> 0.1,  0 -> 0.8, 1 -> 0.1)
    case South => choice( 0 -> 0.2, -1 -> 0.7, 2 -> 0.1)
    case West  => choice(-1 -> 0.1,  0 -> 0.8, 1 -> 0.1)
  }

  //new direction for plane i at time t
  def new_dir(i: Int, t: Int)(d: Dir): Rand[Dir] = d match {
    case North => choice(West  -> 0.2, North -> 0.6, East  -> 0.2)
    case East  => choice(North -> 0.2, East  -> 0.6, South -> 0.2)
    case South => choice(East  -> 0.2, South -> 0.6, West  -> 0.2)
    case West  => choice(South -> 0.2, West  -> 0.6, North -> 0.2)
  }

  type Pos = (Int,Int)

  //equations of motion and radar detection
  //the state of each plane at a given time
  case class PlaneState(plane_idx: Int, plane_pos: Pos, plane_dir: Dir)
  case class LPlaneState(lplane_idx: Rand[Int], lplane_pos: Rand[Pos], lplane_dir: Rand[Dir])

  //implicit lifting for planeStates, should be done in liftstruct
  implicit def liftPlane(lp: LPlaneState): Rand[PlaneState] = {
    for(
      idx <- lp.lplane_idx;
      pos <- lp.lplane_pos;
      dir <- lp.lplane_dir
    ) yield PlaneState(idx, pos, dir)
  }

  implicit def liftList[T](ls: List[Rand[T]]): Rand[List[T]] = ls match{
    case Nil => always(Nil)
    case x::xs =>
      val liftedTail = liftList(xs)
      x.flatMap{x2 => liftedTail.map(x2::_)}
  }

  //lifing accessors
  def pidx(p: Rand[PlaneState]): Rand[Int] = p.map(_.plane_idx)
  def ppos(p: Rand[PlaneState]): Rand[Pos] = p.map(_.plane_pos)
  def pdir(p: Rand[PlaneState]): Rand[Dir] = p.map(_.plane_dir)

  //append random lists
  def append[T](x: Rand[List[T]], y: Rand[List[T]]): Rand[List[T]] = x flatMap {
    case Nil => y
    case h::tl => append(always(tl),y).map(xs=>h::xs) // full list as input, not very efficient?
  }


  //obtain a sample initial plane state, for the plane number i
  def lplane_state0(iRand: Rand[Int]): Rand[PlaneState] = LPlaneState(
    iRand,
    iRand.flatMap{i => (xpos0(i),ypos0(i))},
    iRand.flatMap{i => dir0(i)}
  )

  //evolve the state of one plane to the next time moment
  def plane_fly(t:Int, pstate: Rand[PlaneState]): Rand[PlaneState] = {
    val iRand = pidx(pstate)
    val pPos = ppos(pstate)
    val (xRand, yRand) = (tuple2_get1(pPos), tuple2_get2(pPos))
    val dirRand = pdir(pstate)

    val newx: Rand[Int] = for(
      i <- iRand;
      x <- xRand;
      dir <- dirRand;
      x2 <- xderiv(i, t)(dir)
    ) yield {
      val v = x + x2
      if( v < 0 || v > 9) -1 //never
      else v //always(v)
    }

    val newy: Rand[Int] = for(
      i <- iRand;
      y <- yRand;
      dir <- dirRand;
      y2 <- yderiv(i, t)(dir)
    ) yield {
      val v = y + y2
      if( v < 0 || v > 9) -1 //never
      else v //always(v)
    }

    val newdir: Rand[Dir] =
      for(i <- iRand; dir <- dirRand; newdir <- new_dir(i,t)(dir)) yield newdir

    liftPlane(LPlaneState(
      lplane_idx = iRand,
      lplane_pos = (newx, newy),
      lplane_dir = newdir
    ))
  }

  // parameters of the system

  abstract class SysParam{
    /* A possibly stochastic function that removes a plane from the
    list of existing planes. This is to model the plane disappearance
    (e.g., going too low or too high for radar detection)
    */
    def sp_annihilate(ls: Rand[List[PlaneState]]) : Rand[List[PlaneState]]

    /* A possibly stochastic function that returns the number of new
    planes to create.
    */
    def sp_create: Rand[Int]
  }

  //not too sure whether st_counter should be a random variable
  //the identity is quite deterministic in fact, but for type checking
  //sake it seems easier to make it a rand[int]
  case class SysState(st_counter : Rand[Int], st_planes: Rand[List[PlaneState]])
  //don't see a reason for having sys_sstate too

  // The blip model itself
  /* Arguments of the model:
      parms  -- sys_parms of the evolution
      state  -- sys_state from the previous step
      timep  -- the time moment
      observations: a function that checks the evidence. It receives
              the time moment and plane_states
   The model returns the evolved sys_state
  */
  def blip_HMM_step(parms: SysParam, state: SysState, timep: Int, evidence: (Rand[List[PlaneState]], Int) => Unit) : SysState = {
    val (cnt, planes) = (state.st_counter, state.st_planes)
    val lessplanes = parms.sp_annihilate(planes)
    //evolve the remaining planes
    val movedplanes : Rand[List[PlaneState]] = lessplanes flatMap { lps =>
      lps.map{plane: PlaneState => plane_fly(timep, always(plane))}
    }

    val np = parms.sp_create

    /*
    val countsAndPlanes: Rand[(Int, List[LPlaneState])] = np.flatMap{n =>
      if(n == 0) (cnt,movedplanes)
      else {
        val newplanes = movedplanes ++ {for (i <- 1 to n) lplane_state0(movedplanes.size+i)}
        (cnt+n, newplanes)
      }
    }
    */
    //the above is not used for now because we don't need a Rand[List[PlaneState]], I think
    val cnt2 = cnt + np

    val planes2: Rand[List[PlaneState]] = np.flatMap{n =>
      if(n==0) movedplanes
      else {
        append(movedplanes,
          (for (i <- 1 to n) yield lplane_state0(cnt+always(i))).toList
        )
      }
    }

    evidence(planes2, timep)
    SysState(cnt2, planes2)
  }

  // ---------------  The ideal case: no observation noise

  // Asserting the evidence

  // First is the ideal case: the ground truth. There is no observation noise.
  /* The given list of blips are all the blips that are observed.
     There are no blips anywhere else on the screen. This case lets us
     test the equations of motion.
  */

  def blips_ideal(blips: Rand[List[Pos]])(pstates: Rand[List[PlaneState]]) = {
    val blips_obs: Rand[List[(Pos,Boolean)]] = blips flatMap {poses =>
      liftList(poses map {
        case (x,y) => make_tuple2((make_tuple2((always(x), always(y))),always(false)))
      })
    }
    /* observe the plane at plane_pos. Account for the observation
       in blips_obs by toggling the flag. Fail if plane_pos does not
       correspond to any blip.
    */

    def observe(plane: Rand[PlaneState], bops: Rand[List[(Pos, Boolean)]]): Rand[List[(Pos, Boolean)]] = bops flatMap {
      case Nil => always(Nil)
      case (pos,b)::xs =>
        val matchedpos = plane.flatMap{ p =>
          if(p.plane_pos == pos) always((pos, true))
          else always((pos,b))
        }

        val observedTail = observe(plane, always(xs))
        matchedpos.flatMap{
          mpos => observedTail.map{mpos::_}
        }
    }

    val pstates2 = pstates flatMap{ xs =>
      xs.foldLeft(blips_obs){
        case (b_obs, plane) => observe(always(plane), b_obs)
      }
    }

    pstates2 flatMap {xs =>
      if(xs.exists(!_._2)) never else always(xs)
    }
  }

  // A few commonly-used parameters
  val parm_no = new SysParam{
    def sp_annihilate(ls: Rand[List[PlaneState]]) = ls.map{x => x}
    def sp_create: Rand[Int] = always(0)
  }

/*  let parm_n n = {sp_annihilate = (fun x -> x);
                  sp_create = (fun () -> geometric_bounded n 0.85)};;

  let parm_only n = {sp_annihilate = (fun x -> x);
                     sp_create = (fun () -> n)};;
*/

  val state0 = SysState(st_counter = always(0), st_planes = always(Nil))

}

/*
(* Conversions between lazy and strict states *)
let force_plane lstate =
   {plane_idx = lstate.lplane_idx;
    plane_pos = (let (x,y) = lstate.lplane_pos in (x (), y ()));
    plane_dir = lstate.lplane_dir ()};;


let lazy_plane state =
   {lplane_idx = state.plane_idx;
    lplane_pos = (let (x,y) = state.plane_pos in ((fun () -> x), fun () ->y));
    lplane_dir = fun () -> state.plane_dir};;


let lazy_state sstate =
  {st_counter = sstate.st_scounter;
   st_planes =  List.map lazy_plane sstate.st_splanes};;





*/