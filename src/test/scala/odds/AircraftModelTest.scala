package odds

import org.scalatest.FlatSpec

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
  type LPos = (Rand[Int], Rand[Int]) //lazily evaluated
  //corresponds to Rand[Pos]

  //equations of motion and radar detection
  //the state of each plane at a given time
  case class PlaneState(plane_idx: Int, plane_pos: Pos, plane_dir: Dir)
  case class LPlaneState(lplane_idx: Rand[Int], lplane_pos: LPos, lplane_dir: Rand[Dir])

  //obtain a sample initial plane state, for the plane number i
  def lplane_state0(i: Int) = LPlaneState(
    always(i),
    (xpos0(i),ypos0(i)),
    dir0(i)
  )

  //evolve the state of one plane to the next time moment
  def plane_fly(t:Int, pstate: LPlaneState) = {
    val iRand = pstate.lplane_idx
    val (xRand,yRand) = pstate.lplane_pos
    val dirRand = pstate.lplane_dir


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

    LPlaneState(
      lplane_idx = iRand,
      lplane_pos = (newx, newy),
      lplane_dir = newdir
    )
  }

  // parameters of the system

  abstract class SysParam{
    /* A possibly stochastic function that removes a plane from the
    list of existing planes. This is to model the plane disappearance
    (e.g., going too low or too high for radar detection)
    */
    def sp_annihilate(ls: List[LPlaneState]) : List[LPlaneState]

    /* A possibly stochastic function that returns the number of new
    planes to create.
    */
    def sp_create: Rand[Int]
  }

  case class SysState(st_counter : Int, st_planes: List[LPlaneState])
  //don't see a reason for having sys_sstate too

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


(* The blip model itself *)
(* Arguments of the model:
    parms  -- sys_parms of the evolution
    state  -- sys_state from the previous step
    timep  -- the time moment
    observations: a function that checks the evidence. It receives
            the time moment and plane_states
 The model returns the evolved sys_state
*)

let blip_HMM_step parms state timep evidence =
   let (cnt,planes) = (state.st_counter,state.st_planes) in
   let planes = parms.sp_annihilate planes in
   let planes =       (* evolve the remaining planes *)
     List.map (fun ps -> letlazy (fun () -> plane_fly timep ps)) planes in
   let np = parms.sp_create () in (* perhaps add more planes *)
   let (cnt,planes) = if np = 0 then (cnt,planes) else
    let new_cnt = cnt + np in
    let rec st0loop acc i =
      if i >= new_cnt then acc
      else st0loop (letlazy (fun () -> lplane_state_0 i)::acc) (succ i)
    in (new_cnt, st0loop planes cnt)
   in let () = evidence planes timep in   (* check the evidence *)
   {st_scounter = cnt;
    st_splanes = List.map (fun th -> force_plane (th ())) planes}
;;

(* ---------------  The ideal case: no observation noise *)

(* Asserting the evidence *)

(* First is the ideal case: the ground truth. There is no observation noise. *)
(* The given list of blips are the all blips that are observed.
   There are no blips anywhere else on the screen. This case lets us
   test the equations of motion.
*)

let blips_ideal (blips : pos list) (pstates : (unit -> lplane_state) list) =
  let blips_obs = List.map (fun pos -> (pos, false)) blips in
  (* observe the plane at plane_pos. Account for the observation
     in blips_obs by toggling the flag. Fail if plane_pos does not
     correspond to any blip.
  *)
  let rec observe plane_pos = function [] -> fail ()
   | ((x,y) as pos,_)::rest when x = fst plane_pos () && y = snd plane_pos ()
     -> (pos,true)::rest
   | bp::rest -> bp :: observe plane_pos rest
  in
  let blips_obs_post =
   List.fold_left (fun blips_obs psl -> observe (psl ()).lplane_pos blips_obs)
   blips_obs pstates in
  (* Check that there are no blips unaccounted for *)
  List.iter (fun (_,flag) -> if not flag then fail ()) blips_obs_post
;;

(* A few commonly-used parameters *)
let parm_no =         (* Keep the number of planes:    *)
   {sp_annihilate = (fun x -> x); (* no planes appear or disappear *)
    sp_create = (fun () -> 0)};;

let parm_n n = {sp_annihilate = (fun x -> x);
                sp_create = (fun () -> geometric_bounded n 0.85)};;

let parm_only n = {sp_annihilate = (fun x -> x);
                   sp_create = (fun () -> n)};;

let state0 = {st_counter = 0; st_planes = []};;
*/