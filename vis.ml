(** This file Copyright (c) 2011 Mitchell Johnson. *)
(* *)
(* This software is free software; you can redistribute it and/or *)
(* modify it under the terms of the GNU Library General Public *)
(* License version 2, with the special exception on linking *)
(* described in file LICENSE. *)
(* *)
(* This software is distributed in the hope that it will be useful, *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. *)

(** Command-line utility for measuring and printing data from skeletons. *)

let set_check ref v =
  if !ref = None then ref := Some v
  else raise (Arg.Bad "Only one argument is supported") ;;
let filename = ref None and goutfn = ref None and voutfn = ref None 
  and dimension = ref None ;;
let set_filename = set_check filename ;;
let set_goutfn = set_check goutfn ;;
let set_voutfn = set_check voutfn ;;
let set_dimension = set_check dimension ;;

let usage = "Usage: vis -g out.gp -v out.tsv -d 0.7x0.7x0.8 [-f] skel.gd\n"
let argspec = [
  ("-f", Arg.String set_filename, "pointgraph, created by output_value");
  ("-g", Arg.String set_goutfn, "Output graph for visualization");
  ("-v", Arg.String set_voutfn, "tree data tab-separated value file");
  ("-d", Arg.String set_dimension, "Dimension of a voxel in millimeters")
] ;;

let argsfail _ = Arg.usage argspec usage ; exit 1 ;;
Arg.parse argspec set_filename usage ;;
let req z = match !z with Some x -> x | _ -> argsfail () ;;

let openoc = function Some x -> open_out x | _ -> stdout ;;
let vout = openoc !voutfn ;;

let (edges, edge_colors, vertices, vertex_colors) 
  = Mu.slurp_obj (req filename) ;;

let dims = Scanf.sscanf (req dimension) "%fx%fx%f" (fun x y z -> (x, y, z)) ;;

module VT = VesselTree ;;
module PS = Point.Set ;;
module PG = PointGraph ;;
module ESet = PG.ESet ;;
module Map = PG.Map ;;

let pr = Printf.printf ;;
let str = Printf.sprintf ;;
let print = output_string ;;

let _ =
  let dist (ax, ay, az) (bx, by, bz) =
    let (dx, dy, dz) = dims in
    let sqd dm x = dm *. float (x * x) in
    sqrt (sqd dx (ax - bx) +. sqd dy (ay - by) +. sqd dz (az - bz))
  in
  let skel_set = Point.set edges in pr "1%!" ;
  let skel_g = PG.spanning_tree (PG.create skel_set) in pr "2%!" ;
  let graph_set = Point.set vertices in pr "3%!" ;
  let graph_g = PG.create graph_set in pr "4%!" ;
  let ccs = PG.ccs graph_g in pr "5%!" ;
  let vass = List.hd (List.sort (Mu.compare_with_m PG.size) ccs) in pr "6%!" ;
  let (summ, edata) = VT.summarize dist skel_g vass in pr "7%!" ;
  let edges_gl = PG.edges_gl summ in pr "8\n%!" ;
  print vout (VT.tree_dataset summ edata (req filename)) ;
  let rec ecolor inp outp = match inp with
      [] -> Mu.rev outp
    | (a :: b :: rest) ->
      let e = (a, b) in (*let r = VT.rad_e e edata in*)
      (*let clip mn mx v = if v < mn then mn else if v > mx then mx else v in*)
      (*let alph = truncate (clip 32. 256. (r *. 224. /. 1.5 +. 32.)) in*)
      (*let deviance = truncate (255. -. 255. *. (VT.devf e edata)) in*)
      let col = (0, 0, 0, if VT.devf e edata > 0.20 then 96 else 255) in
      ecolor rest (col :: col :: outp)
    | _ -> invalid_arg "Uneven list"
  in
  Random.init 5 ;
  let vert_colors =
    let kons_colorlist edge colorlist =
      let pls = VT.point_lengths edge edata in
      let points = Map.keys pls in
      (*let rad_e = VT.rad_e edge edata in*)
      (*let (r,g,b) = VT.color edge edata in*)
      (*let f c = if VT.devf edge edata > 0.20 then c / 2 else c / 2 + 128 in*)
      Mu.fold 
        (fun p rest ->
          (p, (128, 128, 128,
                 if VT.devf edge edata > 0.20 then 8 else 64)) 
          :: rest)
        colorlist points 
    in
    ESet.fold kons_colorlist (PG.edges summ) []
  in
  let (vs, v_colors) = List.split vert_colors in
  (match !goutfn with
    Some fn -> Mu.dump_obj fn 
      (edges_gl, ecolor edges_gl [], vs, v_colors)
    | _ -> ()) ;;
