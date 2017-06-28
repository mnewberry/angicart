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

(* Command-line utility to skeletonize a point graph *)

let set_check ref v =
  if !ref = None then ref := Some v
  else raise (Arg.Bad "Only one dir argument is supported") ;;
let filename = ref None and outp = ref None ;;
let set_filename = set_check filename ;;
let set_outp = set_check outp ;;

let usage = "Usage: skeletonize -o output [-f] lcc.pg\n"
           ^"  Skeletonize a connected pointgraph" ;;
let argspec = [
  ("-f", Arg.String set_filename, "pointgraph, created by output_value");
  ("-o", Arg.String set_outp, "Output file")] ;;

let argsfail _ = Arg.usage argspec usage ; exit 1 ;;
Arg.parse argspec set_filename usage ;;
match !filename with Some _ -> () | _ -> argsfail () ;;
let req z = match !z with Some x -> x | _ -> invalid_arg "Fail" ;;

module PG = PointGraph ;;
module VT = VesselTree ;;

let dot () = Printf.printf ".%!" ;;

let (lcc : PG.t) = Mu.slurp_obj (req filename) ;; dot () ;;
let tips = VT.local_maximum_distance_tips 3 6. lcc ;; dot () ;;
let skel_g = VT.skeletonize_dt lcc tips ;; dot ();;
let branch_points = PG.with_degs skel_g (Mu.range 3 26) ;; dot () ;;

let edges = PG.edges_gl skel_g ;; dot () ;;
let edge_colors = Mu.map (Mu.constant (255, 128, 0, 255)) edges ;; dot () ;;

let vertices = PG.points_gl lcc ;; dot () ;;
let vertex_colors =
  let color v =
    if PG.Set.mem v tips then (0, 128, 255, 255) else
    if PG.Set.mem v branch_points then (255, 128, 0, 255)
    else (94, 255, 24, 32)
  in
  Mu.map color vertices ;; dot () ;;

Mu.dump_obj (Mu.default "graph.out" !outp) 
            (edges, edge_colors, vertices, vertex_colors) ;;

dot () ;; Printf.printf "\n%!" ;;
