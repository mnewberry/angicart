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

(** Command-line utility for converting point lists to point graphs *)

let set_check ref v = 
  if !ref = None then ref := Some v
  else raise (Arg.Bad "Only one filename argument is supported") ;;
let filename = ref None and outp = ref None and lcc_only = ref true ;;
let set_filename = set_check filename ;;
let set_outp = set_check outp ;;

let usage = "Usage: pls2pg -o output [-f] points.pls\n"
           ^"  Transform a list of (int * int * int) points to a PointGraph\n"
           ^"  of its largest connected component." ;;
let argspec = [
  ("-f", Arg.String set_filename, "point list file, created by output_value");
  ("-E", Arg.Clear lcc_only, "Keep entire graph, not just the LCC");
  ("-o", Arg.String set_outp, "output file")] ;;

let argsfail _ = Arg.usage argspec usage ; exit 1 ;;
Arg.parse argspec set_filename usage ;;
match !filename with Some _ -> () | _ -> argsfail () ;;
let req z = match !z with Some x -> x | _ -> invalid_arg "Fail" ;;

let (vertices : (int * int * int) list) = Mu.slurp_obj (req filename) ;;
let pg = PointGraph.create (Point.set vertices) ;;
let graph = if !lcc_only
  then let (lcc, size) = PointGraph.largest_cc pg in
       Printf.printf "%d points\n" size ; lcc
  else pg ;;
Mu.dump_obj (Mu.default "out.lcc.pg" !outp) graph ;;
