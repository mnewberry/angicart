(** Command-line utility for displaying saved graphs quickly *)

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

let set_check ref v =
  if !ref = None then ref := Some v
  else raise (Arg.Bad "Only one argument is supported") ;;
let filename = ref None and file_is_pg = ref false 
  and points_only = ref false ;;
let set_filename = set_check filename ;;

let usage = "Usage: graphdisplay [-f] filename\n"
let argspec = [
  ("-f", Arg.String set_filename, "File to display");
  ("-p", Arg.Set points_only, "Display points only (no edges)");
  ("-g", Arg.Set file_is_pg, 
         "Take a pointgraph file as input rather than a graphdisplay file")] ;;

let argsfail _ = Arg.usage argspec usage ; exit 1 ;;
Arg.parse argspec set_filename usage ;;
let req z = match !z with Some x -> x | _ -> argsfail () ;;

module PG = PointGraph ;;

let (edges, edge_colors, vertices, vertex_colors) = 
  if not !file_is_pg then Mu.slurp_obj (req filename) 
  else 
    let (pg : PG.t) = Mu.slurp_obj (req filename) in
    let edges = if !points_only then [] else PG.edges_gl pg in
    let vertices = PG.points_gl pg in
    (edges, Mu.map (Mu.constant (255,128,0,128)) edges,
     vertices, Mu.map (Mu.constant (0,0,0,64)) vertices) ;;

GraphGL.display_loop
  ~edges:edges
  ~edge_colors:edge_colors
  ~vertices:vertices
  ~vertex_colors:vertex_colors ;;

