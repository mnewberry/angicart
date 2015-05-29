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
let filename = ref None and file_is_pg = ref false and file_is_img3 = ref false
  and points_only = ref false ;;
let set_filename = set_check filename ;;

let usage = "Usage: graphdisplay [-f] filename\n"
let argspec = [
  ("-f", Arg.String set_filename, "File to display");
  ("-g", Arg.Set file_is_pg, 
         "Take a pointgraph file as input rather than a graphdisplay file") ;
  ("-p", Arg.Set points_only, "With -g, display points only (no edges)");
  (*  This option is removed temporarily due to a conflict between SDL,
      required by ImageSet, and lablgl.glut, required by GraphGL which
      manifests only on OS X.  On other systems, or after this conflict has
      been resolve through more clever packaging of those dependencies,
      uncomment the code here and below, and modify the make.sh script to link
      graphdisplay to both libraries.
  ("-i", Arg.Set file_is_img3, 
         "Take a 3d image as input rather than a graphdisplay file") *)] ;;

let argsfail _ = Arg.usage argspec usage ; exit 1 ;;
Arg.parse argspec set_filename usage ;;
let req z = match !z with Some x -> x | _ -> argsfail () ;;

module PG = PointGraph ;;

let (edges, edge_colors, vertices, vertex_colors) = 
  if !file_is_pg then 
    let (pg : PG.t) = Mu.slurp_obj (req filename) in
    let edges = if !points_only then [] else PG.edges_gl pg in
    let vertices = PG.points_gl pg in
    (edges, Mu.map (Mu.constant (255,128,0,128)) edges,
     vertices, Mu.map (Mu.constant (0,0,0,255)) vertices) 
  else (* if !file_is_img3 then
    let (ba : ImageSet.ba3) = Mu.slurp_obj (req filename) in
    ImageSet.normalize_img ba ;
    (* let vertices = 
      let (w, h, d) = ImageSet.dims ba in
      Mu.map (fun ((a, b), c) -> (a, b, c))
        (Mu.cross (Mu.cross (Mu.range 0 (w - 1)) (Mu.range 0 (h - 1)))
           (Mu.range 0 (d - 1))) in
    Printf.printf ".\n%!" ; *)
    Printf.printf "up'd, %!" ;
    let vertices = ref [] in
    let vertex_colors = ref [] in
    let intens (i, j, k) = truncate (ba.{i,j,k} *. 255.) in
    let (w, h, d) = ImageSet.dims ba in
    for i = 0 to w - 1 do for j = 0 to h - 1 do for k = 0 to d - 1 do
      if intens (i, j, k) > 145 then (
        vertices := (i, j, k) :: !vertices ;
        vertex_colors := (intens (i,j,k), 0, 0, intens (i, j, k)) :: !vertex_colors)
    done done done ;
    ([], [], !vertices, !vertex_colors)
  else *)
    Mu.slurp_obj (req filename)  ;;

GraphGL.display_loop
  ~edges:edges
  ~edge_colors:edge_colors
  ~vertices:vertices
  ~vertex_colors:vertex_colors ;;

