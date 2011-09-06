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

(* Graphs whose nodes are points *)

module Edge = struct
  type t = (Point.t * Point.t)
  let compare = compare
end
module EMap = MuMap.Make(Edge)
module ESet = MuSet.Make(Edge)
module Map = MuMap.Make(Point)
module Set = Point.Set
module Heap = BatHeap

(* Fix ocaml's Set.Make().fold taking arguments in the wrong order *)
let set_fold kons knil set = Set.fold kons set knil
let eset_fold kons knil eset = ESet.fold kons eset knil

type t = {
  ps : Set.t ;         (* Point (vertex) set *)
  d2ps : Set.t array ; (* a map from degree to the points having that degree *)
  p2ns : Set.t Map.t ; (* a point to its neighbors *)
}

let empty = {
  ps = Set.empty ;
  d2ps = Array.make 27 Set.empty ;
  p2ns = Map.empty 
}

let (|!) a f = f a

let is_empty g = Set.is_empty g.ps

(* this function is unsafe, since it relies on neighbors being symmetric *)
let create_n neighbors set =
  (* we can skip steps (relative to add) since the set is already created,
     and neighbors and degrees are known. *)
  let add_fast point pg =
    let ns = Set.inter (neighbors point) set in
    let k = Set.cardinal ns in
    pg.d2ps.(k) <- Set.add point pg.d2ps.(k) ;
    { pg with
      p2ns = Map.add point ns pg.p2ns }
  in
  set_fold add_fast {empty with ps = set ; d2ps = Array.make 27 Set.empty} set

(** create a [!PointGraph.t] from a [!Point.Set.t] using the standard neighbor
    relation ([!Point.neighbors]) *)
let create set = create_n Point.neighbors set

let create_points set = create_n (Mu.constant Set.empty) set

let with_deg pg n = pg.d2ps.(n)
let with_degs pg l = let kons a b = Set.union (with_deg pg a) b in
  Mu.fold kons Set.empty l
let degree point pg = Set.cardinal (Map.find point pg.p2ns)
let neighbors point pg = Map.find point pg.p2ns 
let neighbors_set set pg = 
  Set.fold (fun p set -> Set.union (neighbors p pg) set) set Set.empty
let are_neighbors a b pg = Set.mem b (Map.find a pg.p2ns)
let choose pg = Set.med_elt pg.ps
let size pg = Set.cardinal pg.ps
let points pg = pg.ps

(** [create_sg set gr] creates a subgraph of [gr] using the points in [set] and
all edges from [gr] that might connect them.  Throws an exception of [set] is
not a subset of [points gr]. *)
let create_sg set gr = create_n (fun p -> neighbors p gr) set

let equal a b = Set.equal a.ps b.ps
  && Set.for_all (fun p -> Set.equal (neighbors p a) (neighbors p b)) a.ps

(** [remove point pg] creates a new point graph with point and all edges
    connected to it removed.  It is idempotent. *)
let remove rp pg =
  if not (Set.mem rp pg.ps) then pg else
  let npg =
    let k = degree rp pg in
    let d2ps = Array.copy pg.d2ps in
    (* We copy the array here, because we should never modify our input *)
    d2ps.(k) <- Set.remove rp d2ps.(k) ;
    { ps = Set.remove rp pg.ps ;
      d2ps = d2ps ;
      p2ns = Map.remove rp pg.p2ns } 
  in
  let update_kons point pg =
    let k = degree point pg in
    (* Here we don't copy the array because we can modify our own variables *)
    pg.d2ps.(k) <- Set.remove point pg.d2ps.(k) ;
    pg.d2ps.(k - 1) <- Set.add point pg.d2ps.(k - 1) ;
    { pg with p2ns = Map.update point (Set.remove rp) pg.p2ns }
  in
  Set.fold update_kons (neighbors rp pg) npg 

let unsafe_remove_edge (an, bn) pg =
  let p2ns = Map.update bn (Set.remove an) pg.p2ns in
  if p2ns = pg.p2ns then pg else
  let p2ns = Map.update an (Set.remove bn) p2ns in
  let ak = ref 0 and bk = ref 0 in
  pg.d2ps.(!ak) <- Set.remove an pg.d2ps.(!ak) ;
  pg.d2ps.(!ak - 1) <- Set.add an pg.d2ps.(!ak - 1) ;
  pg.d2ps.(!bk) <- Set.remove bn pg.d2ps.(!bk) ;
  pg.d2ps.(!bk - 1) <- Set.add bn pg.d2ps.(!bk - 1) ;
  { pg with p2ns = p2ns }

let remove_edge e pg = 
  unsafe_remove_edge e { pg with d2ps = Array.copy pg.d2ps }

let remove_edges es pg = 
  Mu.fold unsafe_remove_edge { pg with d2ps = Array.copy pg.d2ps } es

let add_n neighbors rp pg =
  if Set.mem rp pg.ps then pg else
  let ns = Set.inter (neighbors rp) pg.ps in
  let npg = 
    let k = Set.cardinal ns in
    let d2ps = Array.copy pg.d2ps in
    d2ps.(k) <- Set.add rp d2ps.(k) ;
    { ps = Set.add rp pg.ps ;
      d2ps = d2ps ;
      p2ns = Map.add rp ns pg.p2ns } in
  let update_kons point pg =
    let k = degree point pg in
    pg.d2ps.(k) <- Set.remove point pg.d2ps.(k) ;
    pg.d2ps.(k + 1) <- Set.add point pg.d2ps.(k + 1) ;
    { pg with p2ns = Map.update point (Set.add rp) pg.p2ns }
  in
  set_fold update_kons npg ns

(** [add point pg] creates a new point graph including [point] and edges for
    all its adjacent points.  It does nothing if [point] is already in [pg]. *)
let add rp pg = add_n Point.neighbors rp pg

(** add only a point without adding any edges for its neighbors *)
let add_point rp pg = add_n (Mu.constant Set.empty) rp pg

(** [add_edge (an, bn) pg] adds [an] and [bn] to pg if not already present,
    and an edge connecting them *)
let add_edge (an, bn) pg =
  let anmem = Set.mem an pg.ps and bnmem = Set.mem bn pg.ps in
  if anmem && bnmem && Set.mem an (neighbors bn pg) then pg else
  let update_d2ps mem point pg = (* this is destructive *)
    if mem then 
      let k = degree point pg in
      pg.d2ps.(k) <- Set.remove point pg.d2ps.(k) ;
      pg.d2ps.(k + 1) <- Set.add point pg.d2ps.(k + 1) ; pg
    else 
      let pg = { pg with ps = Set.add point pg.ps } in
      pg.d2ps.(1) <- Set.add point pg.d2ps.(1) ; pg
  in
  let update_p2ns src dst pg = 
    { pg with p2ns = Map.addf src (Set.add dst) Set.empty pg.p2ns }
  in
  (* copy the d2ps array so we can use destructive modifications *)
  { pg with d2ps = Array.copy pg.d2ps } |! update_d2ps anmem an 
    |! update_d2ps bnmem bn |! update_p2ns an bn |! update_p2ns bn an

let canonical_edge = Mu.Ext.canonical_pair
let canonicalize_edge (src, dst) = Mu.Ext.canonical_pair src dst

(** [downstream (src, dst) pg] are the neighbors to [dst] other than [src] *)
let downstream (src, dst) pg = Set.remove src (neighbors dst pg)

(** [downstream_e (src, dst) pg] is a list of edges connecting [dst] and
    [downstream (src, dst) pg] *)
let downstream_e (src, dst) pg =
  let kons n l = (dst, n) :: l in
  Set.fold kons (Set.remove src (neighbors dst pg)) []

(** ESet of all (undirected) edges occurring in the graph *)
let edges pg =
  let kons point es =
    let addn n es = ESet.add (canonical_edge point n) es in
    set_fold addn es (neighbors point pg)
  in
  set_fold kons ESet.empty pg.ps

let fold_edges kons knil pg = ESet.fold kons (edges pg) knil
let fold_vertices kons knil pg = Set.fold kons pg.ps knil

let edges_gl pg =
  let es = edges pg in
  let kons (src, dst) es = src :: dst :: es in
  eset_fold kons [] es

let points_gl pg = Set.elements pg.ps

(* This could be done better, obviously *)
let union apg bpg =
  if (is_empty apg) then apg else
  if (is_empty bpg) then bpg else
  create (Set.union apg.ps bpg.ps)

let union_n neighbors apg bpg =
  if (is_empty apg) then apg else
  if (is_empty bpg) then bpg else
  create_n neighbors (Set.union apg.ps bpg.ps)

let add_l pg = Mu.fold add pg
let add_s pg set = Set.fold add set pg
let remove_l pg = Mu.fold remove pg
let remove_s set pg = Set.fold remove set pg
let union_l = Mu.fold union empty
let create_l l = create (Point.set l)

(** find the set of points connected to a given point *)
let find_cc start pg = 
  let rec kons point seen =
    let new_ns = Set.diff (neighbors point pg) seen in
    set_fold kons (Set.union new_ns seen) new_ns
  in
  kons start (Set.singleton start)

let fold_cc kons knil start pg = 
  let rec traverse point (seen, knil) =
    let new_ns = Set.diff (neighbors point pg) seen in
    set_fold traverse (Set.union new_ns seen, kons point knil) new_ns
  in
  traverse start (Set.singleton start, kons start knil)

let partition_cc pg cc = 
  (* We don't need to do anything fancy with neighbors and degrees, since
     elements of cc share no neighbors with the rest of the graph.  We just
     need to split apart the various maps. *)
  let in_cc key _ = Set.mem key cc in
  let (p2ns_in_cc, p2ns_nin_cc) = Map.partition in_cc pg.p2ns in
  ({ ps = cc ;
     d2ps = Array.map (Set.inter cc) pg.d2ps ;
     p2ns = p2ns_in_cc },
   { ps = Set.diff pg.ps cc ;
     d2ps = Array.map (fun ps -> Set.diff ps cc) pg.d2ps ;
     p2ns = p2ns_nin_cc })

(** partition a graph into a list of its connected component graphs *)
let ccs pg =
  (* Split a non-empty pg into "head" and "tail", where head is a cc, and tail
     is the rest.  If tail is empty, pg has only one connected component. *)
  let split pg =
    let cc = find_cc (choose pg) pg in
    partition_cc pg cc 
  in
  Mu.unfold_u is_empty split pg []

let ccs_fold kons knil pg =
  (* Split a non-empty pg into "head" and "tail", where head is a cc, and tail
     is the rest.  If tail is empty, pg has only one connected component. *)
  let split pg =
    let (cc, kn) = fold_cc kons knil (choose pg) pg in
    let (ccg, restg) = partition_cc pg cc in
    ((ccg, kn), restg)
  in
  Mu.unfold_u is_empty split pg []

(** find the largest connected component, 
    hopefully without traversing the entire graph *)
let largest_cc pg =
  let rec findr max max_size pg pg_size =
    if pg_size < max_size then (max, max_size) else
    let (cc, size) = fold_cc (fun _ n -> succ n) 0 (choose pg) pg in
    let (ccg, npg) = partition_cc pg cc in
    if size > max_size
    then findr ccg size npg (pg_size - size)
    else findr max max_size npg (pg_size - size)
  in
  findr empty 0 pg (size pg)

(** [curtailed_nodes subgraph graph] is the set of nodes in [subgraph] of
    [graph] who have fewer edges in [subgraph] than in [graph]. *)
let curtailed_nodes subgraph pg =
  Mu.fold (fun n st ->
            Set.union (Set.diff (with_deg subgraph n) (with_deg pg n)) st)
          Set.empty (Mu.range 1 26)

(** [neighbors_sg subgraph graph] is the neighbors to [subgraph] in [graph],
    as a set of vertices. *)
let neighbors_sg subgraph pg =
  let ps = curtailed_nodes subgraph pg in
  let ns = Set.fold (fun p st -> Set.union (neighbors p pg) st) ps Set.empty in
  Set.diff ns subgraph.ps

(** [find_disconnected g subgraphs], if [subgraphs] are connected in [g],
returns [(None, Some sg)], where sg is a subgraph of [g] that connects
[subgraphs].  Otherwise, it returns [(Some (sg_cc, tail), None)], where [sg_cc]
is some connected component of [g] which contains at least one of [subgraphs],
and does not contain at least one of [subgraphs], and [tail] is a residual set
of subgraphs of unknown connectedness which can be passed as [subgraphs] to
another call to [find_disonnected]. *)
let pr = Printf.printf
let rec find_disconnected gr = function
    (hd :: []) -> (pr "]%!" ; (None, Some hd))
  | (hd :: tl) ->
      let gr_ns p = neighbors p gr in
      let curtailed = curtailed_nodes hd gr in
      if Set.is_empty curtailed 
      then (pr "!" ; (Some (hd, tl), None) )
      else
        let hd_ns = neighbors_set curtailed gr in
        let (ncsgs, csgs) = List.partition
          (fun sg -> Set.is_empty (Set.inter hd_ns sg.ps)) tl in
        let hd_exp = Set.fold (add_n gr_ns) hd_ns hd in
        let new_hd = Mu.fold (union_n gr_ns) hd_exp csgs in
        (pr "\\%!" ;
        find_disconnected gr (ncsgs @ [new_hd])
        )
   | [] -> invalid_arg "find_disconnected gr [] is undefined"

let rec find_p_cc is_p gr sg =
  let curtailed = curtailed_nodes sg gr in
  if Set.is_empty curtailed
  then Some sg
  else 
    let sg_ns = Set.diff (neighbors_set curtailed gr) sg.ps in
    if Set.for_all is_p sg_ns
    then 
      let gr_ns p = neighbors p gr in
      find_p_cc is_p gr (Set.fold (add_n gr_ns) sg_ns sg)
    else None


(** A haphazardly chosen spanning tree of a haphazardly chosen connected
    component *)
let spanning_tree cc =
  let rec kons point (seen, edges) =
    let new_ns = Set.diff (neighbors point cc) seen in
    let new_edges = Mu.map (canonical_edge point) (Set.elements new_ns) in
    set_fold kons (Set.union new_ns seen, Mu.Ext.rapp edges new_edges) new_ns
  in
  let start = choose cc in
  let (_, edges) = kons start (Set.singleton start, []) in
  Mu.fold add_edge empty edges


let _dijkstra_paths gr w nd ls =
  let k nd nh = Heap.add (w nd nh +. Map.find nd ls, (nd, nh)) in
  Set.fold (k nd) (neighbors nd gr) Heap.empty

let rec _dijkstra_iter cc w spt ls front =
  if front = Heap.empty then (spt, ls) else
  let (len, (src, dst)), rest = Heap.find_min front, Heap.del_min front in
  if Set.mem dst spt.ps then _dijkstra_iter cc w spt ls rest else
  let ls = Map.add dst len ls in
  _dijkstra_iter 
    cc w (add_edge (src, dst) spt) ls 
    (Heap.merge (_dijkstra_paths cc w dst ls) rest)
  
(** The tree of shortest paths from a node, roughly following Dijkstra *)
let shortest_path_tree origin weight cc =
  let ls = Map.add origin 0. Map.empty in
  _dijkstra_iter 
    cc weight (add origin empty) ls (_dijkstra_paths cc weight origin ls)

(* this is roughly identical to the above,
   just a different initialiation and return value. *)
let distance_transform weight cc =
  let bound_ps = Mu.map (with_deg cc) (Mu.range 1 25)
    |! Mu.fold Set.union Set.empty in
  let ls = Set.fold
    (fun p ls ->
      let out_adjs = (Set.diff (Point.neighbors p) (neighbors p cc)) in
       Map.add p
               (1.0 /. (Mu.sumf (Mu.map (fun a -> 1.0 /. Point.dist a p) 
                                        (Set.elements out_adjs))))
               ls)
    bound_ps Map.empty in
  let front = Set.fold 
    (fun p f -> Heap.merge (_dijkstra_paths cc weight p ls) f) 
    bound_ps Heap.empty in
  snd (_dijkstra_iter cc weight (create_points bound_ps) ls front)
  
