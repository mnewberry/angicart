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

(** Library for things pertaining to building the vessel tree out of a point
graph *)

module Set = Point.Set 
module Gr = PointGraph 
module EMap = Gr.EMap
module Map = Gr.Map

let pr = Printf.printf 

let pi = acos (-1.0) 
let sq x = x * x 
let seq f g x = f (g x) 
let ( |! ) x f = f x
let rad l v = sqrt (v /. (l *. pi))

let tips cc =
  let boundary_points =
    let (sx, sy, sz) = Gr.choose cc in
    let kons1 x v ((max, maxs, min, mins) as extrema) =
      if x < min then      (max, maxs,            x,   Set.singleton v)
      else if x > max then (x,   Set.singleton v, min, mins)
      else if x = max then (max, Set.add v maxs, min,           mins)
      else if x = min then (max,           maxs, min, Set.add v mins)
      else extrema
    in
    let kons3 ((vx, vy, vz) as v) (xe, ye, ze) =
      (kons1 vx v xe, kons1 vy v ye, kons1 vz v ze)
    in
    let kn1 x = (x, Set.empty, x, Set.empty) in
    let (xe, ye, ze) = Gr.fold_vertices kons3 (kn1 sx, kn1 sy, kn1 sz) cc in
    let collect1 (_, max, _, min) = Set.union max min in
    Set.union (collect1 xe) (Set.union (collect1 ye) (collect1 ze))
  in
  let choose_each chf grs = Point.set (List.rev_map chf grs) in
  let set_ccs x = Gr.ccs (Gr.create x) in
  let boundary_groups = set_ccs boundary_points in
  let boundary_tips = choose_each (seq Set.med_elt Gr.points) boundary_groups in
  let tip_points = Gr.with_degs cc [1;2;3;4] in
  let tip_groups = List.filter (fun g -> Gr.size g > 1) (set_ccs tip_points) in
  let choose_tip tg =
    Mu.findf (fun k ->
                try Some (Set.med_elt
                           (Set.inter (Gr.with_deg cc k) (Gr.points tg)))
                with Not_found -> None)
             (Mu.range 1 26)
  in
  let tip_tips = choose_each choose_tip tip_groups in
  Set.union boundary_tips tip_tips

(* This is incredibly slow *)
let rec skeletonize_naive pg tips =
  let pr = Printf.printf in pr ".%!" ;
  let chaff = Set.diff (Gr.points pg) tips in
  if Set.is_empty chaff then Gr.spanning_tree pg else
  let point = Set.med_elt chaff in
  let prov_pg = Gr.remove point pg in
  let ccs = Gr.ccs prov_pg in
  let (removable, non_removable) = 
    List.partition (fun cc -> Set.is_empty (Set.inter (Gr.points cc) tips)) ccs
  in
  if List.length non_removable > 2 
  then skeletonize_naive pg (Set.add point tips)
  else 
    let npg = Mu.fold Gr.remove_s prov_pg (Mu.map Gr.points removable) in
    skeletonize_naive npg tips

(* I feel like goldilocks *)
let skeletonize_less_naive order pg tips =
  let pr = Printf.printf in
  let rec remr point (tot, meat, skel) =
    if not (Set.mem point meat) then (tot, meat, skel) else
    let prov_tot = Gr.remove point tot in
    let ns = Gr.neighbors point tot in
    let nns_g_ccs = Gr.ccs (Gr.create_sg 
      (Set.union ns (Gr.neighbors_set ns prov_tot)) prov_tot) in
    if List.length nns_g_ccs = 1
    then (pr ".%!" ; 
      (Gr.remove point tot, 
       Set.remove point meat, 
       skel)
      )
    else
      let cc_has_skel_kons p = function true-> true | false-> Set.mem p skel in
      let cc_skel_pairs = Gr.ccs_fold cc_has_skel_kons false prov_tot in
      let (nonrmablep, rmablep) = List.partition snd cc_skel_pairs in
      let (nonrmable, rmable) = (Mu.map fst nonrmablep, Mu.map fst rmablep) in
      let gr_k rmable_cc gr = snd (Gr.partition_cc gr (Gr.points rmable_cc)) in
      let gr_k_slow rmable_cc gr = Gr.remove_s (Gr.points rmable_cc) gr in
      let set_k rmable_cc gr = Set.diff gr (Gr.points rmable_cc) in
      if List.length nonrmable > 2
      then (pr "+%!" ; 
        (Mu.fold gr_k_slow tot rmable, (* gr_k only works on ccs *)
         Mu.fold set_k (Set.remove point meat) rmable, 
         Set.add point skel)
        )
      else
        (pr "-%!" ;
        (Mu.fold gr_k prov_tot rmable, 
         Mu.fold set_k (Set.remove point meat) rmable, 
         skel)
        )
  in
  let rec skize batch (gr, meat, skel) =
    if Set.is_empty meat 
    then Gr.spanning_tree gr
    else skize (List.tl batch) (remr (List.hd batch) (gr, meat, skel))
  in
  skize order (pg, Set.diff (Gr.points pg) tips, tips)

(** remove points in batches, using "distance transform" to determine which
    points to remove first *)
let skeletonize_dt pg tips = 
  let pr = Printf.printf in
  let rec remr point (tot, meat, skel) =
    if not (Set.mem point meat) then (tot, meat, skel) else
    let prov_tot = Gr.remove point tot in
    let ns = Gr.neighbors point tot in
    let nns_g_ccs = Gr.ccs (Gr.create_sg 
      (Set.union ns (Gr.neighbors_set ns prov_tot)) prov_tot) in
    if List.length nns_g_ccs = 1
    then (prov_tot, Set.remove point meat, skel)
    else (pr "+%!" ; 
      (tot, Set.remove point meat, Set.add point skel))
  in
  pr ">%!" ;
  let dt = Gr.distance_transform Point.dist pg in
  pr "dt;%!" ;
  let removal_order = Mu.map fst 
    (List.sort (Mu.compare_with snd) (Map.bindings dt)) in
  pr "ro;%!" ;
  let rec skize batch (gr, meat, skel) =
    if Set.is_empty meat 
    then gr
    else skize (List.tl batch) (remr (List.hd batch) (gr, meat, skel))
  in
  let run pg = 
    (pr "\nSTART\n%!" ; 
    skize removal_order (pg, Set.diff (Gr.points pg) tips, tips)
    )
  in
  let best = 
    Mu.fix_eq (fun a b -> Set.equal (Gr.points a) (Gr.points b)) run pg in
  skeletonize_less_naive removal_order best tips
  

(* this is broken *)
let skeletonize pg tips =
  let pr = Printf.printf in
  let rec remr point non_removable tails (tot, meat, skel) =
    let prov_tot = Gr.remove point tot in
    let ns = Gr.neighbors point tot in
    if Set.is_empty ns then
      pr "\nns empty, ns in pg = %s, tails=%d\n%!"
         (Point.set_to_str (Gr.neighbors point pg))
         (match tails with None -> (-1) | Some t -> List.length t) ;
    let subgraphs = match tails with
        None -> Gr.ccs (Gr.create_sg ns prov_tot)
      | Some sgs -> sgs in
    match Gr.find_disconnected prov_tot subgraphs with
        (_, Some sg) -> 
          if non_removable = []
          then 
(pr ".%!";
            (prov_tot, Set.remove point meat, skel)
)
          else
            if Set.is_empty (Set.inter (Gr.points sg) skel) then
              match (Gr.find_p_cc (fun p -> not (Set.mem p skel)) prov_tot sg)
              with Some cc ->
                    let to_rm = (Set.add point (Gr.points cc)) in
(pr "N%!";
                    (Gr.remove_s to_rm tot, Set.diff meat to_rm, skel)
)
                | None ->
(pr "+N%!";
                    (tot, Set.remove point meat, Set.add point skel)
)
            else
(pr "+)N%!";
              (tot, Set.remove point meat, Set.add point skel)
)
      | (Some (cc, tl), _) ->
          if Set.is_empty (Set.inter (Gr.points cc) skel) (* cc is removable *)
          then 
(pr "-%!";
            let s = Gr.points cc in
            remr point [] (Some tl) (Gr.remove_s s tot, Set.diff meat s, skel)
)
          else (* point is in skel iff its removal creates two or more
                  non-removable connected components *)
            if non_removable = []
            then 
(pr "`%!"; 
              remr point [cc] (Some tl) (tot, meat, skel)
)
            else
(pr "+%!";
              (tot, Set.remove point meat, Set.add point skel)
)
      | _ -> failwith "can't happen"
  in
  let choose_p = Set.med_elt in
  let rec skize (gr, meat, skel) =
    pr "|%!" ;
    if Set.is_empty meat then Gr.spanning_tree gr
    else skize (remr (choose_p meat) [] None (gr, meat, skel))
  in
  skize (pg, Set.diff (Gr.points pg) tips, tips)

type data = {
  len : float ;
  vol : float ;
  voxc : int ;
  defc : int ;
  ls : float Map.t ;
  color : int * int * int ;
}

let data e edata = EMap.find e edata
let rad_e e edata = let ed = data e edata in rad ed.len ed.vol 
let rad_d ed = rad ed.len ed.vol
let devf e edata = let ed = data e edata in float ed.defc /. float ed.voxc
let point_lengths e edata = (data e edata).ls
let points e edata = Map.keys (point_lengths e edata)
let color e edata = (data e edata).color

let summarize dist skel cc =
  let vol_mm3 = 
    let zero = (0, 0, 0) 
      and xhat = (1, 0, 0) and yhat = (0, 1, 0) and zhat = (0, 0, 1) in
    dist zero xhat *. dist zero yhat *. dist zero zhat in
  let nodes = Set.diff (Gr.points skel) (Gr.with_deg skel 2) in
  let start = Set.choose nodes in
  let weight a b =
    if Set.mem b (Gr.points skel) && Set.mem a (Gr.neighbors b skel) 
    then 0. else dist a b
  in
  let (spt, ls) = Gr.shortest_path_tree start weight cc in
  let rec traverse (src, node) rest =
    let rec path (len, (src, dst)) =
      let len = len +. dist src dst in
      match Gr.degree dst skel with
        2 -> path (len, (dst, Set.choose (Gr.downstream (src, dst) skel)))
      | _ -> (len, (src, dst))
    in
    let kons ntab (gr, edata) =
      let (len, (dtab, dnode)) = path (0., (node, ntab)) in
      let edge = Gr.canonical_edge node dnode in
      let points = 
        if Gr.are_neighbors node dnode cc
        then Set.singleton node |! Set.add dnode
        else Gr.find_cc dtab (Gr.remove node (Gr.remove dnode spt))
          |! Set.add dnode |! Set.add node in
      let tot = Set.cardinal points in
      let vol = float tot *. vol_mm3 in
      let diam = rad len vol +. 1.0 in
      let deformed = Set.filter (fun p -> Map.find p ls > diam) points in
      let defc = Set.cardinal deformed in
      let gr = Gr.add_edge edge gr in
      let edata = EMap.add edge
        { len = len; vol = vol *. (float (tot - defc) /. float tot) ; 
          voxc = tot ; defc = defc ;
          ls = Map.with_keys (Set.elements points) ls ;
          color = (Random.int 255, Random.int 255, Random.int 255) }
        edata in
      traverse (dtab, dnode) (gr, edata)
    in
    Set.fold kons (Gr.downstream (src, node) skel) rest
  in
  let start = Set.choose nodes in
  traverse (start, start) (Gr.empty, EMap.empty)

let tree_dataset sg edata tag =
  let cat = String.concat "" and map = List.map 
    and canon = Gr.canonicalize_edge in
  let downst e = Gr.downstream_e e sg  in
  let show_e e = let (src, dst) = canon e in
    Printf.sprintf "%s-%s" (Point.to_str src) (Point.to_str dst)
  in
  let show_col (r, g, b) = Printf.sprintf "#%02x%02x%02x" r g b in
  let show_edata e ed = 
    Printf.sprintf "%s\t%f\t%f\t%f\t%d\t%d\t%s" (show_e e) ed.len ed.vol 
      (rad ed.len ed.vol) ed.voxc ed.defc (show_col ed.color)
  in
  let rec count_tips e = let cs = downst e in
    if cs = [] then 1 else Mu.sum (map count_tips cs)
  in
  let rec describe_e p pd e ed =
    let children = map (fun e -> (e, data (canon e) edata)) (downst e) in
    let betas = map (fun (c, cd) -> rad_d cd /. rad_d ed) children in
    let gammas = map (fun (c, cd) -> cd.len /. ed.len) children in
    let next_k xs k = 
      k -. (Mu.sumf (map (fun x -> x ** k) xs) -. 1.0) /.
           (Mu.sumf (map (fun x -> x ** k *. log x) xs))
    in
    let (a, b) = 
      if children = [] then ("NA", "NA") else
      let str xs = 
        if List.for_all (fun x -> x < 1.0) xs 
        then Printf.sprintf "%f" (Mu.rec_n 5 (next_k xs) 2.5)
        else "NA"
      in (str gammas, str betas) in
    (* name len vol radius vox_c deformed_c color parent beta gamma a b *)
    Printf.sprintf "%s\t%s\t%d\t%s\t%f\t%f\t%d\t%s\t%s\n" tag (show_edata e ed) 
      (count_tips e) (show_e p) (rad_d ed /. rad_d pd) (ed.len /. pd.len) 
      (List.length children) a b
    ^ cat (map (fun (c, cd) -> describe_e e ed c cd) children)
  in

  let (src, dst) as e =
    Mu.max_by (fun e -> rad_e e edata) (Gr.ESet.elements (Gr.edges sg)) in
  let ed = data e edata in
  let des c = describe_e e ed c (data (canon c) edata) in
  Printf.sprintf 
  "tag\tname\tlen\tvol\trad\tvoxc\tdefc\tcol\ttips\tparent\tbeta\tgamma\tnchild\ta\tb\n"
  ^ Printf.sprintf "%s\t%s\t%d\tNA\tNA\tNA\tNA\tNA\tNA\n" tag (show_edata e ed)
      (Mu.max_kons (count_tips e) (count_tips (dst, src)))
  ^ cat (map des (downst e))
  ^ cat (map des (downst (dst, src)))
