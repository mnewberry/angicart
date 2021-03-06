(** This file Copyright (c) 2011-2016 Mitchell Newberry. *)
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
let absf = abs_float

let pi = acos (-1.0) 
let sq x = x * x 
let seq f g x = f (g x) 
let ( |! ) x f = f x
let rad l v = sqrt (v /. (l *. pi))
let absf = abs_float

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
  let boundary_tips = choose_each (seq Point.centermost Gr.points)
    boundary_groups in
  let tip_points = Gr.with_degs cc [1;2;3;4] in
  let tip_groups = List.filter (fun g -> Gr.size g > 1) (set_ccs tip_points) in
  let choose_tip tg =
    Mu.findf (fun k ->
                try Some (Point.centermost
                           (Set.inter (Gr.with_deg cc k) (Gr.points tg)))
                with Not_found -> None)
             (Mu.range 1 26)
  in
  let tip_tips = choose_each choose_tip tip_groups in
  Set.union boundary_tips tip_tips

let tips_alt cc =
  let (spt, ls) = Gr.shortest_path_tree (Gr.choose cc) Point.dist cc in
  let is_local_max pt =
    Set.for_all (fun n -> Map.find n ls < Map.find pt ls) (Gr.neighbors pt cc)
  in Set.filter is_local_max (Gr.with_degs cc [1;2;3;4;5;6;7;8;9;10;11;12;13])

let local_maximum_distance_tips local_radius same_radius cc =
  let (spt, ls) = Gr.shortest_path_tree (Gr.choose cc) Point.dist cc in
  let is_local_max pt =
    Set.for_all (fun n -> Map.find n ls <= Map.find pt ls) 
      (Gr.neighborhood local_radius pt cc) in 
  let tips = Set.filter is_local_max
    (Gr.with_degs cc (Mu.range 1 16)) in
  let rec same_tips point same =
    let (a,b,c) = point in Printf.printf "(%d, %d, %d)\n%!" a b c ;
    let same_mk nh len same =
      if (len < Point.dist point nh *. (sqrt 2. +. 1. /. sqrt 5.) +. 1.0)
        && Set.mem nh tips && not (Set.mem nh same)
      then Set.union (same_tips nh (Set.add point same)) same else same in
    Map.fold same_mk (Gr.find_within point Point.dist cc same_radius cc.Gr.ps) 
      same in
  let rec prune acc = function
      [] -> acc
    | hd :: tl -> 
      let hd_same_tips = same_tips hd Set.empty in 
      prune (Set.add hd acc)
        (List.filter (fun p -> not (Set.mem p hd_same_tips)) tl) in
  prune Set.empty
    (List.sort (Mu.compare_with (fun p -> Map.find p ls))
      (Set.elements tips)) (* Include tips in descending order *)

(* Remove duplicate tips *)
(* Two tips are the same if they are closeby, and their distance within the
	 network is comparable to their euclidean distance (ie, they are not in
	 separate vessels).  Since this sameness relationship is not transitive,
	 same_tips computes the transitive closure of sameness.  same_tips-ness is a
   legit equivalence relation. *)
  (* let ptset lst = Mu.fold Set.add Set.empty lst in *)
let pruned_tips_alt w radius cc =
  let same_tips point =
    let same_mk nh len same =
      if len < w point nh *. 1.1 +. 1. then Set.add nh same else same in
    Map.fold same_mk (Gr.find_within point w cc radius cc.Gr.ps) Set.empty in
  let rec prune pruned unpruned =
    if unpruned = Set.empty then pruned else
    let (next, unpruned) = Set.pop unpruned in
    prune (Set.add next pruned) (Set.diff unpruned (same_tips next))
  in prune Set.empty (tips_alt cc)

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
  stab : Point.t ;
  dtab : Point.t ;
  stab5 : Point.t option ;
  dtab5 : Point.t option ;
  stab20 : Point.t option ;
  dtab20 : Point.t option ;
}

let data e edata = EMap.find e edata
let rad_e e edata = let ed = data e edata in rad ed.len ed.vol 
let len_e e edata = let ed = data e edata in ed.len
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
  let (spt, ls) = 
    let weight a b =
      if Set.mem b (Gr.points skel) && Set.mem a (Gr.neighbors b skel) 
      then 0. else dist a b
    in Gr.shortest_path_tree start weight cc in
  let rec traverse (src, node) rest =
    let rec path (len, nsteps, nt5, nt20, (src, dst)) =
      let len = len +. dist src dst in
      let nsteps = nsteps + 1 in
      let nt5 = if nsteps == 5 then Some dst else nt5 in
      let nt20 = if nsteps == 20 then Some dst else nt20 in
      match Gr.degree dst skel with
        2 -> path (len, nsteps, nt5, nt20, 
          (dst, Set.choose (Gr.downstream (src, dst) skel)))
      | _ -> (len, nsteps, nt5, nt20, (src, dst))
    in
    let kons ntab (gr, edata) =
      let (len, nsteps, snt5, snt20, (dtab, dnode)) = 
        path (0., 0, None, None, (node, ntab)) in
      let (len_, nsteps_, dnt5, dnt20, (ntab_, node_)) =
        path (0., 0, None, None, (dnode, dtab)) in
      (if absf (len -. len_) > 0.0000000001 || nsteps <> nsteps_ 
         || node <> node_ || ntab <> ntab_
       then failwith "Assertion failed: irreversible edge.") ;
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
          color = Mu.hsl2rgb (Random.float 360., 1., 0.5) ;
          stab   = ntab ;  dtab   = dtab ;
          stab5  = snt5 ;  dtab5  = dnt5 ;
          stab20 = snt20 ; dtab20 = dnt20 }
        edata in
      traverse (dtab, dnode) (gr, edata)
    in
    Set.fold kons (Gr.downstream (src, node) skel) rest
  in
  let start = Set.choose nodes in
  traverse (start, start) (Gr.empty, EMap.empty)

(** Solve 1 = sum_i c_i^q for q using newton's method *)
let newton_exp_solve xs init = let map = List.map in
  let next_k xs k =
    k -. (Mu.sumf (map (fun x -> x ** k) xs) -. 1.0) /.
         (Mu.sumf (map (fun x -> x ** k *. log x) xs))
  in Mu.rec_n 100 (next_k xs) init

(* debugging *)
let sumpow xs q = Mu.sumf (Mu.map (fun x -> x ** q) xs)
let newton_test lst = let e = newton_exp_solve lst 1.0 in (sumpow lst e, e)

let branch_point_dataset sg edata tag =
  let bps =
    Set.diff (Set.diff (Gr.points sg) (Gr.with_deg sg 2)) (Gr.with_deg sg 1) in
  let keys = EMap.keys edata in 
  let edges bp = List.filter (fun (x,y) -> x = bp || y = bp) keys in
  let circular edge = let ed = data edge edata in
    ed.defc < truncate (0.2 *. (float ed.voxc)) && ed.voxc > 3 in
  let describe_bp bp =
    let es = edges bp in
    if List.filter (fun e -> not (circular e)) es = [] then ([],[],[],[]) else
    let ignwarn = List.sort (Mu.compare_with_m (fun e -> rad_e e edata)) es in
    let (par, cs) = (List.hd ignwarn, List.tl ignwarn) in
    let betas = List.map (fun e -> rad_e e edata /. rad_e par edata) cs in 
    let gammas = List.map (fun e -> len_e e edata /. len_e par edata) cs in 
    (betas, gammas, 
    [newton_exp_solve betas 1.0], [newton_exp_solve gammas 1.0]) in
  Set.fold 
    (fun bp str ->
       let fl = Printf.sprintf "%f" in
       let rec print_lines str (bs, gs, qs, ss) = match (bs, gs) with
           ([], []) -> str 
         | ((bhd :: btl), (ghd :: gtl)) -> 
             let pr q s = str ^ 
               (Printf.sprintf "%s\t%s\t%s\t%s\n" (fl bhd) (fl ghd) q s) in
             (match (qs, ss) with 
                 ([], []) -> 
                   print_lines (pr "NA" "NA") (btl, gtl, [], [])
               | ((qhd :: qtl), (shd :: stl)) ->
                   print_lines (pr (fl ghd) (fl shd)) (btl, gtl, qtl, stl)
               | _ -> failwith "can't happen")
         | _ -> failwith "can't happen" in
       print_lines str (describe_bp bp))
    bps "beta\tgamma\tq\ts\n"

let symmetric_tree_dataset () = 
  let beta = 2. ** (-.(1./.2.)) in
  let gamma = 2. ** (-.(1./.3.)) in
  let rec fake_lines name rad len = 
    let vol = pi *. rad *. rad *. len in
    if rad < 0.6 /. beta 
    then (1, [Printf.sprintf 
      "SYM\t%d\t%f\t%f\t%f\t%d\t0\t#000000\t%d\t%d\t%f\t%f\t0\t2.0\t3.0"
      name len vol rad (int_of_float vol) 1 (name/2) beta gamma ])
    else
    let (tls, lls) = fake_lines (name * 2) (rad *. beta) (len *. gamma) in
    let (trs, lrs) = fake_lines (name * 2 + 1) (rad *. beta) (len *. gamma) in
    (tls + trs, [ Printf.sprintf
      "SYM\t%d\t%f\t%f\t%f\t%d\t0\t#000000\t%d\t%d\t%f\t%f\t2\t2.0\t3.0"
      name len vol rad (int_of_float vol) (tls + trs) (name/2) beta gamma ]
      @ lls @ lrs)
  in 
  "tag\tname\tlen\tvol\trad\tvoxc\tdefc\tcol\ttips\tparent\tbeta\tgamma\tnchild\tq\ts\n"
  ^ String.concat "\n" (snd (fake_lines 1 13.6 100.))

let asymmetric_tree_dataset lambda =
  let beta1 = (1. +. lambda ** 2.) ** (-1./.2.) in
  let beta2 = beta1 *. lambda in
  let gamma1 = (1. +. lambda ** 3.) ** (-1./.3.) in
  let gamma2 = gamma1 *. lambda in
  let rec fake_lines name prad plen beta gamma =
    let vol = pi *. prad *. prad *. beta *. beta *. plen *. gamma in
    if prad < 0.6 /. beta 
    then (1, [Printf.sprintf
      "SYM\t%d\t%f\t%f\t%f\t%d\t0\t#000000\t%d\t%d\t%f\t%f\t0\tNA\tNA"
      name (plen *. gamma) vol (prad *. beta) (int_of_float vol) 
      1 (name/2) beta gamma ])
    else
    let (tls, lls) = fake_lines (name * 2) (prad *. beta) (plen *. gamma) 
                       beta1 gamma1 in
    let (trs, lrs) = fake_lines (name * 2 + 1) (prad *. beta) (plen *. gamma) 
                       beta2 gamma2 in
    (tls + trs, [ Printf.sprintf
      "SYM\t%d\t%f\t%f\t%f\t%d\t0\t#000000\t%d\t%d\t%f\t%f\t2\t%f\t%f"
      name (plen *. gamma) vol (prad *. beta) (int_of_float vol) 
      (tls + trs) (name/2) beta gamma 
      (newton_exp_solve [beta1;beta2] 1.0) 
      (newton_exp_solve [gamma1;gamma2] 1.0)]
      @ lls @ lrs)
  in 
  "tag\tname\tlen\tvol\trad\tvoxc\tdefc\tcol\ttips\tparent\tbeta\tgamma\tnchild\tq\ts\n"
  ^ String.concat "\n" (snd (fake_lines 1 13.6 100. beta1 gamma1))

let tree_dataset sg edata tag =
  let cat = String.concat "" and map = List.map
    and canon = Gr.canonicalize_edge in
  let downst e = Gr.downstream_e e sg  in
  let show_e e = let (src, dst) = canon e in
    Printf.sprintf "%s-%s" (Point.to_str src) (Point.to_str dst)
  in
  (* List positions 1 and 5 edges down the vessel centerline *)
  let show_tabs ed =
    let print_pto = function Some pt -> Point.to_str pt | _ -> "NA" in
    String.concat "\t"
      ((Point.to_str ed.stab) :: (Point.to_str ed.dtab) ::
        (map print_pto [ed.stab5; ed.dtab5; ed.stab20; ed.dtab20]))
  in
  let show_col (r, g, b) = Printf.sprintf "#%02x%02x%02x" r g b in
  let show_edata e ed =
    Printf.sprintf "%s\t%s\t%f\t%f\t%f\t%d\t%d\t%s" (show_e e) (show_tabs ed)
      ed.len ed.vol 
      (rad ed.len ed.vol) ed.voxc ed.defc (show_col ed.color)
  in
  let rec count_tips e = let cs = downst e in
    if cs = [] then 1 else Mu.sum (map count_tips cs)
  in
  let children e ed = map (fun e -> (e, data (canon e) edata)) (downst e) in
  let show_cdata e ed =
    let children = children e ed in
    let betas = map (fun (c, cd) -> rad_d cd /. rad_d ed) children in
    let gammas = map (fun (c, cd) -> cd.len /. ed.len) children in
    let (a, b) = 
      if children = [] then ("NA", "NA") else
      let str xs = 
        if List.for_all (fun x -> x < 0.9999) xs
        then string_of_float (newton_exp_solve xs 1.0) else "NA"
(*      let exp = newton_exp_solve xs 1.0 in
        if (List.for_all ((<>) 1.0) xs) & (absf (sumpow xs exp -. 1.0) < 0.001)
        then string_of_float exp else "NA" *)
      in (str betas, str gammas) in
    Printf.sprintf "%d\t%s\t%s"
      (List.length children) a b (* (Mu.join "," (Mu.map (fun (a, b) -> show_e a) children)) *)
  in
  let rec describe_e p pd e ed =
    Printf.sprintf "%s\t%s\t%d\t%s\t%f\t%f\t%s\n" tag (show_edata e ed)
      (count_tips e) (show_e p) (rad_d ed /. rad_d pd) (ed.len /. pd.len) 
      (show_cdata e ed) (* (Mu.join "," (Mu.map (fun (a, b) -> show_e a) children)) *)
    ^ cat (map (fun (c, cd) -> describe_e e ed c cd) (children e ed))
  in

  let (src, dst) as e =
    Mu.max_by (fun e -> rad_e e edata) (Gr.ESet.elements (Gr.edges sg)) in
  let ed = data e edata in
  let des c = describe_e e ed c (data (canon c) edata) in
  let edir = if count_tips e < count_tips (dst,src) then (dst, src) else e in
  "tag\tname\tstab\tdtab\tstab5\tdtab5\tstab20\tdtab20\tlen\tvol\trad\tvoxc\tdefc\tcol\ttips\tparent\tbeta\tgamma\tnchild\tq\ts\n"
  ^ Printf.sprintf "%s\t%s\t%d\tNA\tNA\tNA\t%s\n" tag (show_edata e ed)
      (count_tips edir) (show_cdata edir ed)
  ^ cat (map des (downst edir))
