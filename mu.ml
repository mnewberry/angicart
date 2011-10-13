(** Mitchell's utilities *)
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


(** Alternative versions of standard functions that may fix some defects and
introduce others.  Items labeled [kons] are intended to be fold's cons-like
argument.  Cf.  SRFI-1 *)

(** {7 Misc } *)

let min_max_kons x (min, max) =
  ((if x < min then x else min), (if x > max then x else max)) ;;
let min_kons x min = if x < min then x else min ;;
let max_kons x max = if x > max then x else max ;;
let min_max_kons_by f x ((min, min_obj), (max, max_obj)) =
  ((if f x < min then f x, x else min, min_obj), 
   (if f x > max then f x, x else max, max_obj)) ;;
let min_kons_by f x (min, min_obj) = 
  if f x < min then f x, x else (min, min_obj) ;;
let max_kons_by f x (max, max_obj) = 
  if f x > max then f x, x else (max, max_obj) ;;

let identity x = x ;;
let constant k = (fun x -> k) ;;

let cons a b = a :: b ;;

let hypot a b = sqrt (float (a * a + b * b))
let hypot2 (ax, ay) (bx, by) = let sq x = x * x in 
  sqrt (float (sq (ax - bx) + sq (ay - by)))
let hypot3 (ax, ay, az) (bx, by, bz) = let sq x = x * x in 
  sqrt (float (sq (ax - bx) + sq (ay - by) + sq (az - bz)))

let default d = function
    None -> d
  | Some v -> v

let sgn n = compare n 0 ;;
let divrnd a b = let q = (2 * a) / b in (q + sgn q) / 2 ;;
let sigmod a n = a - n * (divrnd a n)
let ( % ) a b = if a < 0 then a mod b + b else a mod b ;;
  (** The mod operator with a range of [0] through [b] *)

(** {7 Fold } *)

(** [fold kons knil lyst], a pidgin version of the classic list iterator *)
let rec fold kons knil = function 
    [] -> knil
  | h :: t -> fold kons (kons h knil) t ;;

let rec fold2 kons knil la lb = match (la, lb) with ([], []) -> knil
  | ((ha :: ta), (hb :: tb)) -> fold2 kons (kons ha hb knil) ta tb ;
  | _ -> invalid_arg "unequal lengths" ;;

let rec fold3 kons knil la lb lc = match (la, lb, lc) with ([], [], []) -> knil
  | ((ha :: ta), (hb :: tb), (hc :: tc)) -> 
    fold3 kons (kons ha hb hc knil) ta tb tc ;
  | _ -> invalid_arg "unequal lengths" ;;

(** pidgin unfold *)
let rec unfold is_knull kar kdr value tail = if is_knull value = true then tail
  else unfold is_knull kar kdr (kdr value) ((kar value) :: tail) ;;

(** This 'unified' unfold allows the caller to compute [(kar, kdr)] of [value]
at the same time, which may save redundant computation in some cases. *)
let rec unfold_u is_knil unkons seed tail = if is_knil seed = true then tail
  else let (kar, kdr) = unkons seed in 
       unfold_u is_knil unkons kdr (kar :: tail) ;;

(** {7 Common uses of [fold]} *)

(** Any specialization of fold will necessarily be tail-recursive. *)

let min = function 
  (hx :: tx) -> fold min_kons hx tx 
  | _ -> invalid_arg "min_max is not defined on empty lists" ;;
let max = function 
  (hx :: tx) -> fold max_kons hx tx 
  | _ -> invalid_arg "min_max is not defined on empty lists" ;;
let min_max = function
  (hx :: tx) -> fold min_max_kons (hx, hx) tx
  | _ -> invalid_arg "min_max is not defined on empty lists" ;;
let min_by f = function 
  (hx :: tx) -> snd (fold (min_kons_by f) (f hx, hx) tx) 
  | _ -> invalid_arg "min_max is not defined on empty lists" ;;
let max_by f = function 
  (hx :: tx) -> snd (fold (max_kons_by f) (f hx, hx) tx)
  | _ -> invalid_arg "min_max is not defined on empty lists" ;;
let min_max_by f = function
  (hx :: tx) -> 
    let ((_, min), (_, max)) = 
      fold (min_max_kons_by f) ((f hx, hx), (f hx, hx)) tx in
    (min, max)
  | _ -> invalid_arg "min_max is not defined on empty lists" ;;
let rev l = fold cons [] l ;;
let map f l = let kons kar kdr = (f kar) :: kdr in 
  fold kons [] (rev l) ;;
let map2 f la lb = let kons a b kdr = (f a b) :: kdr in 
  fold2 kons [] (rev la) (rev lb) ;;
let map3 f la lb lc = let kons a b c kdr = (f a b c) :: kdr in 
  fold3 kons [] (rev la) (rev lb) (rev lc) ;;
let app m n = fold cons n (rev m) ;;
let catmap f l = let kons kar kdr = app (f kar) kdr in fold kons [] (rev l) ;;
let filtmap f l = 
  let kons kar kdr = match f kar with Some x -> x :: kar | None -> kar in
  fold kons [] (rev l) ;;
let sfiltmap f l = 
  let kons kar kdr = match f kar with Some x -> x :: kar | None -> kar in
  fold kons [] l ;;
let cat l = fold app [] (rev l) ;;

(** Note that partition and grep reverse the order of the list *)

let grep p l = let kons kar kdr = if p kar then kar :: kdr else kdr in
  fold kons [] l ;;

(** partition a list according to a predicate *)
let partition p l = 
  let kons x (ts, fs) = if p x then (x :: ts, fs) else (ts, x :: fs) in
  fold kons ([], []) l ;;

(** partition a list into (a list of) equivalence classes *)
let partition_eqc eq l =
  let eqcs_kons el eqcs =
    let c_kons eqc (eqcs, added) =
      if not added && eq (List.hd eqc) el then ((el :: eqc) :: eqcs, true)
      else (eqc :: eqcs, added)
    in
    let (new_eqcs, added) = fold c_kons ([], false) eqcs in
    if added then new_eqcs else [el] :: eqcs
  in
  fold eqcs_kons [] l

let rec findf f = function
    [] -> raise Not_found
  | x :: l -> (match f x with Some v -> v | None -> findf f l)

(** a fold which runs [kons] once for each equivalence class of elements
    in a list.  The first argument to [kons] is a list of all the
    elements in the equivalence class. *)
let rec fold_eqc eq kons knil els = fold kons knil (partition_eqc eq els)

let ( -- ) a b = 
  if a < b then unfold ((>) a) identity pred b [] 
  else unfold ((<) a) identity succ b [] ;;
  (** Interval operator *)

let range a b = a -- b ;;

let sum = fold (+) 0
let sumf = fold (+.) 0. ;;

let prod = fold ( * ) 0
let prodf = fold ( *. ) 0. ;;

let pow a b = 
  let rec f b t = match b with 0 -> t | b -> f (b - 1) (a * t) in f b 1

let cross a b =
  let kons ael knil = fold (fun bel kn -> (ael, bel) :: kn) knil (rev b) in
  fold kons [] (rev a) ;;
  

(** {7 Zip and unzip} *)

let zip la lb = let f a b = (a, b) in map2 f la lb ;;
let zip3 la lb lc = let f a b c = (a, b, c) in map3 f la lb lc ;;

let unzip l = let kons (a, b) (al, bl) = (a :: al, b :: bl) in 
  fold kons ([], []) l ;;
let unzip3 l = let kons (a, b, c) (al, bl, cl) = (a :: al, b :: bl, c :: cl) in 
  fold kons ([], [], []) l ;;

(** {7 words from Haskell} *)

let take n l = 
  let rec take_ n acc l = match l with 
    [] -> acc
    | (hl :: tl) -> if n > 0 then take_ (n - 1) (hl :: acc) tl else rev acc in
  take_ n [] l ;;

let repeat n x = 
  let rec r n a x = if n > 0 then r (n - 1) (x :: a) x else a in r n [] x ;;

(** {7 creative ideas} *)

(** A better syntax for compare: [compare (f a) (f b)] *)
let compare_with f a b = compare (f a) (f b)

(** {!compare_with} with opposite ordering *)
let compare_with_m f a b = compare (f b) (f a)

(** recurse until a fixed point is achieved *)

let rec fix f i = let n = f i in if n = i then n else fix f n
let rec fix_eq eq f i = let n = f i in if eq n i then n else fix f n

(** recurse a number of times *)
let rec rec_n n f i = if n = 0 then i else rec_n (n - 1) f (f i)

(** {7 Quick and Dirty File IO} *)

let dump filename str = let ch = open_out filename in 
  output_string ch str ; close_out ch ;;
let dump_obj filename obj = let ch = open_out filename in 
  output_value ch obj ; close_out ch ;;

let slurp filename = let chan = open_in filename in
  let rec slurp_ str = 
    try slurp_ (str ^ String.make 1 (input_char chan)) 
    with End_of_file -> str in
  slurp_ "" ;;
let slurp_obj filename = let chan = open_in filename in
  input_value chan ;;

(** {5 The extended remix} *)

module Ext = struct
  let rmap f l = let kons kar kdr = (f kar) :: kdr in fold kons [] l ;;
  let rapp a b = fold cons a b ;;

  let canonical_pair a b = if a < b then (a, b) else (b, a) ;;
  let is_singleton l = (l <> []) && (List.tl l == []) ;;
  let time thunk =
    let start = Unix.gettimeofday () in thunk () ;
    Unix.gettimeofday () -. start
end
