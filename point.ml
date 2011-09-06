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

(** Creative ways of thinking about points. *)

(** Graph.Imperative.Graph.Concrete requires a Graph.Sig.COMPARABLE *)

type t = int * int * int
type t_ = t

module Set = MuSet.Make(struct 
  type t = t_
  let compare = compare 
end)

(* This is a hash function that reverses (mirrors) bits.
   See stanford bit twiddling.
   It is perfect for points inside a 2^10 cube.
   If ever points do not fit into a 1024x1024x1024 cube, use Int64 instead
   to admit a 2^20 cube, and remember to update the constants. *)
let crypt (a, b, c) = 
  let oi = Int32.of_int 
    and (>>) = Int32.shift_right_logical and (<<) = Int32.shift_left
    and (&) = Int32.logand and (^) = Int32.logor in
  let v = oi a ^ (oi b << 10) ^  (oi c << 20) in
  let v = ((v >> 1) & 0x55555555l) ^ ((v & 0x55555555l) << 1) in
  let v = ((v >> 2) & 0x33333333l) ^ ((v & 0x33333333l) << 2) in
  let v = ((v >> 4) & 0x0f0f0f0fl) ^ ((v & 0x0f0f0f0fl) << 4) in
  let v = ((v >> 8) & 0x00ff00ffl) ^ ((v & 0x00ff00ffl) << 8) in
  (v >> 16) ^ (v << 16)

let compare = compare 
  (* for a haphazard order, use Int32.compare (crypt a) (crypt b) *)
let hash = Hashtbl.hash
let to_int3 (a, b, c) = (a, b)
let equal = (=)

let to_str (a, b, c) = Printf.sprintf "(%d, %d, %d)" a b c

(** {7 Point set operations} *)

let set = List.fold_left (fun a b -> Set.add b a) Set.empty

let set_to_str set = 
  let sp = Set.choose set in
  "{" ^ 
  (Set.fold (fun p s -> to_str p ^ "," ^ s) (Set.remove sp set) (to_str sp)) ^
  "}"

let dist (ax, ay, az) (bx, by, bz) = 
  let sq x = x * x in 
  sqrt (float (sq (ax - bx) + sq (ay - by) + sq (az - bz)))

(** A list of the neighbors to [(x, y, z)] *)
let neighbors_l (x, y, z) =
 [(x - 1, y - 1, z - 1); (x + 0, y - 1, z - 1); (x + 1, y - 1, z - 1);
  (x - 1, y + 0, z - 1); (x + 0, y + 0, z - 1); (x + 1, y + 0, z - 1);
  (x - 1, y + 1, z - 1); (x + 0, y + 1, z - 1); (x + 1, y + 1, z - 1);
  (x - 1, y - 1, z + 0); (x + 0, y - 1, z + 0); (x + 1, y - 1, z + 0);
  (x - 1, y + 0, z + 0);                        (x + 1, y + 0, z + 0);
  (x - 1, y + 1, z + 0); (x + 0, y + 1, z + 0); (x + 1, y + 1, z + 0);
  (x - 1, y - 1, z + 1); (x + 0, y - 1, z + 1); (x + 1, y - 1, z + 1);
  (x - 1, y + 0, z + 1); (x + 0, y + 0, z + 1); (x + 1, y + 0, z + 1);
  (x - 1, y + 1, z + 1); (x + 0, y + 1, z + 1); (x + 1, y + 1, z + 1)]

let neighbors (x, y, z) = set (neighbors_l (x, y, z))

let are_neighbors ((ax, ay, az) as a) ((bx, by, bz) as b) = 
  a <> b && abs (ax - bx) <= 1 && abs (ay - by) <= 1 && abs (az - bz) <= 1

let neighbors_in set point = Set.inter (neighbors point) set
