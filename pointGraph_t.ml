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

open OUnit
open Mu

let check x _ = assert_bool "false" x

module PG = PointGraph
module L = List

let tri1 = [
  (0,0,0);(0,0,1);(0,0,2);(0,0,3);
  (0,1,0);(0,1,1);(0,1,2);
  (0,2,0);(0,2,1);
  (0,3,0)] ;;

let xpppp (a, b, c) = (a + 2, b, c) ;;

let tri2 = Mu.map xpppp tri1 ;;
let tri3 = Mu.map xpppp tri2 ;;

let tris = PG.create (Point.set (tri1 @ tri2 @ tri3)) ;;

let _ = run_test_tt_main ("PointGraph" >::: [
  "union" >::
    check (PG.equal 
            (PG.create (Point.set (tri1 @ tri2)))
            (PG.union (PG.create (Point.set tri1)) 
                      (PG.create (Point.set tri2)))) ;
  "find_cc" >::
    check (PG.equal
            (PG.create (PG.find_cc (0,0,0) tris))
            (PG.create (Point.set tri1))) ;
  "find_cc" >::
    check (PG.equal
            (PG.create (PG.find_cc (2,0,0) tris))
            (PG.create (Point.set tri2))) ;
  "ccs" >::
    check (3 = L.length (PG.ccs tris)) ;
  "ccs hd equal to one of the ccs" >::
    check (PG.equal (PG.create (Point.set tri1)) (L.hd (PG.ccs tris)))
]) ;;

