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
open ImageSet
module VT = VesselTree

let nth = List.nth
let fos = float_of_string
let size = List.length
let assert_equali = assert_equal ~printer:string_of_int
let assert_equalf eps = assert_equal 
  ~cmp:(cmp_float ~epsilon:eps) 
  ~printer:string_of_float

(* Calls to nth rowdata refer to the following scheme from vesselTree.ml:
 *   0: tag    3: vol    6: defc   9: parent   12: nchild
 *   1: name   4: rad    7: col    10: beta    13: a
 *   2: len    5: voxc   8: tips   11: gamma   14: b        
 * If the order in vesselTree is modified, these will be wrong. *)

let sumpow xs init = Mu.sumf (Mu.map (fun x -> x ** init) xs)
let newton_vals lst = let e = (VesselTree.newton_exp_solve lst 1.0) in 
  (sumpow lst e, e)

let test_tree_dataset =
  let str = Mu.slurp "local/data/dicom_small.all.tsv" in
  let rows = List.tl (Mu.splitnl str) in
  let assocl = Mu.fold
    (fun row assocl ->
      let data = Mu.splittab row in let name = nth data 1 in
      (name, data) :: assocl) [] rows in
  let test (name, rowdata) =
    let find_all n v = List.filter (fun (_, x) -> nth x n = v) assocl in
    let child_data = Mu.map snd (find_all 9 name) in
    let child_nth n = Mu.map (fun x -> (nth x n)) child_data in
    let (qsum, qexp) = newton_vals (Mu.map float_of_string (child_nth 10)) in
    let (ssum, sexp) = newton_vals (Mu.map float_of_string (child_nth 11)) in
    [ "child_size" >:: (fun _ -> 
        assert_equali (size child_data) (int_of_string (nth rowdata 12))) ;
      "no duplicates" >:: (fun _ -> 
        assert_equali 1 (size (find_all 1 name))) ] @
    if (nth rowdata 13) = "NA" then [] else [
      "qs_newton_exp" >:: (fun _ -> 
        assert_equalf 0.0001 1.0 qsum ) ; 
      "qs" >:: (fun _ -> 
        assert_equalf 0.0001 qexp (float_of_string (nth rowdata 13)))] @
    if nth rowdata 14 = "NA" then [] else [
      "ss_newton_exp" >:: (fun _ -> 
        assert_equalf 0.0001 1.0 ssum) ; 
      "ss" >:: (fun _ -> 
        assert_equalf 0.0001 sexp (float_of_string (nth rowdata 14)))]
  in List.map (fun (name, rowdata) -> name >::: test (name, rowdata)) assocl

let test_newton lst _ = 
  let e = VT.newton_exp_solve lst 1.0 in 
  assert_equalf 0.0001 1.0 (sumpow lst e)

let _ = run_test_tt_main ("tree_dataset" >::: [
  "newton_exp1" >:: test_newton [0.8; 0.8] ;
  "newton_exp2" >:: test_newton [1.2; 1.2] ;
  "newton_exp3" >:: test_newton [0.3; 0.999] ;
  "row_constraints" >::: test_tree_dataset
])
