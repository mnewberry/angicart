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

let setup _ =
  let img = Bigarray.Array3.create imageset_data_type imageset_layout 2 3 5 in
  Bigarray.Array3.fill img 1.0 ;
  img ;;

let teardown _ = () ;;

let test_foldl_img img =
  assert_equal (2. *. 3. *. 5.) (foldl_img ( +. ) 0.0 img) ;;

let test_update_img img =
  update_img (fun x -> x +. 1.0) img ;
  assert_equal (2. *. 2. *. 3. *. 5.) (foldl_img ( +. ) 0.0 img) ;;

let _ = run_test_tt_main ("imageset" >::: [
  "foldl" >:: (bracket setup test_foldl_img teardown) ;
  "update" >:: (bracket setup test_update_img teardown) ;
]) ;;
