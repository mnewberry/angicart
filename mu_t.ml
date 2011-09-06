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

let _ = run_test_tt_main ("Mu" >::: [
  "sgn" >:: check (sgn 10 = 1) ;
  "sgn" >:: check (sgn (-10) = (-1)) ;

  "divrnd" >:: check (divrnd   1    2  =   1 ) ;
  "divrnd" >:: check (divrnd   1  (-2) = (-1)) ;
  "divrnd" >:: check (divrnd (-1)   2  = (-1)) ;
  "divrnd" >:: check (divrnd (-1) (-2) =   1 ) ;

  "divrnd" >:: check (divrnd   4    3  =   1 ) ;
  "divrnd" >:: check (divrnd   4  (-3) = (-1)) ;
  "divrnd" >:: check (divrnd (-4)   3  = (-1)) ;
  "divrnd" >:: check (divrnd (-4) (-3) =   1 ) ;

  "divrnd" >:: check (divrnd   5    3  =   2 ) ;
  "divrnd" >:: check (divrnd   5  (-3) = (-2)) ;
  "divrnd" >:: check (divrnd (-5)   3  = (-2)) ;
  "divrnd" >:: check (divrnd (-5) (-3) =   2 ) ;

  "divrnd" >:: check (divrnd   1    3  = 0) ;
  "divrnd" >:: check (divrnd   1  (-3) = 0) ;
  "divrnd" >:: check (divrnd (-1)   3  = 0) ;
  "divrnd" >:: check (divrnd (-1) (-3) = 0) ;

  "sigmod1" >:: check (sigmod   0 3 =   0) ;
  "sigmod2" >:: check (sigmod   1 3 =   1) ;
  "sigmod3" >:: check (sigmod   2 3 = (-1)) ;
  "sigmod4" >:: check (sigmod (-1) 3 = (-1)) ;
  "sigmod5" >:: check (sigmod (-2) 3 =   1) ;

  "fold" >:: check (1+2+3 = fold (+) 0 [1;2;3]) ;
  "rev" >:: check ([3;2;1] = rev [1;2;3]) ;
  "map" >:: check ([2;3;4;] = map ((+) 1) [1;2;3]) ;
  "app" >:: check ([1;2;3;4;5] = app [1;2;3] [4;5]) ;
  "catmap" >:: check ([1;2;2;3;3;4] = catmap (fun x -> [x;x+1]) [1;2;3]) ;
  "cat" >:: check ([1;2;3;4;5;6] = cat [[1;2;3];[4;5];[6]]) ;
  "min_max" >:: check ((1, 5) = min_max[1;3;5;2;4]) ;
  "take" >:: check ([1;2] = take 2 [1;2;3;4;5]) ;
  "repeat" >:: check ([1;1;1;1;1] = repeat 5 1) ;
  "defualt" >:: check (default 1 (Some 2) = 2) ;
  "defualt" >:: check (default 1 None = 1) ;

  "fold_eqc" >:: check (
     let eq a b = (a mod 3 = b mod 3) in
     let kons es v = List.length es :: v in
     [1;2;3] = List.sort compare (fold_eqc eq kons [] [1;4;7;2;5;3])) ;
]) ;;
