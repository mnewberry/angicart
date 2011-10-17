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

(** Command-line utility for converting png image sets into point lists *)

let set_check ref v = 
  if !ref = None then ref := Some v
  else raise (Arg.Bad "Only one dir argument is supported") ;;

let dir = ref None and rmin = ref None and rmax = ref None ;;
let threshold = ref None and outp = ref None and binn = ref None ;;
let set_dir fn = match fn with "" -> () | fn -> set_check dir fn ;;
let set_min = set_check rmin ;;
let set_max = set_check rmax ;;
let set_threshold = set_check threshold ;;
let set_outp = set_check outp ;;
let set_binn = set_check binn ;;

let usage = "Usage: pngs2pls -d dir -r min max -t threshold -o output" ;;
let argspec = [
  ("-d", Arg.String set_dir, "Source directory for images named NNNNN.png");
  ("-r", Arg.Tuple [Arg.Int set_min; Arg.Int set_max], "range");
  ("-t", Arg.Float set_threshold, "Use thresholding with value N.NNN");
  ("-n", Arg.Int set_binn, "Number of steps in blur (the n in n choose k)");
  ("-o", Arg.String set_outp, "Output file")] ;;

let argsfail _ = Arg.usage argspec usage ; exit 1 ;;
Arg.parse argspec argsfail usage ;;
match (!dir, !rmin, !rmax, !threshold) with 
  (Some _, Some _, Some _, Some _) -> () | _ -> argsfail () ;;

let req z = match !z with Some x -> x | _ -> invalid_arg "Fail" ;;

let mri_img = ImageSet.load (req rmin) (req rmax) ((req dir) ^ "/") ;;
ImageSet.normalize_img mri_img ;;
let blur_img = ImageSet.binomial_blur (req binn) mri_img in
  Mu.dump_obj (Mu.default "out.blurred" !outp) blur_img
  
