(** Library for loading sets of grayscale image files numbered NNNNN.png *)

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

open Mu
open Printf

module SI = Sdlloader
module SV = Sdlvideo
module BA3 = Bigarray.Array3

let imageset_data_type = Bigarray.float32 
let imageset_layout = Bigarray.c_layout
type ba3 = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array3.t

let create (w, h, d) = BA3.create imageset_data_type imageset_layout w h d
let dims img = (BA3.dim1 img, BA3.dim2 img, BA3.dim3 img)

let load start finish path =
  let filename index = sprintf "%s%05d.png" path index in
  let get_image index = SI.load_image (filename (index + start)) in

  (* Load the first image to get the width and height *)
  let init_img = get_image 0 in 
  let (width, height, _) = SV.surface_dims init_img in
  let out_arr = create (width, height, finish - start) in

  for img_i=0 to finish - start - 1 do
    let img = get_image img_i in
    let get_pixel x y = let (r,_,_) = SV.get_pixel_color img x y in r in
    for x = 0 to width-1 do 
      for y = 0 to height-1 do
        out_arr.{x,y,img_i} <- (float (get_pixel x y)) /. 255.
      done
    done
  done ;
  out_arr

let foldl_img_idx kons knil img =
  let res = ref knil in
  for i = 0 to BA3.dim1 img - 1 do
    for j = 0 to BA3.dim2 img - 1 do
      for k = 0 to BA3.dim3 img - 1 do 
        res := kons i j k !res
      done
    done
  done ;
  !res 

let foldl_img kons knil img = 
  let kons_idx i j k = kons img.{i,j,k} in
  foldl_img_idx kons_idx knil img

let update_img f img =
  let kons_idx i j k () = img.{i,j,k} <- f img.{i,j,k} in
  foldl_img_idx kons_idx () img

let normalize_img img =
  let (min, max) = foldl_img min_max_kons (img.{0,0,0}, img.{0,0,0}) img in
  let normalize x = (x -. min) /. (max -. min) in 
  update_img normalize img

let threshold_points threshold img =
  let kons i j k points = 
    if img.{i,j,k} > threshold 
    then (i,j,k) :: points 
    else points in
  foldl_img_idx kons [] img

let threshold_img threshold img =
  let result = create (dims img) in
  let kons i j k _ =
    result.{i,j,k} <- if img.{i,j,k} > threshold then 1. else 0. ; ()
  in
  foldl_img_idx kons () img ; result

(*
let binomial_blur n img =
  let result = create (dims img) in
  let rec value i j k = function
      0 -> let (w,h,d) = dims img in img.{i % w, j % h, k % d}
    | n -> (  value (i + 1) j k (n - 1) +. value (i - 1) j k (n - 1)
           +. value i (j + 1) k (n - 1) +. value i (j - 1) k (n - 1)
           +. value i j (k + 1) (n - 1) +. value i j (k - 1) (n - 1) ) /. 6.
  in
  let kons i j k _ = result.{i,j,k} <- value i j k n ; () in
  foldl_img_idx kons () img ; result
*)


(** this may not work beyond n = 170, since even 32 bit floating point numbers
cannot represent 171 factorial. *)
let binomial_dist n =
  let rec fact = function 0 -> 1. | n -> float n *. fact (n - 1) in
  let binom n k = fact n /. (fact k *. fact (n - k) *. (2. ** float n)) in
  Array.init (n + 1) (binom n) ;;

(** convolve an image by a matrix. *)
let convolve mat img =
  let (mw, mh, md) = dims mat in
  let (iw, ih, id) as idims = dims img in
  let even n = n mod 2 == 0 in
  if even mw || even mh || even md then 
    invalid_arg "The convolution matrix must have all dimensions odd." 
  else
  let cval i j k =
    let imgd mi mj mk =
      let ic icoord mcoord mdim = icoord + (mcoord - mdim - 1 / 2) in
      let imi = ic i mi mw and imj = ic j mj mh and imk = ic k mk md in
      if imi < 0 || imi >= iw || imj < 0 || imj >= ih || imk < 0 || imk >= id
      then 0.
      else img.{imi, imj, imk}
    in
    let cv = ref 0. in
    for mi = 0 to mw - 1 do for mj = 0 to mh - 1 do for mk = 0 to md - 1 do
      cv := !cv +. imgd mi mj mk *. mat.{mi, mj, mk}
    done done done ;
    !cv
  in
  let result = create idims in
  for i = 0 to iw - 1 do for j = 0 to ih - 1 do for k = 0 to id - 1 do
    result.{i,j,k} <- cval i j k
  done done done ;
  result

let binomial_blur n img =
  let mat = Bigarray.Array3.of_array imageset_data_type imageset_layout 
    (Array.make 1 (Array.make 1 (binomial_dist n))) in
  convolve mat img
