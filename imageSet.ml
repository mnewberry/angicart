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
open Images
open OImages
open Printf

module BA3 = Bigarray.Array3

let imageset_data_type = Bigarray.float32 
let imageset_layout = Bigarray.c_layout

let load start finish path =
  let filename index = sprintf "%s%05d.png" path index in
  let get_image index = 
    let image = OImages.load (filename (index + start)) [] in
    (match OImages.tag image with
      | Index8 img -> let rgb = img#to_rgb24 in img#destroy ; rgb
      | Index16 img -> let rgb = img#to_rgb24 in img#destroy ; rgb
      | Rgb24 img -> img
      | _ -> invalid_arg (sprintf "Cannot convert color map of %s" 
                                  (filename index)))
  in 

  (* Load the first image to get the width and height *)
  let init_img = get_image 0 in 
  let width = init_img#width and height = init_img#height in
  let out_arr = BA3.create imageset_data_type imageset_layout
                                       width height (finish - start) in
  init_img#destroy ;

  for img_i=0 to finish - start - 1 do
    let img = get_image img_i in
    for x = 0 to width-1 do 
      for y = 0 to height-1 do
        out_arr.{x,y,img_i} <- (float (img#get x y).r) /. 255.
      done
    done
  done ;
  out_arr ;; 

let foldl_img_idx kons knil img =
  let res = ref knil in
  for i = 0 to BA3.dim1 img - 1 do
    for j = 0 to BA3.dim2 img - 1 do
      for k = 0 to BA3.dim3 img - 1 do 
        res := kons i j k !res
      done
    done
  done ;
  !res ;;

let foldl_img kons knil img = 
  let kons_idx i j k = kons img.{i,j,k} in
  foldl_img_idx kons_idx knil img ;;

let update_img f img =
  let kons_idx i j k () = img.{i,j,k} <- f img.{i,j,k} in
  foldl_img_idx kons_idx () img ;;

let normalize_img img =
  let (min, max) = foldl_img min_max_kons (img.{0,0,0}, img.{0,0,0}) img in
  let normalize x = (x -. min) /. (max -. min) in 
  update_img normalize img ;;

let threshold_points threshold img =
  let kons i j k points = 
    if img.{i,j,k} > threshold 
    then (i,j,k) :: points 
    else points in
  foldl_img_idx kons [] img ;;



