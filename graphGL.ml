(** Out of the box OpenGL display of graphs with many nodes and edges *)

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

open Mu ;;

(** [edges] is a list in the form [[src1; dst1; src2; dst2; ... ; srcN; dstN]].
Colors are specified in RGBA by integers from 0 to 255. *)
let display_loop (*~idle*) ~edges ~edge_colors ~vertices ~vertex_colors =
  let vertex_list_to_raw3f points length (dimi, dimj, dimk) =
    (* This arcane Raw type is a reason to switch away from lablgl *)
    (* There is a new (better) opengl module that has not made it into macports 
       yet: http://www.linux-nantes.org/~fmonnier/OCaml/GL/ *)
    let points_raw = Raw.create_static `float (3 * length) in
    let (scx, scy, scz) = (float dimi, float dimj, float dimk) in
    let transform (i, j, k) = [ float i /. scy -. (0.5 *. scx /. scy) ;
                                float (dimj - j) /. scy -. (0.5 *. scy /. scy) ;
                                float k /. scy -. (0.5 *. scz /. scy) ] in
    let transformed_array = (Array.of_list (catmap transform points)) in
    if length > 0 then Raw.sets_float points_raw 0 transformed_array else ();
    points_raw in
  
  let color_list_to_raw4b points length =
    let points_raw = Raw.create_static `ubyte (4 * length) in
    if length > 0 then Raw.sets points_raw 0
      (Array.of_list (catmap (fun (i,j,k,l) -> [i;j;k;l]) points))
    else () ;
    points_raw in
  
  (*let vcross (ax, ay, az) (bx, by, bz) = 
    (ay *. bz -. az *. by, az *. bx -. ax *. bz, ax *. by -. ay *. bx) in *)

  let ijk_dims = 
    let (vis, vjs, vks) = unzip3 vertices in
    let dim xs = match min_max xs with (min, max) -> max - min in
    (dim vis, dim vjs, dim vks) in
  (*(let (i, j, k) = ijk_dims in printf "[GraphGL] %dx%dx%d array\n%!" i j k);*)
  let vertices_count = List.length vertices in
  let vertices_raw = vertex_list_to_raw3f vertices vertices_count ijk_dims in
  let vertex_colors_raw = color_list_to_raw4b vertex_colors vertices_count in
  let edges_count = List.length edges in
  let edges_raw = vertex_list_to_raw3f edges edges_count ijk_dims in
  let edge_colors_raw = color_list_to_raw4b edge_colors edges_count in

  let display () =
    GlClear.color (0.25, 0.25, 0.25) ;
    GlClear.clear [`color] ;

    GlArray.enable `vertex ;
    GlArray.enable `color ;

    GlArray.vertex `three vertices_raw ;
    GlArray.color `four vertex_colors_raw ;
    GlArray.draw_arrays `points 0 vertices_count ;

    GlArray.vertex `three edges_raw ;
    GlArray.color `four edge_colors_raw ;
    GlArray.draw_arrays `lines 0 edges_count ;

    Gl.flush () ;
    Glut.swapBuffers () ;
  in
  let mouse_l_state = ref Glut.UP and mouse_r_state = ref Glut.UP in
  let key_y_state = ref false and key_z_state = ref false in
  let last_x = ref 0 and last_y = ref 0 in
  let mouse ~button ~state ~x ~y = 
    (match button with
        Glut.LEFT_BUTTON -> mouse_l_state := state 
      | Glut.RIGHT_BUTTON -> mouse_r_state := state
      | _ -> ()); 
    last_x := x ; last_y := y ;
  in 
  let view_theta = ref 0. and view_phi = ref 0. and 
      view_z = ref 0. and view_y = ref 0. in 
  let view_width = ref 640 and view_height = ref 640 in
  let reproject () = 
    GlMat.mode `projection ;
    GlMat.load_identity () ;
    GluMat.perspective 75.0 
      ((float !view_width) /. (float !view_height)) (0.001, 20.0) ;
    GlMat.mode `modelview ; 
    GlMat.load_identity () ;
    GluMat.look_at 
      (sin !view_theta *. cos !view_phi, 
       sin !view_phi, 
       cos !view_theta *. cos !view_phi) 
      (0.0, 0.0, 0.0) (0.0, 1.0, 0.0) ;
    (*GlMat.rotate ~angle:!view_theta ~x:0. ~y:1. ~z:0. () ;
    GlMat.rotate ~angle:!view_phi 
      ~x:(cos !view_phi) ~y:(sin !view_phi) ~z:0. () ; *)
    GlMat.mode `projection ;
    GlMat.translate ~x:0. ~y:!view_y ~z:!view_z () ;
    GlMat.mode `modelview ;
    Glut.postRedisplay () ;
  in
  let motion ~x ~y =
    let clip a b v = if v < a then a else if v > b then b else v in
    let fmod x v = if v < 0. then v +. x else if v > x then v -. x else v in
    if !mouse_l_state == Glut.DOWN then (
      view_theta := fmod (2. *. 3.14159)
        (!view_theta +. (float (x - !last_x)) /. 100.) ;
      view_phi := clip (-1.57078) 1.57078
        (!view_phi +. (float (y - !last_y)) /. 100.) ;
    ) ;
    if (!mouse_r_state == Glut.DOWN) || !key_z_state then (
      view_z := !view_z +. (float (y - !last_y)) /. 500.
    ) ;
    if (!mouse_r_state == Glut.DOWN) || !key_y_state then (
      view_y := !view_y +. (float (y - !last_y)) /. 500.
    ) ;
    last_x := x ; last_y := y ;
    reproject () ;
  in
  let keyboard ~key ~x ~y =
    (* quit on ASCII escape, Q and q, respectively *)
    if key == 121 then key_y_state := true ;
    if key == 122 then key_z_state := true ;
    if key == 27 || key == 81 || key == 113 then exit 0
  in
  let keyboard_up ~key ~x ~y =
    (* quit on ASCII escape, Q and q, respectively *)
    if key == 121 then key_y_state := false ;
    if key == 122 then key_z_state := false
  in
  let reshape ~w ~h = 
    GlDraw.viewport 0 0 w h ;
    view_width := w ; view_height := h ;
    reproject () ;
    display () ;
  in
  ignore (Glut.init Sys.argv) ;
  Glut.initDisplayMode ~double_buffer:true ~index:false ~alpha:true () ;
  Glut.initWindowSize ~w:800 ~h:600 ;
  ignore (Glut.createWindow ~title:"graphgl") ;
  (* Enable anti-aliased lines *)
  Gl.enable `point_smooth ; Gl.enable `line_smooth ; Gl.enable `blend ;
  GlFunc.blend_func `src_alpha `one_minus_src_alpha ;
  GlDraw.line_width 1.5 ;
  GlDraw.point_size 4.0 ;
  (* Set up display functions *)
  Glut.displayFunc ~cb:display ;
  Glut.reshapeFunc ~cb:reshape ;
  Glut.keyboardFunc ~cb:keyboard ;
  Glut.keyboardUpFunc ~cb:keyboard_up ;
  Glut.mouseFunc ~cb:mouse ;
  Glut.motionFunc ~cb:motion ;
  Glut.passiveMotionFunc ~cb:motion ;
  (*Glut.idleFunc ~cb:(Some idle) ;*)
  Glut.mainLoop () ;;
