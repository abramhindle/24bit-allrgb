(*
 *  Copyright (c) 2005, 2006, 2007 Abram Hindle
 *  
 *  This file is part of CaptchaBreaker

 *  CaptchaBreaker is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 3 of the License, or
 *  (at your option) any later version.

 *  Foobar is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.

 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

(* 

   Todo:
     1. load images
     2. Sample them, make histograms
     3. take in commandline images
     4. segment those
     5. figure them out

*)

open Images;;
open OImages;;
open Abez;;
open Shape;;

let for_fold = Abez.for_fold;;
let fold_each_xy = Abez.fold_each_xy;;
let iti = Abez.iti;;
let for_xy = Abez.for_xy;;
let get_extension = Abez.get_extension;;
let get_basename = Abez.get_basename;;
let for_map = Abez.for_map ;;
let shortcircuit = Abez.shortcircuit ;;
let shortcircuit_c = Abez.shortcircuit_c ;;


  type ptype = F of float | I of int | S of string | L of (unit -> unit);;
  let rec pj = function
      [] -> print_string "\n";
    | x::xs -> 
	match x with
	    F(f) -> print_float f;
	  | I(i) -> print_int i;
	  | S(s) -> print_string s;
	  | L(x) -> x () ;
     ;
     print_string " ";
     pj xs
  ;;



let invert_in_place bmp =
  let width = bmp#width  in
  let invert v = { r = (255 - v.r); g = (255 - v.g); b = (255 - v.b)} in
  let height = bmp#height  in

  for_xy width height (
    fun x y ->
      bmp#unsafe_set x y (invert (bmp#get x y))
  );
    bmp
;;

(* color threshold *)
let threshold min bmp =
  let height = bmp#height  in
  let width = bmp#width  in
  let black = {r=0;g=0;b=0} in
  let white = {r=255;g=255;b=255} in
  let value v = if (v.r > min ) then white else black in
  for_xy width height (
    fun x y ->
      bmp#unsafe_set x y (value (bmp#get x y))
  );
    bmp
;;

let grayscale  bmp =
  let height = bmp#height  in
  let width = bmp#width  in
  let black = {r=0;g=0;b=0} in
  let white = {r=255;g=255;b=255} in
  let value v = let a = (v.r + v.g + v.b) / 3 in
    {r = a ; g = a; b = a}
      in
  for_xy width height (
    fun x y ->
      bmp#unsafe_set x y (value (bmp#get x y))
  );
    bmp
;;

let clearedges n bmp =
  let height = bmp#height  in
  let width = bmp#width  in
  let white = {r=255;g=255;b=255;} in
  let horline y = 
    for x = 0 to (width - 1) do
      bmp#unsafe_set x y white
    done;
  in
  let verline x = 
    for y = 0 to (height - 1) do
      bmp#unsafe_set x y white
    done;
  in
    for i = 0 to n do
      horline i;
      horline (height-1-i);
      verline i;
      verline (width - i - 1);
    done;
    bmp
;;


let threshold_bucket percent bmp =
  let height = bmp#height  in
  let width = bmp#width  in
  let arr = Array.create 256 0 in
    for_xy width height (fun x y ->
	      let v = bmp#get x y in
	      let a = (v.r + v.g + v.b) / 3 in
		arr.(a) <- arr.(a) + 1 ;
	   );
    let total = Array.fold_left (fun x o -> o + x ) 0 arr in
    let pcnt = total * percent / 100 in
    let index = ref 0 in
    let cnt = ref 0 in
      while (!cnt <= pcnt) do
	cnt := !cnt + arr.(!index);
	index := !index + 1;
      done;
    let index = !index in
      threshold index bmp
;;
    


(* grayscale *)
let stretch_in_place bmp = 
  let width = bmp#width  in
  let height = bmp#height  in
  let npix = width * height in
  let valu c = c.r in
  let (min,total,max) = fold_each_xy (
    fun x y (min,v,max) ->
      let v' = (valu (bmp#get x y)) in
      let min' = if (min < v') then min else v' in
      let max' = if (max > v') then max else v' in
	(min',v',max')
  ) width height (0,0,0) in
  let avg = total / npix in
    
  for_xy width height (
    fun x y ->
      let v' = (valu (bmp#get x y)) in
      let res = (v' - min)*255/(max - min) in
      let rc = {r = res ; g = res ; b = res;} in
	bmp#unsafe_set x y rc;
  );
    bmp
;;
  


let load_rgb_file file =
  let oimage = OImages.load file [] in
    match OImages.tag oimage with
      | Index8 img ->
          let rgb = img#to_rgb24 in
            img#destroy;
            rgb
      | Index16 img ->
          let rgb = img#to_rgb24 in
            img#destroy;
            rgb
      | Rgb24 img -> img
      | _ -> raise (Invalid_argument "not supported") 
;;



(* WARNING INCLUSIVE *)
let for_map_from f from too = 
  let rec foreach = function
      x when x > too -> []
    | x -> (f x) :: (foreach (x+1))
  in
  foreach from
;;

let iteri f l =
  let rec iii index = function
      [] -> ()
    | x::xs ->
	let _ = f (index) x in
	  iii (index+1) xs 
  in
    iii 0 l
;;

let for_iter f max = 
  let rec foreach = function
      x when x >= max -> ()
    | x -> f x;
	foreach (x+1);
  in
  foreach 0
;;


let for_each_xy f width height = 
  for_iter (
    fun y ->
      for_iter ( fun x ->
		   f x y;
	       ) width
  ) height
;;


    

let edge (bmp : rgb24) =
  let width = bmp#width  in
  let height = bmp#height  in

  let rgb24 = new rgb24 width height in

  let f x y =
    let center = bmp#get x y in
    let points =
      Array.init 3 (fun dx ->
	Array.init 3 (fun dy ->
	  try
	    bmp#get (x+dx-1) (y+dy-1)
	  with
	    Out_of_image -> center)) in

    let r = ref 0
    and g = ref 0
    and b = ref 0 in

    let diff = ref 0 in

    let add dx dy weight =
      let rgb = points.(dx+1).(dy+1) in
      r := !r + rgb.r * weight;
      g := !g + rgb.g * weight;
      b := !b + rgb.b * weight;
      let dr = center.r - rgb.r
      and dg = center.g - rgb.g
      and db = center.b - rgb.b
      in
      diff := !diff + truncate (sqrt ( float (dr * dr + dg * dg + db * db) ))
    in

    add (-1)   0  1;
    add   1    0  1;
    add   0  (-1) 1;
    add   0    1  1;
    add (-1) (-1) 1;
    add   1  (-1) 1;
    add (-1)   1  1;
    add   1    1  1;

    (* 0 <= diff <= 3544 *)
    let cw =
      let cw = float !diff /. 3544.0 in
      if cw < 0.0 then 0.0 else
      if cw > 1.0 then 1.0 else cw in

    (* we need to emphasize the difference *)
    let cw = truncate (sqrt cw *. 256.0) in
    let newcolor org =
      let c = org / 8 + cw in
      if c > 255 then 255 else c in

    { r= newcolor center.r;
      g= newcolor center.g;
      b= newcolor center.b } in

  for x = 0 to width -1 do
    for y = 0 to height -1 do
      rgb24#unsafe_set x y (f x y)
    done
  done;
  rgb24;;


let col c =
  (c.r,c.g,c.b)
;;
let is_text c = 
  let (r,g,b) = col c in
  (r < 129 && g  < 129 && b < 129) 
;;
let is_in x y (x1,y1,x2,y2) =  (x >= x1 && x <= x2 && y >= y1 && y <= y2)  ;;
let segmenter ?(istext=is_text) (bmp : rgb24) =
  let width = bmp#width  in
  let height = bmp#height in

  let gp x y = if (is_in x y (0,0,(width-1),(height-1))) then
    istext (bmp#get x y)
  else
    false
  in
  let segments = ref [] in
  let currsegment = ref None in
  let rec expandsegment (cx1,cy1,cx2,cy2) =
    (* across top *)

    let top = shortcircuit_c (
      for_map_from (
	fun x -> 
	  gp x (cy1-1)
      ) (cx1-1) (cx2+1)
    ) in

    let right = shortcircuit_c (for_map_from (fun x -> gp cx2(x+1)) (cy1-1) (cy2+1)) in
    let left = shortcircuit_c (for_map_from (fun x -> gp cx1 (x-1)) (cy1-1) (cy2+1)) in
    let bottom = shortcircuit_c (for_map_from (fun x -> gp x (cy2+1)) (cx1-1) (cx2+1)) in
    let cx1' = if (left) then cx1  - 1 else cx1 in
    let cy1' = if (top) then cy1 - 1 else cy1 in
    let cx2' = if (right) then cx2 + 1 else cx2 in
    let cy2' = if (bottom) then cy2 + 1 else cy2 in
	(* DO A CHECK HERE IF WE SHOULD KEEP GOING *)
      let ret = (cx1',cy1',cx2',cy2') in
	if (top || right || left || bottom) then
	  expandsegment ret
	else
	  ret
    in
    let segments = fold_each_xy 
      (fun x y o ->
	 if (shortcircuit (fun l -> is_in x y l) o) then
	   o
	 else 
	 if (gp x y) then
	   let segment = expandsegment (x,y,x,y) in
	     o @ [segment] 
	 else 
	   o
      ) width height []
    in
    let within_width v =
      (min (max v 0) (width-1)) 
    in
    let within_height v =
      (min (max v 0) (height-1)) 
    in

      List.map (fun (x1,y1,x2,y2) ->
		  let x1 = within_width x1 in
		  let x2 = within_width x2 in
		  let y1 = within_height y1 in
		  let y2 = within_height y2 in

		  let w = x2 - x1 + 1 in
		  let h = y2 - y1 + 1 in
		  let b = bmp#sub x1 y1 w h in
		    	 (b,(x1,y1,x2,y2)) 
(*		    b *)
	       ) segments
;;

let w_h_of_segment (x1,y1,x2,y2) =
  let w = x2 - x1 + 1 in
  let h = y2 - y1 + 1 in
    (w,h)
;;
let size_of_segment x =
  let (w,h) = w_h_of_segment x in
    w * h 
;;
let segment_of b (x1,y1,x2,y2) =
  let (w,h) = w_h_of_segment (x1,y1,x2,y2) in
  b#sub x1 y1 w h
;;
  

let segment_size (x1,y1,x2,y2) =
  (x2 - x1) * (y2 - y1)
;;

let rec segment_filter size = function
    [] -> []
  | (bmp,x)::xs -> 
      if ((segment_size x) < size) then
	segment_filter size xs
      else
	(bmp,x) :: (segment_filter size xs)
;;

let segment_sort segments =
  List.sort (fun (_,(x,_,_,_)) (_,(y,_,_,_)) -> 
			      if (x = y) then 
				0
			      else if (x > y) then
				1
			      else
				-1
	    ) segments ;;
  
let is_image_file file =
  let ext = String.lowercase (get_extension file) in

   (ext = "png" || ext = "jpg" || ext = "gif" || ext = "jpeg" || ext = "bmp")
;;

let chars = [ "A" ; "B" ; "C" ; "D" ; "E" ; "F" ;
	      "G" ; "H" ; "I" ; "J" ; "K" ; "L" ;
	      "M" ; "N" ; "O" ; "P" ; "Q" ; "R" ;
	      "S" ; "T" ; "U" ; "V" ; "W" ; "X" ;
	      "Y" ; "Z" ; "0" ; "1" ; "2" ; "3" ;
	      "4" ; "5" ; "6" ; "7" ; "8" ; "9" ] ;;

let load_dir_chars values directory =
  (*  print_string "Loading phphash!\n"; *)


  let l = List.map (
    fun ch ->
      let dir = directory^"/"^ch in
	(*	print_string dir ;
		print_string "\n"; *)
      let files = Sys.readdir dir in
	(*	List.iter (fun x -> print_string x ; print_string "\n";) (Array.to_list files) ; *)
      let files = List.filter (is_image_file) (Array.to_list files) in
	(*	List.iter (fun x -> print_string x ; print_string "\n";) files ; *)
      let bmps = List.map (
	fun x ->
	  let path = dir ^ "/" ^  x in
	  let bmp = load_rgb_file path in
	    (bmp,x)
      ) files in
	
	(ch,bmps)
  ) values
  in
    List.flatten (
      List.map (fun (label,l2) ->
		  List.map (fun x ->
			      (label,x) 
			   ) l2
	       ) l
    )
;;
let load_dir directory = load_dir_chars chars directory;;

let load_phphash () =
  List.map (fun (s,(b,f)) -> (s,b)) (load_dir "fonts/phpbb")
;;

(* let bmp_to_list ?(istext=is_text)   b = *)
let bmp_to_list   b =
  let width = b#width in
  let height = b#height in
  (* SLOW *)
  let pt_list = fold_each_xy 
      (fun x y o ->
	 if (is_text (b#get x y)) then
	   (Shape.mk_pti x y) :: o
	 else 
	   o
      ) width height [] in
    pt_list
;;

let sample_conv n b =
  Shape.sample_n n (bmp_to_list b) 
;;

let load_aimhash cnt angles llength  =
  let list = load_dir "fonts/aim" in
(*   let _ = print_string "Dir loaded\n" in *)
    List.map (
      fun (n,(b,f)) ->
	let pts = sample_conv cnt b in
(* 	print_string (n ^ " converted to bmp \n"); *)
(*	let pnt = most_average_pt pts in *)
(*	print_string (n ^ " most average"^(string_of_point pnt)^"\n"); *)
	let histogram = biglog_histogram angles llength cnt pts in
	(n,histogram,f,pts,b)
    ) list 
;;

		
let bool2int b = if b then 1 else 0 ;;
  
let maxlist f l =
  let head::tail = l in
  let rec max m v = function
    [] -> m
    | x::xs ->
	let fx = f x in
	if (fx > v) then
	  max x fx xs
	else
	  max m v xs
  in
    max head (f head) tail
;;

let quickcompare f b1 b2 =
  let bw1 = b1#width  in
  let bw2 = b2#width  in
  let bh1 = b1#height in
  let bh2 = b2#height in
  let (a,b,c) = fold_each_xy (
    fun x y (a,b,c) ->
      let ab = (f b1 bw1 bh1 x y) in
      let bb = (f b2 bw2 bh2 x y) in
      let a' = bool2int  ab in
      let b' = bool2int  bb in
      let c' = bool2int (ab && bb) in
      let a = a + a' in
      let b = b + b' in
      let c = c + c' in
	(a,b,c)
  ) bw1 bh1 (0,0,0) in
    ((float_of_int c) /. (float_of_int b) +. (float_of_int c) /. (float_of_int a)) /. 2.
;;
    
let phpbbsolver ?(phphash = [])(bmp : rgb24) =
  let phphash = if ((List.length phphash) > 0) then phphash else load_phphash () in
  let segments = segmenter bmp in
  let segments = segment_filter 100 segments in
  let segments = segment_sort segments in
  let istext c = 
    let (r,g,b) = col c in
      (r < 129 && g  < 129 && b < 129) 
  in
  let qp bmp width height x y = 
    if (is_in x y (0,0,(width-1),(height-1))) then
      istext (bmp#get x y)
    else
      false
  in
  String.concat "" (
    List.map ( 
      fun (bmp,segment) ->
	let matches = List.map (
	  fun (label,x) -> 
	    let v = quickcompare qp bmp x in 
	      (v,label,x) 
	) phphash in
	let (value,label,matchbmp) = maxlist (fun (v,_,_) -> v) matches in
	  label
    ) segments
  )
      
;;



let solver_main solver files =
  List.iter (fun file ->
	       try
		 let rgb = load_rgb_file file in
		 let str = solver rgb in
		   pj [S(file);S(str)];
	       with _ -> ()) files
;;

let is_black v = v.r = 0 ;;
exception Fill1;;
exception Fill2;;
  
let maxregion (x1,y1,x2,y2) (x3,y3,x4,y4) =
  let min x y = if (x > y) then y else x in
  let max x y = if (x < y) then y else x in
  let x1' = min x1 x3  in
  let x2' = max x2 x4  in
  let y1' = min y1 y3  in
  let y2' = max y2 y4  in

    (x1',y1',x2',y2')
;;
let print_region (x1,y1,x2,y2) =
  print_string ("Region "^
		  (string_of_int x1) ^ "," ^
		  (string_of_int y1) ^ " " ^
		  (string_of_int x2) ^ "," ^
		  (string_of_int y2) ^ " " ^ "\n"
	       );
;;

let gen_fill width height found_in_main found_in_buf setter x y =
  let rec fill fregion cnt  x y =
    if (  (is_in x y (0,0,(width-1),(height-1))) &&
      (found_in_main x y) && not(found_in_buf x y )) then
      let _ = if (x >= width ) then raise Fill1 in
      let _ = if (y >= height) then raise Fill1 in
      let _ = setter x y in
      let cnt = cnt + 1 in
      let fregion = maxregion fregion (x,y,x,y) in
(*      let l = [(1,1);(0,1);((-1),1);((-1),0);((-1),(-1));(0,(-1));(1,(-1));(1,0)] in *)
      let l = [(0,1);((-1),0);(0,(-1));(1,0)] in
      let (cnt',fregion') = List.fold_left (
	fun (c,f) (dx,dy) ->
	  fill f c (x + dx) (y + dy)
      ) (cnt,fregion) l in
	(cnt',fregion')
    else
      (cnt,fregion) 
  in
    fill (x,y,x,y) 0 x y
;;

let gfill ?diag:(diagonal=false) bmp width height gp gp2 setter textcol x y tobmp =
  let rec fill fregion cnt  x y tobmp =
    if ((gp bmp x y) && not(gp2 tobmp x y )) then
      let _ = if (x >= width ) then raise Fill1 in
      let _ = if (y >= height) then raise Fill1 in
      let _ = setter tobmp x y textcol in
      let cnt' = cnt + 1 in
      let fregion' = maxregion fregion (x,y,x,y) in
	(*      let _ = print_region fregion in *)
      let (cnt1,fregion1) = fill fregion' cnt' (x+1) (y) tobmp in
      let (cnt2,fregion2) = fill fregion1 cnt1 (x) (y+1) tobmp in
      let (cnt3,fregion3) = fill fregion2 cnt2 (x-1) (y) tobmp in
      let (cnt4,fregion4) = fill fregion3 cnt3 (x) (y-1) tobmp in
	if (diagonal) then
	  let (cnt5,fregion5) = fill fregion4 cnt4 (x+1) (y+1) tobmp in
	  let (cnt6,fregion6) = fill fregion5 cnt5 (x-1) (y+1) tobmp in
	  let (cnt7,fregion7) = fill fregion6 cnt6 (x-1) (y-1) tobmp in
	  let (cnt8,fregion8) = fill fregion7 cnt7 (x+1) (y-1) tobmp in
	  (cnt8,fregion8)
	else
	  (cnt4,fregion4)
    else
      (cnt,fregion) 
  in
    fill (x,y,x,y) 0 x y tobmp
;;
let fill ?diag:(diagonal=false) bmp gp textcol x y tobmp =
  let width = bmp#width in
  let height = bmp#height in
  let setter tobmp x y col = tobmp#set x y textcol in
    gfill ~diag:diagonal bmp width height gp gp setter textcol x y tobmp
;;
(*
let fill bmp gp textcol x y tobmp =
  let width = bmp#width in
  let height = bmp#height in
  let rec fill fregion cnt  x y tobmp =
    if ((gp bmp x y) && not(gp tobmp x y )) then
      let _ = if (x >= width ) then raise Fill1 in
      let _ = if (y >= height) then raise Fill1 in
      let _ = tobmp#set x y textcol in
      let cnt' = cnt + 1 in
      let fregion' = maxregion fregion (x,y,x,y) in
(*      let _ = print_region fregion in *)
      let (cnt1,fregion1) = fill fregion' cnt' (x+1) (y) tobmp in
      let (cnt2,fregion2) = fill fregion1 cnt1 (x) (y+1) tobmp in
      let (cnt3,fregion3) = fill fregion2 cnt2 (x-1) (y) tobmp in
      let (cnt4,fregion4) = fill fregion3 cnt3 (x) (y-1) tobmp in
	(cnt4,fregion4)
    else
      (cnt,fregion) 
  in
    fill (x,y,x,y) 0 x y tobmp
 ;;
*)

let get_white_buffer width height =
  let bgcol = {r=255;g=255;b=255} in
  let buffer = new rgb24_filled width height bgcol in
    buffer 
;;

let fill_segmenter ?diag:(diagonal=false) ?(istext=is_text) (bmp : rgb24) =
  let width = bmp#width  in
  let height = bmp#height in
  let textcol = {r=0;g=0;b=0} in
  let bgcol = {r=255;g=255;b=255} in
(*  let buffer = new rgb24_filled width height bgcol in *)
  let buffer =  get_white_buffer width height in
  let gp tb x y = 
    if (is_in x y (0,0,(width-1),(height-1))) then
      istext (tb#get x y)
    else
      false
  in
  let fill fregion cnt  x y tobmp = fill ~diag:diagonal bmp gp textcol x y tobmp in
    
  let clear_buf (x1,y1,x2,y2) =
    for y = y1 to y2 do
      for x = x1 to x2 do
	let _ = if (x >= width ) then raise Fill2 in
	let _ = if (y >= height) then raise Fill2 in
	buffer#set x y bgcol
      done
    done
  in

  let get_bmp_from_buf (x1,y1,x2,y2) =
    let _ = if (x1 >= width ) then raise Fill2 in
    let _ = if (y1 >= height) then raise Fill2 in
    let _ = if (x2 >= width ) then raise Fill2 in
    let _ = if (y2 >= height) then raise Fill2 in

    let w = x2 - x1 + 1 in
    let h = y2 - y1 + 1 in
    let b = buffer#sub x1 y1 w h in
      b
  in
  let get_xy1 (a,b,c,d) = (a,b) in
  let is_actually_in x y (b,l) =
    let width = b#width in
    let height = b#height in
      if (is_in x y l) then
	let (x1,y1) = get_xy1 l in
	let x' = x - x1 in
	let y' = y - y1 in
	  if (x' < width && y' < height && x' >= 0 && y' >= 0 ) then
	    istext (b#get x' y')
	  else
	    false
      else
	false
  in
  let segments = fold_each_xy 
    (fun x y o ->
(*    if (shortcircuit (fun l -> is_actually_in x y l) o) then  *)
    if (shortcircuit (fun (b,l) -> is_in x y l) o) then 
	 o
       else 
	 if (gp bmp x y) then
	 let (cnt,region) = fill (x,y,x,y) 0 x y buffer in
	   if ( cnt > 20 ) then 
	     let b = get_bmp_from_buf region in
	       o @ [(b,region)] 
	   else
	     let _ = clear_buf region in
	       o
       else 
	 o
    ) width height []
  in
    buffer#destroy;
    segments
;;

let region_to_xywh (x1,y1,x2,y2) =
  (x1,y1,(x2-x1)+1,(y2-y1)+1)
;;
  
let get_segment_of buffer (_,r) =
  let (x1,y1,w,h) = region_to_xywh r in
  let outb = buffer#sub x1 y1 w h in
    outb
;;

type fills = Edge | Possible | Background | Unknown ;;
let string_of_fills = function
    Edge -> "Edge"
  | Possible -> "Possible"
  | Background -> "Background"
  | Unknown -> "Unknown"
;;
exception UnknownFound;;
exception SegmentationError;;
let stupid_segmenter (b,r) =
  let width = 25 in
  let err = 10 in
  let (ox,oy,w,h) = region_to_xywh r in
  let n = w / width in
  match n with
      0 -> [(b,r)]
    | 1 -> [(b,r)]
    | 2 -> 
	let r1 = (0,0,width+err/2,h-1) in
	let r2 = (w-width-err/2,0,w-1,h-1) in
	let b1 = get_segment_of b (b,r1) in
	let b2 = get_segment_of b (b,r2) in
	  [(b1,r1);(b2,r2)]
    | x when x > 2 -> 
	let r1 = (0,0,width+err/2,h-1) in
	let r2 = (w-width-err/2,0,w-1,h-1) in
	let b1 = get_segment_of b (b,r1) in
	let b2 = get_segment_of b (b,r2) in
	let l = for_map (fun x ->
		     let x = x + 1 in
		     let r' = ((x*width-(err/2)),0,((x+1)*width+(err/2)),(h-1)) in
		     let b' = get_segment_of b (b,r') in
		       (b',r')
		  ) (x - 2) in

	let ll = List.flatten [[(b1,r1)] ; l ; [(b2,r2)]] in
	  ll
    | _ -> raise SegmentationError 
	
;;

(* stupid idea:
   scan left to right count pixels get min and max and split at those points
*)

let color_subsegmenter (b,r) rgb =
  let (ox,oy,w,h) = region_to_xywh r in
  let width = w in
  let height = h in
  let buff = get_segment_of rgb (b,r) in
  let tmpbuff = Array.make_matrix w h Unknown in
  let fillbuff = Array.make_matrix w h false in
    (* fill from the borders *)
  let in_main x y = 
    if (is_in x y (0,0,(width-1),(height-1))) then
      is_text (b#get x y)
    else
      false
  in
  let in_buf x y =
    if (is_in x y (0,0,(width-1),(height-1))) then
      tmpbuff.(x).(y) = Background
    else
      false
  in
  let bgsetter x y =
    tmpbuff.(x).(y) <- Background
  in
    (* fill in edges with Background goo *)
    for x = 0 to (w-1) do
      let _ = gen_fill w h in_main in_buf bgsetter x 0 in
      let _ = gen_fill w h in_main in_buf bgsetter x (h-1) in
	()
    done;
    for y = 0 to (h-1) do
      let _ = gen_fill w h in_main in_buf bgsetter 0 y in
      let _ = gen_fill w h in_main in_buf bgsetter (w-1) y in
	()
    done;
    (* fill in the rest *)
    for_xy w h (
      fun x y ->
	match tmpbuff.(x).(y) with
	    Unknown -> 
	      tmpbuff.(x).(y) <- if (in_main x y) then Edge else Possible ;
	  |  _ -> ()
    );
    let yh = h / 2 in
      (* shoot a ray down the middle find possible fills *)
    let targets = for_fold (
      fun x o ->
	match tmpbuff.(x).(yh)  with
	    Unknown -> raise UnknownFound;
	  | Possible -> (Possible,x,yh) :: o
	  | Edge -> o (* (Edge,x,yh) :: o *)
	  | Background -> o
    ) (w) [] in
      
      (* do color fill to a buffer, record segments *)
    let dist col col2 =
      let r = abs ( col.r - col2.r ) in
      let g = abs ( col.g - col2.g ) in
      let b = abs ( col.b - col2.b ) in
	r+g+b
    in
    let clear_fill (x1,y1,x2,y2) =
      for x = x1 to x2 do
	for y = y1 to y2 do
	  fillbuff.(x).(y) <- false
	done
      done
    in
    let tmp_ok x y =
	match tmpbuff.(x).(y)  with
	    Unknown -> false
	  | Possible -> true
	  | Edge -> true
	  | Background -> false
     in
      


    let dist_threshold = 40 in
    List.iter (fun (t,x,y) ->
	pj [S(string_of_fills t);I(x);I(y)];
	      ) targets;

    let bget x y =
      if (x >= w || y>=h || x < 0 || y < 0) then
	{r=0;g=0;b=0}
      else
	buff#get x y
    in
    let minpixels = 100 in
    let fills = List.map (
      fun (t,x,y) ->
	let col = bget x y in
	let inmain x y = (tmp_ok x y) && (dist_threshold > (dist col (bget x y))) in
	let inbuf  x y = fillbuff.(x).(y) in
	let setter x y = fillbuff.(x).(y) <- true in
	let (c,r) = gen_fill w h inmain 	  inbuf 	  setter x y in
	let _ = print_int c in
	let _ = print_region r in
	let _ = if (c < minpixels) then clear_fill r in
	  (*let _ = clear_fill r in*)    (* if we don't we reduce duplicates? *)
	  (c,r)
    ) targets in

    let bigfills = List.filter (fun (c,r) -> c > minpixels) fills in    List.map (fun (c,r) ->
		let buf = get_segment_of b (0,r) in
		  (buf,r)
	     ) 
      bigfills
;;

let bmp_iter bmp f =
  let height = bmp#height  in
  let width = bmp#width  in  
    for i = 0 to width - 1 do
      for j = 0 to height - 1 do
        f i j (bmp#get i j)
      done;
    done;
;;

  

let color_replace_inplace bmp color_find color_replace =
  let height = bmp#height  in
  let width = bmp#width  in  
    bmp_iter bmp 
      (fun x y v ->
         if (v = color_find) then
           bmp#unsafe_set x y color_replace
      )
;;

let min_color bmp =
  let minc = ref (bmp#get 0 0) in
  bmp_iter bmp 
      (fun x y v ->
         if (v < !minc) then
           minc := v;
      );
    !minc
;;

  

let bmp_copy bmp =
  let height = bmp#height  in
  let width = bmp#width  in  
  let out_bmp = new rgb24 width height in
  (* val blit : t -> int -> int -> t -> int -> int -> int -> int -> unit *)
    bmp#blit 0 0 out_bmp 0 0 width height;
    out_bmp
;;
  

let color_replace bmp color_find color_replace =
  let bmp' = bmp_copy bmp in
    color_replace_inplace bmp' color_find color_replace
;;


let generic_main_w_args name args f =
  let files = ref [] in
    Arg.parse args (fun s -> files := s :: !files) "files";
    let files = List.rev !files in
      List.iter (
	fun file ->
	  print_endline file;
	  let outfile1 = "segments/" ^ (get_basename file) ^ "." ^ name ^ ".png" in
	  let rgb = load_rgb_file file in
	  let bmp = f rgb in
	    bmp#save outfile1 (Some Png) [];
      ) files
;;

let generic_main name f =
  generic_main_w_args name [] f
;;
    

let aim_solver bmp =
    let segments = segmenter bmp in
    let segments = segment_filter 20 segments in
    let segments = segment_sort segments in
  let istext c = 
    let (r,g,b) = col c in
      (r < 129 && g  < 129 && b < 129) 
  in
  let qp bmp width height x y = 
    if (is_in x y (0,0,(width-1),(height-1))) then
      istext (bmp#get x y)
    else
      false
  in
    "DOESN'T WORK"
(*
  String.concat "" (
    List.map ( 
      fun (bmp,segment) ->
	let matches = List.map (
	  fun (label,x) -> 
	    let v = quickcompare qp bmp x in 
	      (v,label,x) 
	) phphash in
	let (value,label,matchbmp) = maxlist (fun (v,_,_) -> v) matches in
	  label
    ) segments
  )*)
      
;;
let generic_load chars dir f = 
  let label_bitmaps = List.map (fun (s,(b,f)) -> (s,b)) (load_dir_chars chars  dir) in
    List.map f label_bitmaps
;;

let segsplitter (two,three) suggested_width bmp =
  let width = bmp#width in
  let height = bmp#height in
  if (width >= three) then
    [ (bmp#sub 0 0 suggested_width height) ;
      (bmp#sub ((width / 2) - (suggested_width / 2)) 0 suggested_width height) ;
      (bmp#sub (width - 1 - suggested_width) 0 suggested_width height) ]
  else if (width >= two) then
    [ (bmp#sub 0 0 suggested_width height) ;
      (bmp#sub (width - 1 - suggested_width) 0 suggested_width height) ]
  else
    [ bmp ]
;;

(* centers a bitmap *)
let center rgb width height = 
  let rgb24 = new rgb24 width height in
  let width' = rgb#width in
  let height' = rgb#height in    
  let xoffset = (width - width')/2 in
  let yoffset = (height - height')/2 in
  let fgcol = {r=0;g=0;b=0} in
  let bgcol = {r=255;g=255;b=255} in

  let safeget x y =
    if (x < 0 || x >= width') then 
      bgcol
    else if (y < 0 || y >= height') then
      bgcol
    else
      let p = (rgb#get x y) in
      let p' = p.r + p.g + p.b in
	if (p' < 3 * 32) then
	  fgcol
	else
	  bgcol
  in
  for y = 0 to height - 1 do
    for x  = 0 to width - 1 do
      rgb24#unsafe_set x y (safeget (x - xoffset) (y - yoffset))
    done;
  done;
  rgb24
;;

let identity x = x ;;

(* convert a bmp to a vector, transform it using f *)
let bmp_to_array ~f bmp =
  let width = bmp#width in
  let height = bmp#height in
  let n = width * height in
    Array.init n 
      (fun i ->
	 let x = i mod width in
	 let y = i / width in
	   (f (bmp#get x y))
      )
;;
