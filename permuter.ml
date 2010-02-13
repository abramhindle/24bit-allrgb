(*
  ./permuter input-image blocksize startline nlines
  input-image is 4096x4096 already!
  blocksize 128 is good
  e.g.
  ./permuter input-image 128 0 1024
  ./permuter input-image 128 1024 1024
  ./permuter input-image 128 2048 1024
  ./permuter input-image 128 3072 1024
*)
open Munkres;;
open Images;;
open OImages;;

let status str =
  prerr_endline ("[ permuter ] "^str)
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



let dwidth = 4096 ;;
let dheight = dwidth ;;
let drange = dwidth * dheight ;;

let mkcolorarray n =
  Array.create n {r = 0 ; g =0 ; b =0} 
;;

(* arrayout is mutated *)
let img_line img line arrayout =
  for i = 0 to (dwidth  - 1) do
    arrayout.(i) <- img#get i line
  done;
  arrayout
;;

let arr_of_line img line =
  Array.init dwidth
    (fun i -> img#get i line)
;;

let rgb_of_int i =
  {  r = (i land 255); 
     g = ((i land 65535) lsr 8); 
     b = ((i land 16777215) lsr 16)}
;;

let int_of_rgb rgb =
  rgb.r + (rgb.g * 256) + (rgb.b * 65535)
;;

let rgb_of_palette_index p i =
  rgb_of_int (p.(i))
;;


(* arr is mutated *)
let get_colors_for_arr palette blocksize blkn arr =
  for i = 0 to blocksize - 1 do
    arr.(i) <- rgb_of_palette_index palette (i * (drange / blocksize) + blkn)
  done;
  arr
;;

let get_colors_for palette blocksize blkn =
  get_colors_for_arr palette blocksize blkn (mkcolorarray blocksize)
;;

(* j is the block to look at *)
let get_colors_of_line line blocksize j =
  let bj = blocksize * j in (* start location *)
    Array.init blocksize 
      (fun i ->
         line.(bj + i))
;;

(* j is the block to look at *)
let get_colors_of_line_arr line blocksize j arr =
  let bj = blocksize * j in (* start location *)
    for i = 0 to blocksize - 1 do
      arr.(i) <- line.(bj + i)
    done;
    arr
;;

let fisher_yates_shuffle arr =
  let len = Array.length arr in
  let rec helper n =
    if (n > 1) then
      let k = Random.int len in
      let tmp = arr.(k) in
        arr.(k) <- arr.(n - 1);
        arr.(n - 1) <- tmp;
        helper (n  - 1)
    else
      arr
  in
    helper len
;;
      

let random_palette () =
  let palette =   Array.init 16777216
    (fun i -> i)
  in
    fisher_yates_shuffle palette
;;

let rgb_to_yuv rgb =
  let red = rgb.r in
  let blue = rgb.b in
  let green = rgb.g in
  let y = min ((abs (red *  2104 + green * 4130 + blue * 802 + 4096 + 131072)) lsr 13) 235 in
  let u = min ((abs (red * (-1214) + green * (-2384) + blue * 3598 + 4096 + 1048576)) lsr 13) 240 in
  let v = min ((abs (red * 3598 + green * (-3013) + blue * (-585) + 4096 + 1048576)) lsr 13) 240 in
    (y,u,v)
;;

let color_distance c1 c2 =
  let sqr x = x * x in
  let (y1,u1,v1) = rgb_to_yuv c1 in
  let (y2,u2,v2) = rgb_to_yuv c2 in
    (sqr (y1 - y2)) + (sqr (u1 - u2)) + (sqr (v1 - v2))
    (* (sqr (c1.r - c2.r)) + (sqr (c1.g - c2.g)) + (sqr (c1.b - c2.b)) *)
;;


let calculate_color_distance  blocksize input_colors palette_colors matrix =
  for y = 0 to blocksize - 1 do
    for x = 0 to blocksize - 1 do
      let d = color_distance input_colors.(x) palette_colors.(y) in
        matrix.(x).(y) <- float_of_int d
    done;
  done;
  matrix
;;      


let map_palette_to_allrgb img =
  status ("[map_palette_to_allrgb] making the tuples");

  let pixel_tuples = Array.init drange 
    (fun i ->
       let y = i / dwidth in
       let x = i mod dwidth in
         (int_of_rgb (img#get x y), x, y))
  in
  let pixel_tuples = fisher_yates_shuffle pixel_tuples in
    status ("[map_palette_to_allrgb] sorting the tuples");
    Array.sort (fun (i,_,_) (j,_,_) -> compare i j) pixel_tuples;
    status ("[map_palette_to_allrgb] mapping the tuples");
    let palette = Array.create drange 0 in
      for i = 0 to drange - 1 do
        let (_,x,y) = pixel_tuples.(i) in
          (* i is color x y is location *)
          palette.(y * dwidth + x) <- i
      done;
      status ("[map_palette_to_allrgb] done");
      palette
;;


let output_palette palette_file_name palette =
  let oimg = new rgb24 dwidth dheight in
    for y = 0 to dheight - 1 do
      let yoff = dwidth * y in
      for x = 0 to dwidth - 1 do
        let i = yoff + x in
          oimg#unsafe_set x y (rgb_of_int (palette.(i)))
      done;
    done;
    oimg#save palette_file_name (Some Png) [];
    ()
;;


let permuter input_image blocksize start len =
  assert(blocksize <= dwidth);
  assert(dwidth mod blocksize = 0);
  assert(len > 0 && len <= dwidth);
  status ("Loading the image "^ input_image);
  let img = load_rgb_file input_image in
    assert(img#width = dwidth);
    assert(img#height = dwidth);
    let n = dwidth / blocksize in
    let output_file_name = input_image ^ "." ^ (string_of_int blocksize) ^"."^ (string_of_int start) ^ "." ^ (string_of_int len) ^ ".png" in
    let palette_file_name = "palette-of."^input_image ^ "." ^ (string_of_int blocksize) ^"."^ (string_of_int start) ^ "." ^ (string_of_int len) ^ ".png" in

      status ("Output file: "^output_file_name);

    let oimg = new rgb24 dwidth len in
    let colors = Array.create dwidth {r = 0 ; g =0 ; b =0} in
    let bcolors =  Array.create blocksize { r = 0; g = 0; b = 0 } in
    let line_colors = Array.create blocksize { r = 0; g = 0; b = 0 } in
      (* let palette = random_palette () in *)
      status ("Generating the palette");
    let palette = map_palette_to_allrgb img in
      output_palette palette_file_name palette;
    let distance_matrix = Array.make_matrix blocksize blocksize 0.0 in
      for i = start to (start + len - 1) do
        status ("Line "^(string_of_int i));
        status ("Get that line "^(string_of_int i));
        let line = img_line img i colors in
          for j = 0 to (n - 1) do
            (* status ("Block "^(string_of_int j)); *)
            let palette_colors = get_colors_for_arr palette blocksize ((i*n) + j) bcolors in
            let line_colors = get_colors_of_line_arr line blocksize j line_colors in
            let distance_matrix = calculate_color_distance blocksize line_colors palette_colors distance_matrix in
            let mask =   munkres distance_matrix in
            let (p1,p2) = find_stars mask in
              for k = 0 to blocksize - 1 do
              (* ok now we we take the mappings from p1 and find which colors best match *)
              (* I hope this isn't backwards *)
                oimg#unsafe_set ((blocksize * j) + k) (i - start) (palette_colors.(p2.(k)))
              done;
          done;
      done;
        oimg#save output_file_name (Some Png) [];

;;

let dispatch_from_cmdline () =
  Random.init 666;
  let files = ref [] in
    Arg.parse [] (fun s -> files := s :: !files) "edge files";
    let files = List.rev !files in
      match files with
          [] -> prerr_endline "./permuter <input_image> <blocksize> <startline> <nlines>"
        | (filename::[]) -> permuter filename  128 0 dwidth
        | (filename::blocksize::[]) -> permuter filename (int_of_string blocksize) 0 dwidth
        | (filename::blocksize::start::[]) ->
            let blocksize = (int_of_string blocksize) in
            let start = (int_of_string start) in
            let len   = dwidth - start in
              permuter (List.hd files) blocksize start len
        | (filename::blocksize::start::len::[]) ->
            let blocksize = (int_of_string blocksize) in
            let start = (int_of_string start) in
            let len   = (int_of_string len) in
              permuter (List.hd files) blocksize start len
        
;;
  

dispatch_from_cmdline ();;         
      
