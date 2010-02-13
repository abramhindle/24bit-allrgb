(*
# Hilbert logic from https://www.cs.dal.ca/sites/default/files/CS-2006-07.pdf
# Algorithm a direct implementation of the logic presented there.
# returns the index into the rgb cube indexed by the 3d hilbert curve
ported from
http://gist.github.com/300780
by eric burnett
*)

 
 
(* trailing set bits *)
let rec tsb i =
  if ((i land 7) = 7) then
    (* # All 4 set *)
    4 + (tsb (i lsr 4))
  else
    match (i land 7) with
        0 -> 0
      | 1 -> 1
      | 2 -> 0
      | 3 -> 2
      | 4 -> 0
      | 5 -> 1
      | 6 -> 0
      | 7 -> 3
(*    tsb4 = (0, 1, 0, 2, 0, 1, 0, 3)
      return tsb4[i & 7]*)
;;
 
(* the greycode of the integer i *)
let gc i = i lxor (i lsr 1)


let gcinverse m g = 
  let i = g in
  let j = 1 in
  (* while j < m:
        i = i ^ (g >> j)
        j = j + 1
    return i *)
  let rec helper i j =
    if (j < m) then
      helper (i lxor (g lsr j)) (j + 1)
    else
      i
  in
    helper i j
;;
 

let dee i n  =
  if (i == 0) then 0
  else if ((i land 1) != 0) then ((tsb i) mod n)
  else ((tsb i-1) mod n)
;;

let eee i =
  if (i = 0) then 0
  else (gc  ( (i-1) land (lnot 1) ))
;;    
  

 
let twopow = function
    0 -> 1
  | 1 -> 2
  | 2 -> 4
  | 3 -> 8
  | 4 -> 16
  | 5 -> 32
  | 6 -> 64
  | 7 -> 128
  | 8 -> 256
  | 9 -> 512
  | 10 -> 1024
  | 11 -> 2048
  | 12 -> 4096
  | 13 -> 8192
  | 14 -> 16384
  | 15 -> 32768
  | 16 -> 65536
  | n -> 1 lsl n
;;

let cycle a b n = ( a lsr b ) lor  ((a lsl ( n - b )) land ((twopow n) - 1))

let cycle_left a b n =  cycle a (n - b) n ;;

let tee e d n b = cycle (b lxor e) (d + 1) n ;;

let teeinv e d n b = tee (cycle e (d+1) n) (n - d - 1) n b;;


 
let bitSet n i v = n lor (v lsl i) ;;

 
let bit n i =  (n lsr i) land 1 ;;


let hindex m p =
  let (p0,p1,p2) = p in
  let dim = 3 in
  let rec helper i h e d =
    if (i >= 0) then
      let l = ((bit p2 i) lsl 2) lor ( (bit p1  i) lsl 1) lor  ( bit p0  i) in
      let l = cycle_left l  1  dim in
      let l = tee e  d dim l in
      let w = gcinverse dim l in
      let e = e lxor ( cycle_left (eee w) (d+1) dim ) in
      let d = (d + (dee w dim) + 1) mod dim in
      let h = (h lsl dim) lor w in
        helper (i - 1) h e d
    else
      h
  in
    helper (m - 1) 0 0 0
;;

let hindex_inverse m h =
  let dim = 3 in
  let e = 0 in
  let    d = 0 in
  let p = [| 0 ; 0 ; 0 |] in
  let rec helper i e d =
    if ( i >= 0) then
      let w = ((bit h (3*i+2)) lsl 2) lor ((bit h (3*i+1)) lsl 1) lor ( bit h  (3*i)) in
      let l = gc w in
      let l = teeinv  e d dim l in
        for j = 0 to dim - 1 do
          p.(j) <- p.(j) lor ((bit l j) lsl i)
        done;
        let e = e lxor (cycle_left (eee w)  (d+1) dim) in
        let d = (d + (dee w dim) + 1) mod dim in
          helper (i - 1) e d
    else
      p
  in
    helper (m - 1) 0 0
;;
