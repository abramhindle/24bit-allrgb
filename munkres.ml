type mask = Primed | Starred | Blank ;;
type covered = Covered | UnCovered

exception StepError of string;;

let debugf f = () ;;
let debug x = ();;
let print_per_iter = ref false ;;

let covered_to_int = function
    Covered -> 1
  | UnCovered -> 0
;;

let covered_to_string = function
    Covered -> "C"
  | UnCovered -> "u"
;;

let covered_printer arr =
  let n = Array.length arr in
    for i = 0 to n - 1 do
      print_string (covered_to_string arr.(i));
      print_string "\t";
    done;
    print_endline "";
;;
  

let mask_to_string = function
    Blank -> "-"
  | Starred -> "*"
  | Primed -> "'"
;;

let mask_printer mask =
  let n = Array.length mask in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        print_string (mask_to_string mask.(i).(j));
        print_string "\t";
      done;
      print_endline "";
    done;
;;

let cost_printer cost = 
  let n = Array.length cost in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        print_float cost.(i).(j);
        print_string "\t";
      done;
      print_endline "";
    done;
;;


let path_debugger path mask =
  let len = Array.length path in
    print_string ("PATH ["^(string_of_int len)^"] ");
    for i = 0 to (len - 1) do
      let r = path.(i).(0) in
      let c = path.(i).(1) in
      let m = mask.(r).(c) in
        print_string (mask_to_string m);
        print_string ",";
    done;
    print_endline "";
    print_string ("PATH ["^(string_of_int len)^"] ");
    for i = 0 to (len - 1) do
      let r = path.(i).(0) in
      let c = path.(i).(1) in
      let m = mask.(r).(c) in
        print_string ((string_of_int r)^":"^(string_of_int c));
        print_string " ";
    done;
    print_endline "";

;;

let fabs x = if (x >= 0.) then x else  (-1.0) *. x ;;

let munkres costmatrix =
  let c i j = costmatrix.(i).(j) in
  let czero i j = 
    let eps = 1.0e-6 in
    let v = c i j in
      (fabs v) <= eps
  in
  let cset i j v = costmatrix.(i).(j) <-                       v in
  let csub i j v = costmatrix.(i).(j) <- costmatrix.(i).(j) -. v in
  let cadd i j v = costmatrix.(i).(j) <- costmatrix.(i).(j) +. v in
  let n = Array.length costmatrix in
    assert((Array.length costmatrix.(0)) = n);
    let mask = Array.create_matrix n n Blank in

    let r_cov = Array.create n UnCovered in
    let c_cov = Array.create n UnCovered in
    let z0_r = ref 0 in
    let z0_c = ref 0 in
    let z1_r = ref 0 in
    let z1_c = ref 0 in

    let mainisdone = ref false in
    let stepnum = ref 1 in

    let path = Array.make_matrix (n*n) 2 0 in


    let stepone stepnum =
      let minval = ref (c 0 0) in
        for i = 0 to (n-1) do
          minval := c i 0;
          for j = 1 to (n-1) do
            let cij = c i j in
              if (!minval > cij) then
                minval:= cij;
          done;
          for j = 0 to (n - 1) do
            csub i j !minval
          done;
        done;
        2
    in

    let steptwo stepnum =
      for i = 0 to (n-1) do
        for j = 0 to (n-1) do
          if (((czero i j)) && c_cov.(j)=UnCovered && r_cov.(i)=UnCovered) then
            begin
              mask.(i).(j) <- Starred;
              c_cov.(j)    <- Covered;
              r_cov.(i)    <- Covered;
            end;
        done;
      done;

      for i = 0 to (n-1) do
        c_cov.(i) <- UnCovered;
        r_cov.(i) <- UnCovered;
      done;

      3
    in

    let stepthree stepnum =
      for i = 0 to (n-1) do
        for j = 0 to (n-1) do
          if mask.(i).(j) = Starred then
            c_cov.(j) <- Covered
        done;
      done;
      let count = ref 0 in
        for j = 0 to (n-1) do
          count := !count + (covered_to_int c_cov.(j));
        done;
        if !count >= n then
          7
        else
          4
    in
      

    let stepfour stepnum =

      (* row,col  : integer;
         done     : boolean; *)

      let step = ref 0 in
      let isdone = ref false in
          

      let find_a_zero () =
        (* row,col : out integer is
           i,j : integer;
           done: boolean; *)
        let row = ref (-1) in
        let col = ref (-1) in
        let i = ref 0 in
        let isdone = ref false in
          
          while(not(!isdone)) do
            let j = ref 0 in
              while(!j < n) do
                if ((czero !i !j) && r_cov.(!i)=UnCovered && c_cov.(!j)=UnCovered) then
                  begin
                    row:= !i;
                    col:= !j;
                    isdone:=true
                  end;
                j:=!j+1;
              done;
              i := !i+1;
              isdone := if (!i >= n) then true else !isdone;
          done;
          (* assert ((!row) != (-1)); *)
          (* assert ((!col) != (-1)); *)
          (!row,!col)
      in

      let star_in_row row =
        let tbool = ref false in
          for j = 0 to (n - 1) do
            if (mask.(row).(j)=Starred) then 
              tbool:=true;
          done;
          !tbool
      in
          
      let find_star_in_row row  =
        let col = ref (-1) in
          for j = 0 to(n-1) do
            if (mask.(row).(j) = Starred) then
              col:= j;
          done;
          (assert (!col != (-1)));
          !col
      in

        (* back to step 4 *)
          while (not(!isdone)) do
            let (row,col) = find_a_zero () in
              if row<0 then
                (
                  isdone:=true;
                  step:=6;
                )
              else
                (
                  mask.(row).(col) <- Primed;
                  if star_in_row(row) then
                    (
                    let col = find_star_in_row row in
                      r_cov.(row) <- Covered; 
                      c_cov.(col) <- UnCovered;
                    )
                  else
                    (
                      isdone:=true;
                      step:=5;
                      z0_r:=row;
                      z0_c:=col;
                    )
                )
                  
          done; 

          !step
      in

        
        
      let stepfive stepnum =
        let count = ref 0 in
        let find_star_in_col c =
          assert(c < n);
          assert(c >= 0);
          debug  ("find_star_in_col " ^ (string_of_int c));
          let r = ref (-1) in
            for i = 0 to (n - 1) do
              if (mask.(i).(c) = Starred) then
                r := i
            done;
            !r
        in
        let find_prime_in_row r =
          assert(r < n);
          assert(r >= 0);
          let c = ref (-1) in
            for j = 0 to (n - 1) do
              if (mask.(r).(j) = Primed) then
                c := j;
            done;
            assert ((!c) != (-1));
            !c
        in

        let convert_path count =
          debugf (fun () -> debug ("COUNT: "^(string_of_int count)));
          for  i = 0 to count do
            let p0 = path.(i).(0) in
            let p1 = path.(i).(1) in
              debugf (fun () -> debug ("CONVERT "^(string_of_int p0)^" "^(string_of_int p1)));
              if (mask.(p0).(p1)) = Starred then
                (mask.(p0).(p1) <- Blank)
              else 
                (mask.(p0).(p1) <- Starred)
          done;
        in

        let clear_covers () =
          for i = 0 to n - 1 do
            r_cov.(i)<-UnCovered;
            c_cov.(i)<-UnCovered;
          done;
        in
        let erase_primes () =
          for i = 0 to n - 1 do
            for j = 0 to n - 1 do
              if (mask.(i).(j) = Primed) then
                mask.(i).(j) <- Blank
            done;
          done;
        in
          
          count := 0;
          debug  "Set Path!";
          debug  ("z0_r " ^(string_of_int !z0_r));
          debug  ("z0_c " ^(string_of_int !z0_c));
          path.(!count).(0) <- !z0_r; 
          path.(!count).(1) <- !z0_c;
          debug  "Path set!";
          let isdone = ref false in
          
          while not(!isdone) do
            debug ("Count: "^(string_of_int !count));
            debug  "find star in col!";
            debug  (string_of_int !count);
            let p = path.(!count).(1) in
              debug  "Have path!";
            let r = (find_star_in_col p) in
            debug  "find star in col done!";
              (
                if (r >= 0) then
                  (
                    debug  "r>=0 path setting!";
                    count:=!count+1;
                    debug  (string_of_int !count);
                    path.(!count).(0) <- r;
                    path.(!count).(1) <- path.(!count-1).(1);
                    debug  "r>=0 path setting done!";
                  )
                else
                  (isdone := true;)
              );
              if (not(!isdone)) then
                (
                  debug  "not isdone find prime!";
                  let c = find_prime_in_row path.(!count).(0) in
                    assert(c!=(-1));
                    debug  "find_prime_in_row_path done!";
                    count := !count + 1;
                    debug  (string_of_int !count);
                    path.(!count).(0)<- path.(!count-1).(0);
                    path.(!count).(1)<- c;

                    debug  "not isdone find prime done!";
                        
                )
          done;
            
            debug "PATH!";
            debugf (fun () -> path_debugger path mask );

          debug  "Convert Path";
          convert_path !count;
          debugf (fun () ->
                    debug "After covert! ";
                    mask_printer mask;
                 );


          debug  "Clear covers";
          clear_covers ();
          erase_primes ();
          3
      in

      let stepsix stepnum =
        let find_smallest () =
          let minval = ref max_float in
            for i = 0 to n - 1 do
              for j = 0 to n - 1 do
                if (r_cov.(i)=UnCovered &&  c_cov.(j)=UnCovered) then
                  let cij = c i j in
                    if !minval > cij then
                      begin
                        minval:= cij;
                        debug "ASSIGNED!";
                      end
              done;
            done;
            debugf (fun () ->
                     print_string "stepsix: ";
                     covered_printer r_cov;
                     print_string "stepsix: ";
                     covered_printer c_cov;
                  );
            assert (!minval != max_float);
            !minval
        in
        let minval = find_smallest () in
          for i = 0 to n - 1 do
            for j = 0 to n - 1 do
              if r_cov.(i)=Covered then
                cadd i j minval;
              if c_cov.(j)=UnCovered then
                csub i j minval;
            done;
          done;
          4
      in
        
        while not(!mainisdone) do
          if (!print_per_iter) then
              begin
                print_endline "############################\n";
                print_endline (string_of_int !stepnum); 
                mask_printer mask; 
                cost_printer costmatrix;
                covered_printer r_cov;
                covered_printer c_cov;
              end;
          let o = 
            match !stepnum with
                1 -> (stepone(!stepnum))
              | 2 -> (steptwo(!stepnum))
              | 3 -> (stepthree(!stepnum))
              | 4 -> (stepfour(!stepnum))
              | 5 -> (stepfive(!stepnum))
              | 6 -> (stepsix(!stepnum))
              | 7 -> (mainisdone:=true; 7)
              | _ -> raise (StepError("What"));
          in
            stepnum := o;
            (*
              if (!print_per_iter) then
              begin
              print_endline ("New state: " ^ (string_of_int !stepnum)); 
              end
            *)

        done;
          if (!print_per_iter) then
              begin
                print_endline "############################\n";
                print_endline (string_of_int !stepnum); 
              end;
          
          mask
;;

exception StarsException;;

let find_stars mask =
  let n = Array.length mask in
  let perm = Array.create n 0 in
  let perm' = Array.create n 0 in

  let rec find_star arr i =
    if (arr.(i) = Starred) then 
      i
    else 
      find_star arr (i + 1)
  in
  let rec find_star_down arr j i  =
    if (arr.(j).(i) = Starred) then 
      i
    else
      find_star_down arr j (i + 1)
  in
    for i = 0 to (n - 1) do
      perm.(i) <- find_star mask.(i) 0;
      perm'.(i) <- find_star_down mask i 0;
    done;
    (perm,perm')
;;

let sum_perm_row_order perm costmatrix =
  let n = Array.length perm in
  let sum = ref 0. in
    for i = 0 to n - 1 do
      sum := !sum +. costmatrix.(i).(perm.(i));
    done;
    !sum
;;
let sum_perm_col_order perm costmatrix =
  let n = Array.length perm in
  let sum = ref 0. in
    for i = 0 to n - 1 do
      sum := !sum +. costmatrix.(perm.(i)).(i);
    done;
    !sum
;;

       

let munkres_permutation n costmatrix =
  let cm = Array.make_matrix n n 0.  in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        cm.(i).(j) <- costmatrix.(i).(j)
      done;
    done;
    let mask = munkres cm in
    let (p1,p2) = find_stars mask in
    let sum1 = sum_perm_col_order p1 costmatrix in 

    let sum2 = sum_perm_col_order p2 costmatrix in 
    let sum3 = sum_perm_row_order p1 costmatrix in 
    let sum4 = sum_perm_row_order p2 costmatrix in 

    (* let sum = sum1 in *)
    let sum = min (min sum2 sum3) (min sum1 sum4) in 
      (* the min, 11/17 *)
      (* col p1 - 4/9 11/17 *)
      (* row p1 - 3/9 9/17 *)
      (* row p2 - 4/9 *)
      (* col p2 - 5/9 11/17 *)
      (sum,p1,p2)
;;

let munkres_test () =
  let run_test matrix =
    print_endline "--------------------------";
    (* cost_printer matrix; *)
    let mask =   munkres matrix in
    let (p1,p2) = find_stars mask in
      mask_printer mask;
      for i = 0 to ((Array.length p1) - 1) do
        print_int p1.(i);
        print_string ",";
      done;
      print_endline "";
      print_endline "--------------------------";
  in
    List.iter run_test [
      [| [| 1.0 ; 2.0 ; 3.0 ; 4.0 |] ;
         [| 4.0 ; 1.0 ; 2.0 ; 3.0 |] ;
         [| 3.0 ; 4.0 ; 1.0 ; 2.0 |] ;
         [| 2.0 ; 3.0 ; 4.0 ; 1.0 |] |];
      
      
      [| [| 1.0 ; 2.0 ; 3.0 |] ;
         [| 2.0 ; 4.0 ; 6.0 |] ;
         [| 3.0 ; 6.0 ; 9.0 |] |];
      
      [| [| 1.0 ; 2.0 ; 3.0 ; 4.0 |] ;
         [| 2.0 ; 4.0 ; 6.0 ; 8.0 |] ;
         [| 3.0 ; 6.0 ; 9.0 ; 12.0 |] ;
         [| 4.0 ; 8.0 ; 12.0 ; 16.0 |] |];

      [| [| 10.0 ; 10.0 ; 1.0 ;  1.0 |] ;
         [| 1.0 ;  10.0 ; 10.0 ; 10.0 |] ;
         [| 10.0 ; 1.0 ;   9.0 ; 10.0 |] ;
         [| 10.0 ; 10.0 ;  9.0 ; 10.0 |] |];
      [| [| 11.0 ; 10.0 ; 10.0 ;  10.0 |] ;
         [| 10.0 ; 11.0 ; 10.0 ; 10.0 |] ;
         [| 10.0 ; 10.0 ; 11.0 ; 10.0 |] ;
         [| 10.0 ; 10.0 ; 10.0 ; 11.0 |] |]
    ];

    let big_random_test n =
      let sn = string_of_int n in
      let fn = float_of_int n in
        print_endline ("TRYING THE "^sn^" x "^sn^" matrix!");
        let m = Array.make_matrix n n 0. in
          for i = 0 to n-1 do
            for j = 0 to n-1 do
              m.(i).(j) <- Random.float fn;
            done;
          done;
          run_test m;
    in
      List.iter big_random_test [2;4;8;16;32;64;128];
        
;;

(* munkres_test ();;  *)
