open Printf

type decl = 
  | DFun of string * string list


let decl_lst = [DFun("foo",["x";"y"]); DFun("bar", ["a";"b"])]

let ilst = [2;3]
(* 
let rec func1 (ls : int list) (ds : decl list) =
  match ls with
  | [] -> []
  | head::tail -> let fun_names = List.map fst ds in
  [(string_of_int head) ^ (List.hd fun_names)] @ (func1 tail (List.tl ds))
      

let () =
    let res = func1 ilst decl_lst in
    match res with
    | [] -> printf "\n"
    | head::tail -> printf "%s\n" head;;
 *)

let () = 
  let res = List.map (fun a -> (a,true)) ilst in
  match res with
  | [] -> printf "\n"
  | (a,boolean)::tail -> 
    
    printf "(%d,%b)\n" a boolean;;
