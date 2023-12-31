let flatmap  f xs = List.map  f xs |> List.flatten
let flatmapi f xs = List.mapi f xs |> List.flatten

let unlines = String.concat "\n"

let option_mappend a b =
  if Option.is_some a then a else b

let rec split_at n xs = match xs with
  | x::xs when n > 0 ->
     let (pre,post) = split_at (n-1) xs in
     (x::pre,post)
  | _ -> ([],xs)

let rec find_f (pred: 'a -> bool) (ls: 'a list) : 'a option =
  match ls with
  | []    -> None
  | x::xs -> if pred x then Some x else find_f pred xs

let rec find ls x =
  match ls with
    | [] -> None
    | (y,v)::rest ->
      if y = x then Some(v) else find rest x 

(* let find ls x = find_f (fun (y,_) -> y = x) ls |> Option.map snd *)

let rec find_dup (l : 'a list) : 'a option =
  match l with
  | x::xs -> option_mappend (find_f (fun y -> x = y) xs) (find_dup xs)
  | _ -> None