open Lib.Expr
open Lib.Instruction
open Lib.Misc
open Printf

type 'a envt = (string * 'a) list

let find_decl (ds : decl list) (name : string) : decl option =
  find_f (fun (DFun(fname,_,_)) -> fname = name) ds


let count = ref 0
let gen_temp base =
  count := !count + 1;
  sprintf "temp_%s_%d" base !count

let const_true = HexConst(0xffffffff)
let const_false = HexConst(0x7fffffff)

(* We want to be using C functions to deal with error output here. *)
let throw_err code = 
  [
    IPush(Sized(DWORD_PTR, Const(code)));
    ICall("error");
  ]

let max n m = if n > m then n else m

let error_overflow = "error_overflow"
let error_non_int  = "error_non_int"
let error_non_bool = "error_non_bool"
let error_non_tuple = "error_non_tuple"
let error_too_small = "error_too_small"
let error_too_large = "error_too_large"
let error_non_closure = "error_non_closure"
let error_arity_mismatch = "error_arity_mismatch"


let stackloc i      = RegOffset (-4 * i, ESP)  
let save_to_stack i = [ IMov (stackloc i, Reg(EAX)) ]
let restore_stack i = [ IMov (Reg(EAX), stackloc i) ]

let check_overflow = IJo(error_overflow)

let check_num =  [IAnd(Reg(EAX), Const(0x00000001)) ;ICmp(Reg(EAX), Const(0x00000000)) ;IJne(error_non_int)]

let check_bool = [IAnd(Reg(EAX), Const(0x00000003)) ;ICmp(Reg(EAX), Const(0x00000003)) ;IJne(error_non_bool)]

let check_tuple = [IAnd(Reg(EAX), Const(0x00000003)) ;ICmp(Reg(EAX), Const(0x00000001)) ;IJne(error_non_tuple)]

let rec env_mem (s: string) (env : (string * int) list) =
  match env with
  | [] -> false
  | (var_name, var_val)::tail -> if (var_name = s) then true else (env_mem s tail)

let c_call si f =
  [ ISub (Reg ESP, Const (4 * si))
  ; IPush (Sized (DWORD_PTR, Reg EAX))
  ; ICall f
  ; IAdd (Reg ESP, Const (4 * (si +1)))
  ]      

(* let freevars (e: expr) : string list = 
  match expr with
  | 
  failwith "TBD: freevars" *)

let rec compile_prim1 (o : prim1) (e : expr) (istail : bool) (si : int) (ds : decl list) (env : int envt) : instruction list =
  let prelude = compile_expr e false si ds env in
  let op_is   = match o with
    | Add1   -> [IMov((RegOffset(-4*si, ESP)), Reg(EAX))] 
                 @ check_num @ 
                 [IMov(Reg(EAX), (RegOffset(-4*si, ESP))) 
                 ;IAdd(Reg(EAX), Const(1*2))
                 ]
    | Sub1   -> [IMov((RegOffset(-4*si, ESP)), Reg(EAX))] 
                @ check_num @ 
                [IMov(Reg(EAX), (RegOffset(-4*si, ESP))) 
                ;ISub(Reg(EAX), Const(1*2))
                ]
    | IsNum  -> 
        let isnum_false_br = (gen_temp "isnum_false_br") in let end_of_isnum = (gen_temp "end_of_isnum") in
              [ IAnd(Reg(EAX), Const(1))
              ; ICmp(Reg(EAX), Const(0))
              ; IJne(isnum_false_br)
              ; IMov(Reg(EAX), const_true)
              ; IJmp(end_of_isnum)
              ; ILabel(isnum_false_br)
              ; IMov(Reg(EAX), const_false) 
              ; ILabel(end_of_isnum)]            

    | IsBool -> 
        let isbool_false_br = (gen_temp "isbool_false_br") in let end_of_isbool = (gen_temp "end_of_isbool") in
              [ IAnd(Reg(EAX), Const(3))
              ; ICmp(Reg(EAX), Const(3))
              ; IJne(isbool_false_br)
              ; IMov(Reg(EAX), const_true)
              ; IJmp(end_of_isbool)
              ; ILabel(isbool_false_br)
              ; IMov(Reg(EAX), const_false)
              ; ILabel(end_of_isbool)]

    | IsTuple -> 
      let istuple_false_br = (gen_temp "istuple_false_br") in let end_of_istuple = (gen_temp "end_of_istuple") in
            [ IAnd(Reg(EAX), Const(3))
            ; ICmp(Reg(EAX), Const(1))
            ; IJne(istuple_false_br)
            ; IMov(Reg(EAX), const_true)
            ; IJmp(end_of_istuple)
            ; ILabel(istuple_false_br)
            ; IMov(Reg(EAX), const_false)
            ; ILabel(end_of_istuple)]
        
    | Print  -> (* FFI to C function in the runtime *)
                c_call si "print" 

    | Input   -> check_num @ (c_call si "input")         

  in prelude @ op_is

and compile_prim2 (o : prim2) (e1 : expr) (e2 : expr) (istail : bool) (si : int) (ds : decl list) (env : int envt) : instruction list =
match o with
| Equal -> compile_equals e1 e2 istail si ds env
| _     -> compile_prim2_helper o e1 e2 istail si ds env

and compile_equals (e1 : expr) (e2 : expr) (istail : bool) (si : int) (ds : decl list) (env : int envt) : instruction list =
  let rhs_loc = stackloc (si + 1) in
  let e1' = compile_expr e1 false si ds env       @ save_to_stack si        in
  let e2' = compile_expr e2 false (si + 1) ds env @ save_to_stack (si + 1)  in
  
      let equal_true_br = (gen_temp "equal_true_br") in 
      let end_of_equal = (gen_temp "end_of_equal") in
      let op' =
                [
                ICmp(Reg(EAX), stackloc (si + 1))
                ; IJe(equal_true_br)
                ; IMov(Reg(EAX), const_false)
                ; IJmp(end_of_equal)
                ; ILabel(equal_true_br)
                ; IMov(Reg(EAX), const_true)
                ; ILabel(end_of_equal)
                ]
      in    
        e1'
      @ e2'
      @ restore_stack si
      @ op'
                


and compile_prim2_helper (o : prim2) (e1 : expr) (e2 : expr) (istail : bool) (si : int) (ds : decl list) (env : int envt) : instruction list =
  let rhs_loc = stackloc (si + 1) in
  let e1' = compile_expr e1 false si ds env       @ save_to_stack si       @ check_num in
  let e2' = compile_expr e2 false (si + 1) ds env @ save_to_stack (si + 1) @ check_num in
  let op'   = match o with
    | Plus    -> [ IAdd (Reg(EAX), rhs_loc); check_overflow ]
    | Minus   -> [ ISub (Reg(EAX), rhs_loc); check_overflow ]
    | Times   -> [ ISar (Reg(EAX), Const(1))
                 ; IMul (Reg(EAX), rhs_loc)
                 ; check_overflow
                 ]
    | Less -> 
      let less_true_br = (gen_temp "less_true_br") in 
      let end_of_less = (gen_temp "end_of_less") in
                        [
                        ICmp(Reg(EAX), stackloc (si + 1))
                        ; IJl(less_true_br)
                        ; IMov(Reg(EAX), const_false)
                        ; IJmp(end_of_less)
                        ; ILabel(less_true_br)
                        ; IMov(Reg(EAX), const_true)
                        ; ILabel(end_of_less)
                        ]

    | Greater -> 
      let greater_true_br = (gen_temp "greater_true_br") in 
      let end_of_greater = (gen_temp "end_of_greater") in
                          [
                          ICmp(Reg(EAX), stackloc (si + 1))
                          ; IJg(greater_true_br)
                          ; IMov(Reg(EAX), const_false)
                          ; IJmp(end_of_greater)
                          ; ILabel(greater_true_br)
                          ; IMov(Reg(EAX), const_true)
                          ; ILabel(end_of_greater)
                          ]

    | Equal -> []

    in    e1'
        @ e2'
        @ restore_stack si
        @ op'

    
and compile_let (bs : (string * expr) list) (body : expr) (istail : bool) (si : int) (ds : decl list) (env : int envt) : instruction list =
  match bs with
  | []         -> compile_expr body istail si ds env
  | (x,e)::bs' ->
    let e_is   = compile_expr e false si ds env in
      e_is
    @ save_to_stack si
    @ compile_let bs' body istail (si+1) ds ((x,si)::env) 

and compile_if (cnd : expr) (thn : expr) (els : expr) (istail : bool) (si : int) (ds : decl list) (env : int envt) : instruction list = 
  let cnd' = (compile_expr cnd false si ds env) in 
  let cnd'' = cnd' @ save_to_stack si @ check_bool @ restore_stack si in
  let thn' = (compile_expr thn istail si ds env) in 
  let els' = (compile_expr els istail si ds env) in
  let thn_br = (gen_temp "thn_br") in
  let if_end_br = (gen_temp "if_end_br") in
    cnd'' 
  @ [ICmp(Reg(EAX) ,const_true)
  ; IJe(thn_br)] 
  @ els' 
  @ [IJmp(if_end_br); ILabel(thn_br)]
  @ thn' 
  @ [ILabel(if_end_br)]


and rearrage_args_on_stack from_idx save_loc to_idx=
  if (from_idx < save_loc) then
    (restore_stack from_idx) @ (save_to_stack to_idx) @ (rearrage_args_on_stack (from_idx + 1) save_loc (to_idx + 1))
  else
    []


and save_args_on_stack args istail save_loc si ds num_args env =
  match args with
  | [] -> 
    if istail
    then
      let bottom = (save_loc - num_args) in
      (rearrage_args_on_stack bottom save_loc 2)
    else []

  | exp::exps -> 
    let compiled_exp = (compile_expr exp false si ds env) in
    compiled_exp 
    @ [IMov(RegOffset((-4*save_loc), ESP), Reg(EAX))]
    @ (save_args_on_stack exps istail (save_loc + 1) (si+1) ds num_args env)

(* and compile_lambda (args : string list) (body : expr)  (si : int) (ds : decl list) (env : int envt) : instruction list = 
  failwith "TBD: compile_lambda" *)


and compile_app (f : string) (args : expr list) (istail : bool) (si : int) (ds : decl list) (env : int envt) : instruction list = 

  let compile_app_normal () =
        let after_call = gen_temp "after_call" in
        let sub_esp_by = ((List.length args) + 1) in
        let call_prelude =  [  
                              IMov(stackloc si, Sized(DWORD_PTR ,Label(after_call)))
                            ; IMov(stackloc (si + 1), Reg(ESP))
                            ] 
                            @ (save_args_on_stack args istail (si + 2) (si + 2) ds (List.length args) env)
                            @ 
                            [
                              ISub(Reg(ESP), Const(4*si))
                            ; IJmp(f)
                            ]
                            in
        let post_call = [ 
                        ILabel(after_call)
                        ; IMov(Reg(ESP), stackloc 2)
                        ] in
                        call_prelude @ post_call

                        in


  let compile_app_tailcall () = 
    (save_args_on_stack args istail si (si + 2) ds (List.length args) env)
    @
    [IJmp(f)]

  in

  if istail
    then compile_app_tailcall ()
    else compile_app_normal ()
  
  


and compile_tuple (args : expr list) (istail : bool) (num_args : int)  (hi : int)  (si : int) (ds : decl list) (env : int envt) : instruction list = 
      let ops = 
        begin match args with
        | [] -> []
          
        | fst_arg::rest -> 
          let fst_arg_is = (compile_expr fst_arg false si ds env) @ (save_to_stack si) in 
          fst_arg_is
          @ (compile_tuple rest istail num_args (hi + 1) (si + 1) ds env)
          @
          (restore_stack si)
          @
          [
            IMov(RegOffset(hi*4, EBX), Reg(EAX))
          ]
          
        end
      in
      ops


and compile_get_item (tuple : expr) (idx : expr) (istail : bool) (si : int) (ds : decl list) (env : int envt) : instruction list = 
    let tuple_is = (compile_expr tuple false si ds env) @ (save_to_stack si) @ check_tuple in
    let idx_is = (compile_expr idx false (si+1) ds env) @ (save_to_stack (si+1)) @ check_num in

      (* ------------------  Note : check whether the index is in range or not ---------------------- *)

      tuple_is
      @
      idx_is
      @
      (restore_stack si) (* tuple addr in eax *) 
      @ 
      [
        IMov(Reg(ECX), (stackloc (si+1)))  (* Mov the idx in ecx *)
      ; IShr(Reg(ECX), Const(1))
      ; IMov(Reg(EAX), RegOffsetReg(EAX, ECX, 4, -1))
      ]


and compile_expr (e : expr) (istail : bool) (si : int) (ds : decl list) (env : (string * int) list) : instruction list =
  match e with
    | ENumber(n) -> [IMov(Reg(EAX), Const(2*n))]

    | EBool(b) -> [ IMov (Reg(EAX), if b then const_true else const_false) ]
      
    | EPrim1(op, arge) -> (compile_prim1 op arge istail si ds env)
      
    | EPrim2(op, el, er) -> (compile_prim2 op el er istail si ds env)
          
    | EId(x) -> 
      let arg = begin match find env x with
                | Some(i) -> RegOffset(-4 * i, ESP)
                | None    -> failwith (sprintf "Unbound identifier: %s" x)
               end
      in [ IMov (Reg(EAX), arg) ]

    | ELet(bs, body) -> compile_let bs body istail si ds env

    | EIf(cnd, thn, els) -> compile_if cnd thn els istail si ds env

    | EApp(f, args) -> compile_app f args istail si ds env

    | ETuple(args) -> 
      let tuple_length = (List.length args) in 
        let heap_index = 0 in
          (compile_tuple args istail tuple_length (heap_index + 1) si ds env)
          @
          [ IMov(RegOffset(0, EBX), Sized(DWORD_PTR, Const(tuple_length*2))) ]
          @
          [
            IMov(Reg(EAX), Reg(EBX))
          ; IOr(Reg(EAX), Const(1))
          ; IAdd(Reg(EBX), Const(4*(tuple_length + 1)))
          ]
          

    | EGetItem(tuple,index) -> (compile_get_item tuple index istail si ds env)

    | ESeq ([e])               -> compile_expr     e  istail si ds env
    | ESeq (e::es)             -> compile_expr     e  istail si ds env @ compile_expr (ESeq es) istail si ds env
    | ESeq []                  -> []

      

let rec create_arg_idx args si = 
  match args with
  | [] -> []
  | head::tail -> (head, si)::(create_arg_idx tail (si + 1))


(* The stack_index to be used for compilng body exprs will be (2 + num_args)  bcoz the stack slots
   storing the args are not to be meddled with and modified during compilation , and 2 bcoz 2 slots are used
   for storing the return address and old ESP resectively *)
let compile_decl (ds : decl list) (d : decl) : instruction list = 
  match d with
  | DFun(name, args, body) ->
    let num_args = (List.length args) in
    let new_env = (create_arg_idx args 2) in
    let stack_index_for_body = (2 + num_args) in
    let body_is = compile_expr body true stack_index_for_body ds new_env in 
    [ILabel(name)] @ body_is @ [IRet]
    


let rec find_one (l : 'a list) (elt : 'a) : bool =
  match l with
    | []    -> false
    | x::xs -> (elt = x) || (find_one xs elt)

let rec find_dup (l : 'a list) : 'a option =
  match l with
    | []    -> None
    | [x]   -> None
    | x::xs ->
      if find_one xs x then Some(x) else find_dup xs


let rec find_arity our_func decl_list = 
  match decl_list with
  | [] -> None
  | DFun(name, args, body)::tail -> 
    if (name = our_func) then
      Some(List.length args)
    else
      (find_arity our_func tail)


let rec well_formed_e (e : expr) (ds : decl list) (env : bool envt) =
  
  (* let go xs = flatmap (fun x -> well_formed_e x ds env) xs in *)
  
  match e with
    | ENumber(_)
    | EBool(_) -> []
    | EId(x)   ->
      begin match find env x with
        | None    -> ["Unbound identifier: " ^ x]
        | Some(_) -> []

      end
    | EGetItem(l, r) -> 
      (well_formed_e l ds env) @ (well_formed_e r ds env)

    | ETuple(elts)   -> 
      let rec tupl_check elts = 
        match elts with
        | [] -> []
        | cur_e::es -> (well_formed_e cur_e ds env) @ (tupl_check es) in
      (tupl_check elts)

    | EPrim1(op, e)           ->
        well_formed_e e ds env
    | EPrim2(op, left, right) ->
      (well_formed_e left ds env) @ 
      (well_formed_e right ds env)

    | EIf(cond, thn, els)     ->
      (well_formed_e cond ds env) @
      (well_formed_e thn ds env ) @
      (well_formed_e els ds env )

    | EApp(name, args)        -> 
      let fun_names = List.map (fun (DFun(name, _, _)) -> name) ds in

      if (List.mem name fun_names) then 
        let expected_arity = 
          begin match (find_arity name ds) with
          | None -> failwith (sprintf "No such function: %s" name)
          | Some(v) -> v 
        end in
        let observed_arity = (List.length args) in
          if (expected_arity = observed_arity) then
            (match args with
            | [] -> []
            | head::tail -> well_formed_e head ds env)
          else
            ["Arity Mismatch"]
      else
        (match find env name with
        | None    -> ["No such function " ^ name]
        | Some(_) -> [])
    
    (* | ELambda(args, body) ->
      let dup_args =
        begin match find_dup args with
        | None -> []
        | Some(dup) -> ["Multiple arguments named " ^ dup]
        end
      in
      let body_env = List.map (fun a -> (a, true)) args in
      let from_body = well_formed_e body ds (body_env @ env) in
      dup_args @ from_body *)


    | ELet(binds, body)       ->
      let names          = List.map fst binds                           in
      let env_from_binds = List.map (fun a -> (a, true)) names          in
      let from_body      = well_formed_e body ds (env_from_binds @ env) in
      begin match find_dup names with
        | None       -> from_body
        | Some(name) -> ("Duplicate name in let: " ^ name)::from_body
      end

    | ESeq(e::es) -> (well_formed_e e ds env) @ (well_formed_e (ESeq es) ds env)

    

let well_formed_d (d : decl) (ds : decl list) : string list =
  match d with
  | DFun(name, args, body) -> 
    let new_env = (List.map (fun a -> (a,true)) args) in
    let from_body      = well_formed_e body ds new_env in
    begin match find_dup args with
        | None    -> from_body
        | Some(v) -> ("Duplicate args: " ^ v)::from_body
      end

let well_formed_p (p : program) : string list =
  match p with
    | Program(ds, maine) ->
      let names        = List.map (fun (DFun(name, _, _)) -> name) ds in
      let subexpr_errs = 
        (well_formed_e maine ds []) @
        (List.flatten (List.map (fun d -> well_formed_d d ds) ds))    in
      begin match find_dup names with
        | None    -> subexpr_errs
        | Some(v) -> ("Duplicate function definition: " ^ v)::subexpr_errs
      end


let flatmap  f xs = List.map  f xs |> List.flatten

let compile_to_string prog =
  match well_formed_p prog with
    | x::rest ->
      let errstr = String.concat "\n" (x::rest) in
      failwith (errstr ^ "\n")
    | [] ->
      match prog with
        | Program(decls, main) ->
          let compiled_decls = flatmap (compile_decl decls) decls in
          let compiled_main = (compile_expr main false 1 decls []) in
          let prelude = "
section .text
extern error
extern equal
extern print
extern input
global our_code_starts_here" in
          let main_start = [
            ILabel("our_code_starts_here");
            IMov(Reg(EBX), RegOffset(4, ESP));
            IAdd(Reg(EBX), Const(4));
            IAnd(Reg(EBX), HexConst(0xFFFFFFFC));
          ] in
          let postlude = [
            IRet
          ]
          @ [ILabel(error_non_int)]    @ (throw_err 1)
          @ [ILabel(error_non_bool)]   @ (throw_err 2)
          @ [ILabel(error_overflow)]   @ (throw_err 3)
          @ [ILabel(error_non_tuple)]  @ (throw_err 4)
          @ [ILabel(error_too_small)]  @ (throw_err 5)
          @ [ILabel(error_too_large)]  @ (throw_err 6) 
          @ [ ILabel error_non_closure ] @ (throw_err 7)
          @ [ ILabel error_arity_mismatch] @ (throw_err 8) in

          let as_assembly_string = (to_asm (
            compiled_decls @
            main_start @
            compiled_main @
            postlude)) in
          sprintf "%s%s\n" prelude as_assembly_string




(* ============================================== TEST EXAMPLES ============================================  *)


let example1 = EPrim1(Add1, EBool(true))

let example2 = ELet([("x", ENumber(10)); ("y", ENumber(9))],  
                  EPrim2(Times, EPrim2(Minus, EId("x"), EId("y")), ENumber(2)))
  
(* x=10,y=9 in 
            ( * (- x y) 2)
            (10 - 9) * 2 == 2 *)

let example3 = ELet([("x", ENumber(5))], EPrim1(Add1, EId("x")))

let example4 = EPrim1(IsNum, EBool(true))

let example5 = EPrim2(Plus, ENumber(5), EBool(true))

let example6 = EPrim2(Less, ENumber(5), ENumber(10))

let t1 = EPrim2(Plus, ENumber(5), ENumber(10))
let t2 = EPrim2(Times, ENumber(5), ENumber(1000))

let example7 = EIf(EPrim2(Less, ENumber(5), ENumber(10)), t1, t2)


let prg1 = Program([], t2)

let prg2 = Program([DFun("func", ["x"; "y"], EPrim2(Times, EId("x"), EId("y")))], 
              ELet([("x", ENumber(8)); ("y", ENumber(4))], EApp("func", [EId("x"); EId("y")])))


(* Failing on this example *)
let prg3 = Program([DFun("func", ["x"; "y"], EPrim2(Times, EId("x"), EId("y")))], 
            ELet([("x", ENumber(19)); ("y", ENumber(21))], EApp("func", [EPrim1(Add1, EId("x")) ; EPrim1(Sub1, EId("y"))])))
(* ans = 400 *)

let prg4 = Program([DFun("func", ["x"; "y"], EPrim2(Times, EId("x"), EId("y")))], 
              EApp("func", [EPrim1(Add1, ENumber(9)) ; EPrim1(Sub1, ENumber(21))]))
(* ans = 200 *)

let prg5 = Program([DFun("func", ["x"; "y"], EPrim1(Print ,EPrim2(Times, EId("x"), EId("y"))))], 
              EApp("func", [EPrim1(Add1, ENumber(9)) ; EPrim1(Sub1, ENumber(21))]))
(* ans = 200 *)

let z1 = EPrim2(Plus, EId("x"), EId("y"))

let z2 = EPrim2(Times, EId("x"), EId("y"))


let prg6 = Program([DFun("func", ["x"; "y"],
  EIf(EPrim2(Less, EId("x"), EId("y")), z1, z2))],
  EApp("func", [EPrim1(Add1, ENumber(20)) ; EPrim1(Sub1, ENumber(30))]))

(* Ans = 50 *)


let prg7 = Program([],example7)

let fact_base_case = ENumber(1)
let fact_inductive_case = EPrim2(Times, EId("x"), EApp("fact", [EPrim1(Sub1, EId("x"))]))
let factorial_example = 
  Program([DFun("fact", ["x"],
  EIf(EPrim2(Less, EId("x"), ENumber(2)), fact_base_case , fact_inductive_case))],
  EApp("fact", [ENumber(3)]))

  (*
    def fact(x):
      if x < 2:
        1
      else
        x * fact(x-1)

    main:
        fact(5)
  *)


(* let fibonacci_example = failwith "todo" *)


let tuple_eg1 = Program([], ETuple([ENumber(1); ENumber(2)]))

let tuple_eg2 = Program([], 
EGetItem(ETuple([ENumber(100); ENumber(22); ENumber(43)]), EPrim1(Add1, ENumber(2))))

(* (100,22,43)[3] ==> 43 *)

let tuple_eg3 = Program([], 
  ELet([("x", ETuple([ENumber(1); ENumber(2); ENumber(3)]))], EGetItem(EId("x"), EPrim1(Add1, ENumber(1)))))

let tuple_eg4 = Program([], EPrim2(Equal, ETuple([ENumber(1); ENumber(2)]), ENumber(2)))

let tuple_eg5 = Program([], EPrim2(Equal, ETuple([ENumber(1); ENumber(2)]), ETuple([ENumber(1); ENumber(2)])))


let tuple_eg6 = Program([], 
  ELet([("x", ETuple([ENumber(1); ENumber(2); ENumber(3)]))], EPrim2(Equal, EId("x"), EId("x"))))


let tuple_eg7 = 
Program([], 
    EGetItem(
      EGetItem(
      ETuple([ENumber(71); ENumber(24); (EPrim1(Add1, ENumber(32))); ETuple([ENumber(99); ENumber(121)])])
      , ENumber(4))
      , ENumber(1))
      )

let tuple_eg8 = 
  Program([], 
      EGetItem(
        EGetItem(
        ETuple(
          [ETuple([ENumber(27); ENumber(33)])
          ; ETuple([ENumber(99); ENumber(121)])])
        , ENumber(1))
        , ENumber(3))
        )


let tuple_eg9 = Program([], 
EGetItem(
EGetItem(
  ETuple(
    [
      ENumber(11)
    ; ETuple([ENumber(9); EBool(false)])
    ])
    , ENumber(2))
    , ENumber(2))
)

let tuple_eg10 = Program([],
  ETuple(
    [
      ENumber(11)
    ; ETuple([ENumber(9); EBool(false)])
    ])
)

let tuple_eg11 = Program([], 
EGetItem(
  ETuple(
    [
      ENumber(11)
    ; ETuple([ENumber(9); EBool(false)])
    ])
    , ENumber(2))
) 


let tco_eg1 = Program(
  [                                 (* tail call*)
  DFun("f", ["x"], EApp("g", [EPrim2(Plus, EId("x"), ENumber(1)); EPrim2(Plus, EId("x"), ENumber(2)); EPrim2(Plus, EId("x"), ENumber(3))]))
  ;
  DFun("g", ["w"; "y"; "z"], EPrim2(Times, EPrim2(Times, EId("w"), EId("y")), EId("z")))
  ], 
  EApp("f", [ENumber(4)]))

(* Answer =  210 *)



let tco_eg2 = Program(
  [
  DFun("f", ["n"], EPrim2(Times, EId("n"), ENumber(2)))
  ;
  DFun("g", ["n"], EApp("f", [EPrim1(Add1, EId("n"))])) (* tail call *)
  ;
  DFun("h", ["n"], EPrim2(Times, EApp("f", [EPrim1(Add1, EId("n"))]), ENumber(3))) (* not a tail call *)
  ], 
  EPrim2(Times, EApp("g", [ENumber(4)]), EApp("h", [ENumber(4)]))
  )

(* Answer = 300 *)


(* let lam_eg1 = Program(
  [
  DFun("make_f", ["x"], ELambda(["y"], EPrim2(Plus, EId("x"), EId("y"))))
  ], 
  ELet([("f", EApp("make_f",[ENumber(10)]))], EApp("f", [ENumber(5)])))


let lam_eg2 = Program(
  [
  DFun("make_f", ["x";"z"], 
  ELet([("y", ENumber(92))], ELambda([], EPrim2(Plus, EId("x"), EId("y")))))
  ], 
  ELet([("f", EApp("make_f",[ENumber(37); ENumber(44)]))], EApp("f", [])))


let lam_eg3 = Program(
  [
  DFun("make_f", [], ELambda(["x"; "y"], EPrim2(Plus, EId("x"), EId("y"))))
  ], 
  ELet([("f", EApp("make_f",[]))], EApp("f", [ENumber(37); ENumber(92)]))) *)