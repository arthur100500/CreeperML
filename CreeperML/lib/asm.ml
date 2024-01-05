module Asm = struct
  open Anf.AnfTypeAst
  open Type_ast.TypeAst
  open Parser_ast.ParserAst
  open Counter
  open Type_ast.TypeAst

  type reg = string

  type storage =
    | Stack of int
    | Reg of reg
    | IntConst of int
    | Displacement of string * int
    | Fndef of string (* Function name? *)

  let reg_size = 16
  let void_ptr_size = 8

  type instruction =
    | Mov of storage * storage
    | Movzx of storage * storage
    | Add of storage * storage
    | Sub of storage * storage
    | Imul of storage * storage
    | Call of string
    | Push of storage
    | Pop of storage
    | Int of int
    | Ret
    | Jmp of string
    | Je of string
    | Jne of string
    | Cmp of storage * storage
    | Setge of storage
    | Setg of storage
    | Setle of storage
    | Setl of storage
    | Label of string (* bad *)

  type fn = { name : string; body : instruction list }

  let rax = Reg "rax"
  let rdi = Reg "rdi"
  let rsi = Reg "rsi"
  let al = Reg "al"
  let eax = Reg "eax"
  let rsp = Reg "rsp"
  let rbp = Reg "rbp"
  let rdx = Reg "rdx"
  let rcx = Reg "rcx"
  let r8d = Reg "r8d"
  let r9d = Reg "r9d"
  let r8 = Reg "r8"
  let r9 = Reg "r9"
  let r10 = Reg "r10"
  let r11 = Reg "r11"
  let r12 = Reg "r12"
  let pop l = Pop l
  let add r1 r2 = Add (r1, r2)
  let imul r1 r2 = Imul (r1, r2)
  let sub r1 r2 = Sub (r1, r2)
  let push r = Push r
  let push l = Push l
  let ic i = IntConst i
  let stack d = Stack d
  let mov r1 r2 = Mov (r1, r2)
  let setle r = Setle r
  let movzx r1 r2 = Movzx (r1, r2)
  let cmp r1 r2 = Cmp (r1, r2)
  let call r = Call r
  let je x = Je x
  let jne x = Jne x
  let jmp x = Jmp x
  let dp reg d = Displacement (reg, d)

  module MemMap = Map.Make (Int)
  module IntSet = Set.Make (Int)

  let rec render_storage = function
    | Stack d when d > 0 -> Format.sprintf "qword [rbp+%d]" d
    | Stack d when d < 0 -> Format.sprintf "qword [rbp%d]" d
    | Stack _ -> Format.sprintf "qword [rbp]"
    | Reg r -> r
    | IntConst c -> Format.sprintf "%d" c
    | Displacement (r, d) when d > 0 -> Format.sprintf "qword [%s+%d]" r d
    | Displacement (r, d) when d < 0 -> Format.sprintf "qword [%s%d]" r d
    | Displacement (r, _) -> Format.sprintf "qword [%s]" r
    | Fndef fname -> fname

  let external_fns = [ (* 5 *) ]

  module NameMap = Map.Make (Int)
  module ArityMap = Map.Make (String)

  let std_fns =
    (* TODO: bring it from main or other file *)
    NameMap.empty
    |> NameMap.add 1 "less_equals"
    |> NameMap.add 2 "minus" |> NameMap.add 3 "multiply" |> NameMap.add 4 "plus"
    |> NameMap.add 5 "print_int"

  let std_fns_arity = ArityMap.singleton "print_int" 1
  let sizeof _ = 8
  let align16 x = if Int.rem x 16 <> 0 then x - Int.rem x 16 + 16 else x
  let sum_by f = List.fold_left (fun xs x -> xs + f x) 0

  (*  %rdi, %rsi, %rdx, %rcx, %r8, and %r9 then stack *)
  let get_arg_loc = function
    | 0 -> Some rdi
    | 1 -> Some rsi
    | 2 -> Some rdx
    | 3 -> Some rcx
    | 4 -> Some r8d
    | 5 -> Some r9d
    | _ -> None

  let split_args lst =
    let rec helper lst i (acc1, acc2) =
      match (lst, get_arg_loc i) with
      | [], _ -> (acc1, acc2)
      | hd :: tl, Some _ -> helper tl (i + 1) (hd :: acc1, acc2)
      | hd :: tl, None -> helper tl (i + 1) (acc1, hd :: acc2)
    in
    helper lst 0 ([], [])

  let rec collect_locals l =
    match l.e with
    | Aite (_, t, e) ->
        List.concat_map collect_locals t.lets
        @ List.concat_map collect_locals e.lets
        @ [ l.name ]
    | _ -> [ l.name ]

  let map_fn_stack (fn : anf_fun_binding) =
    let reg_args, stack_args = split_args (fn.env @ fn.args) in
    let all_local_vars = List.concat_map collect_locals fn.body.lets in
    let args, last_reg_arg =
      List.fold_left
        (fun (mm, li) x ->
          let size = sizeof x.typ in
          (MemMap.add x.value (Stack li) mm, li - size))
        (MemMap.empty, -reg_size) reg_args
    in
    let args, _ =
      List.fold_left
        (fun (mm, li) x ->
          let size = sizeof x.typ in
          (MemMap.add x.value (Stack li) mm, li + size))
        (args, reg_size) stack_args
    in
    List.fold_left
      (fun (mm, li) x ->
        let size = sizeof x.typ in
        (MemMap.add x.value (Stack li) mm, li - size))
      (args, last_reg_arg) all_local_vars
    |> fst

  let map_main_stack (vbs : anf_val_binding list) =
    List.fold_left
      (fun (mm, li) (x : anf_val_binding) ->
        let name = x.name in
        let size = sizeof name.typ in
        (MemMap.add x.name.value (Stack li) mm, li + size))
      (MemMap.empty, reg_size) vbs
    |> fst

  let compile_fn_call fn args =
    let stack_len =
      List.filteri (fun i _ -> Option.is_none (get_arg_loc i)) args
      |> List.fold_left (fun xs x -> xs + sizeof x) 0
    in
    let stack_len_ext = List.fold_left (fun xs x -> xs + sizeof x) 0 args in
    let stack_len =
      if List.mem fn external_fns then stack_len_ext else stack_len
    in
    let stack_fix = [ add rsp (stack_len |> align16 |> ic) ] in
    match fn with
    (* Inline function cal here *)
    | 1 -> [ cmp rdi rsi; setle al; movzx eax al; push rax ] (* <= *)
    | 2 -> [ sub rdi rsi; mov rax rdi; push rax ] (* - *)
    | 3 -> [ imul rdi rsi; mov rax rdi; push rax ] (* * *)
    | 4 -> [ add rdi rsi; mov rax rdi; push rax ] (* + *)
    | 5 -> [ call "print_int" ] @ stack_fix (* print int )*)
    | other -> [ call (Format.sprintf "fn%d" other) ] @ stack_fix @ [ push rax ]

  let int_of_literal x =
    match x.value with
    | LInt x -> x
    | LBool true -> 1
    | LBool false -> 0
    | LUnit -> 0
    | _ -> failwith "not done"

  let rec arity_of_typ t =
    match t with TyArrow (_, t2) -> 1 + arity_of_typ t2 | _ -> 0

  let rec alloc_closure all_fns mm fn args =
    let arity = List.length args + arity_of_typ fn.typ in
    let fn_name =
      match NameMap.find_opt fn.value std_fns with
      | None -> Format.sprintf "fn%d" fn.value
      | Some x -> x
    in
    let load_size = [ mov rsi (arity * void_ptr_size |> ic) ] in
    let create_env_ptr = [ call "cm_malloc" ] in
    let store_args =
      [ push r9 ]
      @ (List.mapi
           (fun i arg ->
             [
               mov r9 (mem_imm all_fns mm arg);
               mov (dp "rax" (i * void_ptr_size)) r9;
             ])
           args
        |> List.concat)
      @ [ pop r9 ]
    in
    let load_argv = [ mov rdx rax ] in
    let load_argc = [ mov rsi (List.length args |> ic) ] in
    let load_fn = [ mov rdi (Fndef fn_name) ] in
    (* function *clsr, cmptr argc, cmptr *argv *)
    let create_closure = [ call "create_function" ] in
    let push_res = [ push rax ] in
    load_size @ create_env_ptr @ load_argv @ store_args @ load_argc @ load_fn
    @ create_closure @ push_res

  and mem_imm fns mm imm =
    match imm with
    | ImmVal v when List.mem v.value fns ->
        Fndef (Format.sprintf "fn%d" v.value)
    | ImmVal x -> MemMap.find x.value mm
    | ImmLit l -> int_of_literal l |> ic

  let ld_imm fns mm imm =
    match imm with
    | ImmVal v when List.mem v.value fns -> alloc_closure fns mm v []
    | ImmVal v when MemMap.mem v.value std_fns -> alloc_closure fns mm v []
    | _ -> mem_imm fns mm imm |> push |> fun x -> [ x ]

  let fix_align_16 instructions size =
    instructions @ [ sub rsp (Int.rem size 16 |> ic) ]

  let compile_push_args_regs fns mm (args : imm list) =
    let instr, _, size =
      List.fold_left
        (fun (xs, arg_i, s) arg ->
          let loc = mem_imm fns mm arg in
          match get_arg_loc arg_i with
          | Some reg -> (xs @ [ Mov (reg, loc) ], arg_i + 1, s)
          | None -> (xs @ ld_imm fns mm arg, arg_i + 1, s + sizeof arg))
        ([], 0, 0) args
    in
    fix_align_16 instr size

  let compile_push_args_stack fns mm (args : imm list) =
    let instr, _, size =
      List.fold_left
        (fun (xs, arg_i, s) arg ->
          xs @ ld_imm fns mm arg |> fun x -> (x, arg_i + 1, s + sizeof x))
        ([], 0, 0) args
    in
    fix_align_16 instr size

  let compile_push_args fn fns mm args =
    if List.mem fn.value external_fns |> not then
      compile_push_args_regs fns mm args
    else compile_push_args_stack fns mm args

  let rec compile_expr mm all_fns (e : anf_expr) =
    match e with
    | AApply (ImmVal fn, args) when MemMap.mem fn.value std_fns ->
        compile_push_args fn all_fns mm args @ compile_fn_call fn.value args
    | AApply (ImmVal fn, args) ->
        let argc = List.length args in
        (* Allocate array (at r11) *)
        let allocate_args_arr =
          [
            mov rdi (argc * void_ptr_size |> ic); call "cm_malloc"; mov r12 rax;
          ]
        in
        (* Move arguments to allocated array *)
        let store_args =
          List.mapi
            (fun i arg ->
              ld_imm all_fns mm arg @ [ pop (dp "r12" (i * void_ptr_size)) ])
            args
          |> List.concat
        in
        (* Create and load self to rdi (1) *)
        let load_self =
          if List.mem fn.value all_fns then
            alloc_closure all_fns mm fn [] @ [ pop rdi ]
          else [ push (mem_imm all_fns mm (ImmVal fn)); pop rdi ]
        in
        (* Move argc to rsi (2) *)
        let load_argc = [ mov rsi (argc |> ic) ] in
        (* Move argv from r11 to rdx (3) *)
        let load_argv = [ mov rdx r12 ] in
        (* Call apply and save *)
        let call_and_save = [ call "apply_args"; push rax ] in
        [ push r12; sub rsp (ic 8) ]
        @ allocate_args_arr @ store_args @ load_self @ load_argc @ load_argv
        @ [ add rsp (ic 8); pop r12 ]
        @ call_and_save
    | AImm i -> ld_imm all_fns mm i
    | Aite (i, t, e) ->
        let compile_block =
          List.fold_left (fun xs x -> xs @ compile_vb mm all_fns x) []
        in
        let if_id = Counter.cnt_next () in
        let mklabel name = [ Label (Format.sprintf "%s_%d" name if_id) ] in
        let cont = mklabel "cont" in
        let jmp_to_cont = [ Jmp (Format.sprintf "cont_%d" if_id) ] in
        let then_branch =
          compile_block t.lets @ ld_imm all_fns mm t.res @ jmp_to_cont
        in
        let else_branch =
          mklabel "else" @ compile_block e.lets @ ld_imm all_fns mm e.res @ cont
        in
        let if_part =
          ld_imm all_fns mm i
          @ [ pop rax; cmp rax (ic 0); je (Format.sprintf "else_%d" if_id) ]
        in
        if_part @ then_branch @ else_branch
    | AClosure (fn, args) -> alloc_closure all_fns mm fn args
    | _ -> failwith "not done"

  and compile_vb mm all_fns (vb : anf_val_binding) : instruction list =
    let store =
      match vb.name.typ with
      | TyGround TUnit -> []
      | _ -> MemMap.find vb.name.value mm |> pop |> fun x -> [ x ]
    in
    let expr = compile_expr mm all_fns vb.e in
    expr @ store

  let rec collect_expr_size (e : anf_val_binding) =
    match e.e with
    | Aite (_, t, e) ->
        (sum_by collect_expr_size) t.lets + (sum_by collect_expr_size) e.lets
    | _ -> sizeof e.name

  let add_fn_stuff x =
    let save_base = [ push rbp; mov rbp rsp ] in
    let ret = [ mov rsp rbp; pop rbp; Ret ] in
    save_base @ x @ ret

  let compile_fn all_fns (fn : anf_fun_binding) =
    let name = Format.sprintf "fn%d" fn.name.value in
    let mm = map_fn_stack fn in
    let stack_len =
      (sum_by sizeof) fn.args
      + (sum_by collect_expr_size) fn.body.lets
      + sizeof fn.body.res
    in
    let sub_esp = [ sub rsp (stack_len + reg_size |> align16 |> ic) ] in
    let body =
      List.fold_left (fun xs x -> xs @ compile_vb mm all_fns x) [] fn.body.lets
    in
    let res = [ mov rax (mem_imm all_fns mm fn.body.res) ] in

    let save_args =
      let rec helper lst i instrs =
        match (lst, get_arg_loc i) with
        | [], _ -> instrs
        | hd :: tl, Some reg ->
            Mov (MemMap.find hd.value mm, reg) :: instrs |> helper tl (i + 1)
        | _, None -> instrs
      in
      helper (fn.env @ fn.args) 0 []
    in
    { name; body = add_fn_stuff (sub_esp @ save_args @ body @ res) }

  let create_arity_fn fn_defs =
    let fn_count = List.length fn_defs in
    let real_fn_count =
      fn_count + ArityMap.fold (fun _ _ x -> x + 1) std_fns_arity 0
    in
    List.mapi
      (fun i (fn : anf_fun_binding) ->
        [
          Label (Format.sprintf "fn_check_%d" i);
          mov rcx (Fndef (Format.sprintf "fn%d" fn.name.value));
          cmp rdi rcx;
          jne (Format.sprintf "fn_check_%d" (i + 1));
          mov rax (ic (List.length fn.args + List.length fn.env));
          jmp (Format.sprintf "fn_check_%d" real_fn_count);
        ])
      fn_defs
    @ List.mapi
        (fun i (k, v) ->
          [
            Label (Format.sprintf "fn_check_%d" (i + fn_count));
            mov rcx (Fndef k);
            cmp rdi rcx;
            jne (Format.sprintf "fn_check_%d" (i + fn_count + 1));
            mov rax (ic v);
            jmp (Format.sprintf "fn_check_%d" real_fn_count);
          ])
        (ArityMap.to_seq std_fns_arity |> List.of_seq)
    |> List.concat
    |> fun x ->
    [ mov rax (ic 0) ]
    @ x
    @ [ Label (Format.sprintf "fn_check_%d" real_fn_count) ]
    |> add_fn_stuff
    |> fun instr -> { name = "get_arity"; body = instr }

  let compile ast =
    let fn_defs, main_fn =
      List.partition_map
        (function AnfFun fn -> Left fn | AnfVal v -> Right v)
        ast
    in
    let all_fns =
      List.map (fun (x : anf_fun_binding) -> x.name.value) fn_defs
    in
    let main_fn =
      {
        args = [];
        env = [];
        name = { value = -2; typ = TyGround TInt };
        body =
          {
            lets = main_fn;
            res = ImmLit { value = LInt 0; typ = TyGround TInt };
          };
      }
      |> compile_fn all_fns
      |> fun x -> { x with name = "main" }
    in
    let get_arity_fn = create_arity_fn fn_defs in
    main_fn :: get_arity_fn :: List.map (compile_fn all_fns) fn_defs
end

module AsmRenderer = struct
  open Asm

  let header =
    {|
extern print_int
extern create_function
extern apply_args
extern cm_malloc

extern get_arity ; out

global main
|}

  let rec rm = function
    | Stack d when d > 0 -> Format.sprintf "qword [rbp+%d]" d
    | Stack d when d < 0 -> Format.sprintf "qword [rbp%d]" d
    | Stack _ -> Format.sprintf "qword [rbp]"
    | Reg r -> r
    | IntConst c -> Format.sprintf "%d" c
    | Displacement (r, d) when d > 0 -> Format.sprintf "qword [%s+%d]" r d
    | Displacement (r, d) when d < 0 -> Format.sprintf "qword [%s%d]" r d
    | Displacement (r, _) -> Format.sprintf "qword [%s]" r
    | Fndef fname -> fname

  let not_reg = function Reg _ -> false | _ -> true

  let render_instr = function
    | Mov (src, dst) when not_reg src && not_reg dst ->
        Format.sprintf "mov r9, %s\n  mov %s, r9" (rm dst) (rm src)
    | Mov (src, dst) -> Format.sprintf "mov %s, %s" (rm src) (rm dst)
    | Movzx (src, dst) -> Format.sprintf "movzx %s, %s" (rm src) (rm dst)
    | Add (src, dst) -> Format.sprintf "add %s, %s" (rm src) (rm dst)
    | Sub (src, dst) -> Format.sprintf "sub %s, %s" (rm src) (rm dst)
    | Cmp (src, dst) -> Format.sprintf "cmp %s, %s" (rm src) (rm dst)
    | Imul (src, dst) -> Format.sprintf "imul %s, %s" (rm src) (rm dst)
    | Call fn -> Format.sprintf "call %s" fn
    | Push src -> rm src |> Format.sprintf "push %s"
    | Pop dst -> rm dst |> Format.sprintf "pop %s"
    | Int _ -> "int 0x80"
    | Ret -> "ret"
    | Label m -> Format.sprintf "%s:" m
    | Jmp d -> Format.sprintf "jmp %s" d
    | Je d -> Format.sprintf "je %s" d
    | Jne d -> Format.sprintf "jne %s" d
    | Setge d -> Format.sprintf "setge %s" (rm d)
    | Setle d -> Format.sprintf "setle %s" (rm d)
    | Setl d -> Format.sprintf "setl %s" (rm d)
    | Setg d -> Format.sprintf "setg %s" (rm d)

  let render_instrs il =
    let add_offset s = Format.sprintf "  %s" s in
    List.map render_instr il |> List.map add_offset |> String.concat "\n"

  let render_fn p =
    let nm_dec = Format.sprintf "%s:\n" p.name in
    Format.sprintf "%s%s" nm_dec (render_instrs p.body)

  let render (p : fn list) =
    List.map render_fn p |> String.concat "\n\n" |> Format.sprintf "%s%s" header
end

module AsmOptimizer = struct
  open Asm

  let meaningful_instr = function
    | Mov (x, y) when x = y -> false
    | Add (_, IntConst 0) -> false
    | Sub (_, IntConst 0) -> false
    | _ -> true

  let optimize_fn fn = { fn with body = List.filter meaningful_instr fn.body }
  let optimize = List.map optimize_fn
end

module Exe = struct
  let create_newdir path perm =
    if not (Sys.file_exists path) then Sys.mkdir path perm

  let build_dir = "cm_build"

  let make_exe program =
    let () = create_newdir build_dir 0o777 in
    let asm_out = open_out (build_dir ^ "/program.asm") in
    let () = Printf.fprintf asm_out "%s" (AsmRenderer.render program) in
    let () = close_out asm_out in
    let _ = Sys.command "./build.sh" in
    ()
end
