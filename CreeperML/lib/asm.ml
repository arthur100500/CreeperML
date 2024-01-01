module Asm = struct
  open Anf.AnfTypeAst
  open Type_ast.TypeAst
  open Parser_ast.ParserAst
  open Counter

  type reg = string
  type storage = Stack of int | Reg of string | IntConst of int

  let reg_size = 16

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

  module MemMap = Map.Make (Int)
  module IntSet = Set.Make (Int)

  (* let render_storage = function
     | Stack i -> Format.sprintf "qword [esp - %d]" i
     | Reg r -> r *)

  let external_fns = [ (* 5 *) ]
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
    let reg_args, stack_args = split_args fn.args in
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

  let int_or_die x =
    match x.value with
    | LInt x -> x
    | LBool true -> 1
    | LBool false -> 0
    | LUnit -> 0
    | _ -> failwith "not done"

  let ld_imm mm imm =
    match imm with
    | ImmVal x -> MemMap.find x.value mm |> push |> fun x -> [ x ]
    | ImmLit l -> int_or_die l |> ic |> push |> fun x -> [ x ]

  let mem_imm mm imm =
    match imm with
    | ImmVal x -> MemMap.find x.value mm
    | ImmLit l -> int_or_die l |> ic

  let fix_align_16 instructions size =
    instructions @ [ sub rsp (Int.rem size 16 |> ic) ]

  let compile_push_args_regs mm (args : imm list) =
    let instr, _, size =
      List.fold_left
        (fun (xs, arg_i, s) arg ->
          let loc = mem_imm mm arg in
          match get_arg_loc arg_i with
          | Some reg -> (xs @ [ Mov (reg, loc) ], arg_i + 1, s)
          | None -> (xs @ ld_imm mm arg, arg_i + 1, s + sizeof arg))
        ([], 0, 0) args
    in
    fix_align_16 instr size

  let compile_push_args_stack mm (args : imm list) =
    let instr, _, size =
      List.fold_left
        (fun (xs, arg_i, s) arg ->
          xs @ ld_imm mm arg |> fun x -> (x, arg_i + 1, s + sizeof x))
        ([], 0, 0) args
    in
    fix_align_16 instr size

  let compile_push_args fn mm args =
    if List.mem fn.value external_fns |> not then compile_push_args_regs mm args
    else compile_push_args_stack mm args

  let rec compile_expr mm (e : anf_expr) =
    match e with
    | AApply (ImmVal fn, args) ->
        compile_push_args fn mm args @ compile_fn_call fn.value args
    | AImm i -> ld_imm mm i
    | Aite (i, t, e) ->
        let compile_block =
          List.fold_left (fun xs x -> xs @ compile_vb mm x) []
        in
        let if_id = Counter.cnt_next () in
        let mklabel name = [ Label (Format.sprintf "%s_%d" name if_id) ] in
        let cont = mklabel "cont" in
        let jmp_to_cont = [ Jmp (Format.sprintf "cont_%d" if_id) ] in
        let then_branch =
          compile_block t.lets @ ld_imm mm t.res @ jmp_to_cont
        in
        let else_branch =
          mklabel "else" @ compile_block e.lets @ ld_imm mm e.res @ cont
        in
        let if_part =
          ld_imm mm i
          @ [ pop rax; cmp rax (ic 0); je (Format.sprintf "else_%d" if_id) ]
        in
        if_part @ then_branch @ else_branch
    | _ -> failwith "not done"

  and compile_vb mm (vb : anf_val_binding) : instruction list =
    let store =
      match vb.name.typ with
      | TyGround TUnit -> []
      | _ -> MemMap.find vb.name.value mm |> pop |> fun x -> [ x ]
    in
    let expr = compile_expr mm vb.e in
    expr @ store

  let rec collect_expr_size (e : anf_val_binding) =
    match e.e with
    | Aite (_, t, e) ->
        (sum_by collect_expr_size) t.lets + (sum_by collect_expr_size) e.lets
    | _ -> sizeof e.name

  let compile_fn (fn : anf_fun_binding) =
    let name = Format.sprintf "fn%d" fn.name.value in
    let mm = map_fn_stack fn in
    let stack_len =
      (sum_by sizeof) fn.args
      + (sum_by collect_expr_size) fn.body.lets
      + sizeof fn.body.res
    in
    let sub_esp = [ sub rsp (stack_len + reg_size |> align16 |> ic) ] in
    let body =
      List.fold_left (fun xs x -> xs @ compile_vb mm x) [] fn.body.lets
    in
    let res = [ mov rax (mem_imm mm fn.body.res) ] in
    let save_base = [ push rbp; mov rbp rsp ] in
    let ret = [ mov rsp rbp; pop rbp; Ret ] in
    let save_args =
      let rec helper lst i instrs =
        match (lst, get_arg_loc i) with
        | [], _ -> instrs
        | hd :: tl, Some reg ->
            Mov (MemMap.find hd.value mm, reg) :: instrs |> helper tl (i + 1)
        | _, None -> instrs
      in
      helper fn.args 0 []
    in
    { name; body = save_base @ sub_esp @ save_args @ body @ res @ ret }

  let add_custom custom prog = prog @ custom

  let make_main x =
    {
      name = "main";
      body =
        [ push rbp; mov rbp rsp ]
        @ x
        @ [ mov rax (ic 0); mov rsp rbp; pop rbp; Ret ];
    }

  let compile ast =
    let fn_defs, main_fn =
      List.partition_map
        (function AnfFun fn -> Left fn | AnfVal v -> Right v)
        ast
    in
    let main_fn =
      {
        args = [];
        name = { value = -2; typ = TyGround TInt };
        body =
          {
            lets = main_fn;
            res = ImmLit { value = LInt 0; typ = TyGround TInt };
          };
      }
      |> compile_fn
      |> fun x -> { x with name = "main" }
    in
    main_fn :: List.map compile_fn fn_defs
end

module AsmRenderer = struct
  open Asm

  let rm = function
    | Stack d when d > 0 -> Format.sprintf "qword [rbp+%d]" d
    | Stack d when d < 0 -> Format.sprintf "qword [rbp%d]" d
    | Stack _ -> Format.sprintf "qword [rbp]"
    | Reg r -> r
    | IntConst c -> Format.sprintf "%d" c

  let render_instr = function
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
    List.map render_fn p |> String.concat "\n\n"
    |> Format.sprintf "extern print_int\nglobal main\n%s"
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
