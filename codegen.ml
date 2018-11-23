module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (begin_block, loop_block, end_block, config_block) =

  let context = L.global_context () in

	(* Create the LLVM compilation module into which
	 we will generate code *)
	let the_module = L.create_module context "Bawk" in

	(* Get types from the context *)
	let i32_t      = L.i32_type    context
	and i8_t       = L.i8_type     context
	and i1_t       = L.i1_type     context
	and str_t 	   = L.pointer_type ( L.i8_type context ) 
	and void_t     = L.void_type   context in

	(* Return the LLVM type for a bawk type *)
	let ltype_of_typ = function
	  A.Int   -> i32_t
	| A.Bool  -> i1_t
	| A.Void  -> void_t
	| A.String -> str_t
  | _ -> raise (Failure "types no pattern match")
	in

  (* Declare built-in functions *)
	let printf_t : L.lltype = 
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
    L.declare_function "printf" printf_t the_module in

  let int_to_string_t : L.lltype =
    L.function_type str_t [| i32_t |] in
  let int_to_string_func : L.llvalue =
    L.declare_function "int_to_string" int_to_string_t the_module in

  let string_to_int_t : L.lltype =
    L.function_type i32_t [| str_t |] in
  let string_to_int_func : L.llvalue = 
    L.declare_function "string_to_int" string_to_int_t the_module in


  let ftype = L.function_type void_t [||] in
  let ltype : L.lltype = 
    L.function_type void_t [| str_t |] in
 
  (* Loop and end LLVM functions *)
  let loop_func = L.define_function "loop" ltype the_module in
  let loopbuilder = L.builder_at_end context (L.entry_block loop_func) in

  let end_func = L.define_function "end" ftype the_module in
  let endbuilder = L.builder_at_end context (L.entry_block end_func) in

  (*--- Build begin block: globals ---*)
  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) = 
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty (fst begin_block) in


  (*--- Build begin block: function declarations ---*)
  let function_decls : (L.llvalue * A.func_decl) StringMap.t =
    let function_decl m fdecl =
      let formal_types =
	      Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      and name = fdecl.A.fname
      in let ftype = L.function_type (ltype_of_typ fdecl.ret_type) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
   
    List.fold_left function_decl StringMap.empty (snd begin_block)
    
  in

  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
	    Some _ -> ()
      | None -> ignore (instr builder)
  
  in

  

  (*--- Build function bodies defined in BEGIN block ---*)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let func_builder = L.builder_at_end context (L.entry_block the_function) in
    let string_format_str builder = L.build_global_stringptr "%s\n" "fmt" builder in
    
    let local_vars =
      let add_formal m (t, n) p = 
        L.set_value_name n p; (* p = LLVM value of param from function declaration we created earlier, n = name from fdecl *)
	      let local = L.build_alloca (ltype_of_typ t) n func_builder in
          ignore (L.build_store p local func_builder);
	      StringMap.add n local m

      and add_local m (t, n) =
	      let local_var = L.build_alloca (ltype_of_typ t) n func_builder
	      in StringMap.add n local_var m
      
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in

      List.fold_left add_local formals fdecl.A.locals
   
    in

    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    
    in

    let rec expr builder = function
      A.StringLiteral s -> let l = L.define_global "" (L.const_stringz context s) the_module in
			  L.const_bitcast (L.const_gep l [|L.const_int i32_t 0|]) str_t
			| A.BoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | A.Literal i -> L.const_int i32_t i
      | A.Noexpr -> L.const_int i32_t 0
      | A.Id i -> L.build_load (lookup i) i builder
      | A.Assign (e1, e2) ->
          let lhs = match e1 with
            (* TODO: A.ArrayDeref *)
            A.Id i -> lookup i
          and rhs = expr builder e2
          in ignore(L.build_store rhs lhs builder); rhs
      (* TODO: handle strings and rgx by building binop_gen function (see Decaf)*)
      | A.Binop(e1, op, e2) ->
        let e1' = expr builder e1
        and e2' = expr builder e2 in
        (match op with
          A.Add -> L.build_add
          | A.Sub     -> L.build_sub
    	    | A.Mult    -> L.build_mul
    	    | A.Div     -> L.build_sdiv
          | A.And     -> L.build_and
    	    | A.Or      -> L.build_or
    	    | A.Equal   -> L.build_icmp L.Icmp.Eq
    	    | A.Neq     -> L.build_icmp L.Icmp.Ne
    	    | A.Less    -> L.build_icmp L.Icmp.Slt
    	    | A.Leq     -> L.build_icmp L.Icmp.Sle
    	    | A.Greater -> L.build_icmp L.Icmp.Sgt
    	    | A.Geq     -> L.build_icmp L.Icmp.Sge
          | _         -> raise (Failure "no binary operation")
        ) e1' e2' "tmp" builder
      | A.Unop(uop, e) ->
        let e' = expr builder e in
        (match uop with
          A.Neg -> L.build_neg
          | A.Not -> L.build_not
          | _ -> raise (Failure "no unary operation")
        ) e' "tmp" builder
      | A.Call ("print", [e]) ->
    		L.build_call printf_func [| string_format_str builder; (expr builder e)|] "printf" builder
      | A.Call (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (expr builder) (List.rev args)) in
	      let result = (match fdecl.A.ret_type with 
                        A.Void -> ""
                      | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list llargs) result builder
      | _ -> raise (Failure "begin expr no pattern match") 
        
    in 

    let rec stmt builder = function
      A.Expr ex -> ignore(expr builder ex); builder 
      | A.Block sl -> List.fold_left stmt builder sl
      | A.Return e -> ignore (match fdecl.A.ret_type with
          A.Void -> L.build_ret_void builder
          | _ -> L.build_ret (expr builder e) builder); builder
      | _ -> raise (Failure "stmt no pattern match")
    in

    let func_builder = stmt func_builder (Block fdecl.A.body) in

    (* return types for functions *)
    add_terminal func_builder (match fdecl.ret_type with
      A.Void -> L.build_ret_void
      | A.Int -> L.build_ret (L.const_int i32_t 0)
      | A.Bool -> L.build_ret (L.const_int i1_t 0)
      | A.String -> L.build_ret (L.const_pointer_null str_t)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))

  (* end of build_func_body *)
    
  in
  
  let local_vars =
    let add_local m (t, n) =
	    let local_var = L.build_alloca (ltype_of_typ t) n loopbuilder
	    in StringMap.add n local_var m
      
    in
    List.fold_left add_local StringMap.empty (fst loop_block)
  in

  let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars

  in

  let string_format_str builder = L.build_global_stringptr "%s\n" "fmt" loopbuilder in

  let rec expr builder = function
    A.StringLiteral s -> let l = L.define_global "" (L.const_stringz context s) the_module in
      L.const_bitcast (L.const_gep l [|L.const_int i32_t 0|]) str_t
    | A.Literal i -> L.const_int i32_t i
    | A.Id s -> L.build_load (lookup s) s builder 
    | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | A.Assign (e1, e2) ->
      let lhs = match e1 with
        (* TODO: A.ArrayDeref *)
        A.Id i -> lookup i
      and rhs = expr builder e2
      in ignore(L.build_store rhs lhs builder); rhs
    | A.Call ("print", [e]) ->
      L.build_call printf_func [| string_format_str builder; (expr builder e) |] "printf" builder
    | A.Call ("int_to_string", [e]) -> L.build_call int_to_string_func [| expr builder e |] "int_to_string" builder
    | A.Call ("string_to_int", [e]) -> L.build_call string_to_int_func [| expr builder e |] "string_to_int" builder
    | A.Call (f, args) ->
      let (fdef, fdecl) = StringMap.find f function_decls in
      let llargs = List.rev (List.map (expr builder) (List.rev args)) in
	    let result = (match fdecl.A.ret_type with 
        A.Void -> ""
        | _ -> f ^ "_result") in
        L.build_call fdef (Array.of_list llargs) result builder
    | A.Assign (e1, e2) ->
      let lhs = expr builder e1 and 
      rhs = expr builder e2
      in ignore(L.build_store rhs lhs builder); rhs
    | _ -> raise (Failure "end expr no pattern match") 
    in 

  let rec stmt builder = function
    A.Expr ex -> ignore (expr builder ex); builder
    | A.Block sl -> List.fold_left stmt builder sl
    | _ -> raise (Failure "statement no pattern match")
    
  in




  (*--- Build loop block ---*)
  let build_loop_block loop_block =
    ignore(stmt loopbuilder (Block (snd loop_block)));
    add_terminal loopbuilder L.build_ret_void
  
    in 

  (*--- Build end block ---*)
  let build_end_block end_block =
    ignore(stmt endbuilder (Block (snd end_block)));
    add_terminal endbuilder L.build_ret_void

  in

  (* Call the things that happen in main *)
  List.iter build_function_body (snd begin_block);
  ignore (build_loop_block loop_block);
  ignore (build_end_block end_block);

  the_module



