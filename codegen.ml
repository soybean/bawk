(* Authors: Ashley, Christine, Melanie, Victoria *)
(* Loosely based on MicroC *)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let translate (begin_block, loop_block, end_block, config_block) =

  let context = L.global_context () in

	(* Create the LLVM compilation module into which
	 we will generate code *)
	let the_module = L.create_module context "Bawk" in

	(* Get types from the context *)
	let i32_t	= L.i32_type    context
  	and i64_t	= L.i64_type    context in
        let i8_t	= L.i8_type     context
  	and  i1_t       = L.i1_type     context
	and str_t	= L.pointer_type ( L.i8_type context ) in
  	let node_t	= let node_t = L.named_struct_type context "Node" in
                   L.struct_set_body node_t [| i64_t ; L.pointer_type node_t |] false;
                   node_t in
  	let node_p_t	= L.pointer_type node_t in
  	let arr_t	= let arr_t = L.named_struct_type context "Array" in
                   	L.struct_set_body arr_t [| node_p_t ; i32_t ; i32_t |] false;
                   	arr_t in
  	let arr_p_t	= L.pointer_type arr_t in
  	let void_t	= L.void_type context in

	(* Return the LLVM type for a bawk type *)
	let ltype_of_typ = function
	  A.Int   -> i32_t
	| A.Bool  -> i1_t
	| A.Void  -> void_t
	| A.String -> str_t
        | A.Rgx -> str_t
  	| A.ArrayType _ -> arr_p_t
  	in
 

  (* Array helper functions *)
  	let arr_elem_type = function
    		A.ArrayType t -> t
    		| _ -> raise (Failure "not an array type")
  	in

  let rec arr_depth = function
    A.ArrayType t -> (arr_depth t) + 1
    | _ -> 0
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

  let bool_to_string_t : L.lltype =
    L.function_type str_t [| i1_t |] in
  let bool_to_string_func : L.llvalue = 
    L.declare_function "bool_to_string" bool_to_string_t the_module in

  let string_to_int_t : L.lltype =
    L.function_type i32_t [| str_t |] in
  let string_to_int_func : L.llvalue = 
    L.declare_function "string_to_int" string_to_int_t the_module in

  let concat_t : L.lltype =
    L.function_type str_t [| str_t; str_t|] in
  let concat_func : L.llvalue =
    L.declare_function "concat" concat_t the_module in

  let rgxcomp_t : L.lltype =
    L.function_type i1_t [| str_t; str_t |] in
  let rgxcomp_func : L.llvalue =
    L.declare_function "comp" rgxcomp_t the_module in
  let rgxnot_func : L.llvalue =
    L.declare_function "ncomp" rgxcomp_t the_module in

  let rgxeq_func : L.llvalue =
    L.declare_function "equals" rgxcomp_t the_module in
  let rgxneq_func : L.llvalue =
    L.declare_function "nequals" rgxcomp_t the_module in

  let rgx_to_str_t : L.lltype =
    L.function_type str_t [| str_t |] in
  let rgx_to_str_func : L.llvalue = 
    L.declare_function "rgx_to_string" rgx_to_str_t the_module in

  let access_t : L.lltype =
    L.function_type str_t [| str_t; i32_t|] in
  let access_func : L.llvalue =
    L.declare_function "access" access_t the_module in

  let strequals_t : L.lltype =
    L.function_type i1_t [| str_t; str_t |] in
  let strequals_func : L.llvalue = 
    L.declare_function "streq" strequals_t the_module in

  let strnequals_t : L.lltype =
    L.function_type i1_t [| str_t; str_t |] in
  let strnequals_func : L.llvalue = 
    L.declare_function "strneq" strnequals_t the_module in

  let strless_func : L.llvalue =
    L.declare_function "strless" strequals_t the_module in

  let strgreater_func : L.llvalue =
    L.declare_function "strgreater" strequals_t the_module in

  let strgeq_func : L.llvalue =
    L.declare_function "strgeq" strequals_t the_module in

  let strleq_func : L.llvalue =
    L.declare_function "strleq" strequals_t the_module in

  let numfields_t : L.lltype =
    L.function_type i32_t [| str_t |] in
  let numfields_func : L.llvalue =
    L.declare_function "numfields" numfields_t the_module in

  (* array functions *)
  let initlist_t : L.lltype =
    L.function_type arr_p_t [| i64_t; i32_t |] in
  let initlist_func : L.llvalue =
    L.declare_function "initList" initlist_t the_module in

  let addfront_t : L.lltype =
    L.function_type node_p_t [| arr_p_t; i64_t|] in
  let addfront_func : L.llvalue =
    L.declare_function "addFront" addfront_t the_module in

  let arrayderef_t : L.lltype =
    L.function_type i64_t [| arr_p_t; i32_t |] in
  let arrayderef_func : L.llvalue =
    L.declare_function "getElement" arrayderef_t the_module in

  let assign_t : L.lltype =
    L.function_type i32_t [| arr_p_t; i32_t; i64_t |] in
  let assign_func : L.llvalue =
    L.declare_function "assignElement" assign_t the_module in

  let reverse_t : L.lltype =
    L.function_type i8_t [| arr_p_t |] in
  let reverse_func : L.llvalue =
    L.declare_function "reverseList" reverse_t the_module in

  let length_t : L.lltype =
    L.function_type i32_t [| arr_p_t |] in
  let length_func : L.llvalue =
    L.declare_function "length" length_t the_module in

  let delete_t : L.lltype =
    L.function_type i64_t [| arr_p_t; i32_t |] in
  let delete_func : L.llvalue =
    L.declare_function "removeNode" delete_t the_module in

  let insert_t : L.lltype =
    L.function_type i8_t [| arr_p_t; i32_t; i64_t |] in
  let insert_func : L.llvalue =
    L.declare_function "insertElement" insert_t the_module in

  let compare_t : L.lltype =
    L.function_type i32_t [| i64_t ; i64_t |] in

  let compare_p_t = L.pointer_type compare_t in

  let compareint_func : L.llvalue =
    L.declare_function "compareInts" compare_t the_module in
  let comparebools_func : L.llvalue =
    L.declare_function "compareBools" compare_t the_module in
  let comparestr_func : L.llvalue =
    L.declare_function "compareStrs" compare_t the_module in

  let contains_t : L.lltype =
    L.function_type i1_t [| arr_p_t ; i64_t ; compare_p_t |] in
  let contains_func : L.llvalue =
    L.declare_function "contains" contains_t the_module in

  let indexof_t : L.lltype =
    L.function_type i32_t [| arr_p_t ; i64_t ; compare_p_t |] in
  let indexof_func : L.llvalue =
    L.declare_function "findIndexOfNode" indexof_t the_module in

  (*--- Build begin block: globals ---*)
  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) = 
      let int_init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n int_init the_module) m
    in

    List.fold_left global_var StringMap.empty (fst begin_block)

  in

  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
	    Some _ -> ()
      | None -> ignore (instr builder) in

  (*---Build config block ---*)
  let build_config_block (rs, fs) =
    let build_global_str str name =
      let const_str = L.define_global ("_" ^ name) (L.const_stringz context str) the_module in
      let _ = L.set_unnamed_addr true const_str;
              L.set_global_constant true const_str;
              L.set_linkage L.Linkage.Private const_str;
              L.set_alignment 1 const_str;
      in
      let get_ptr = L.const_in_bounds_gep const_str [| L.const_int i32_t 0; L.const_int i32_t 0 |] in
      (*let _ = raise (Failure "shid") in *)
      L.define_global name get_ptr the_module
    in
    ignore(build_global_str rs "RS");
    ignore(build_global_str fs "FS");

  in

  (*--- Build begin block: function declarations ---*)
  let function_decls_no_loop_end : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let formal_types =
	      Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      and name = fdecl.sfname
      in let ftype = L.function_type (ltype_of_typ fdecl.sret_type) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
   
    List.fold_left function_decl StringMap.empty (snd begin_block)
    
  in

	let function_decls_loop : (L.llvalue * sfunc_decl) StringMap.t =
		let function_decl m fdecl =
			let formal_types =
				Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
			and name = fdecl.sfname
			in let ftype = L.function_type (ltype_of_typ fdecl.sret_type) formal_types in
			StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    function_decl function_decls_no_loop_end (loop_block) in

	let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
		let function_decl m fdecl =
			let formal_types =
				Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
			and name = fdecl.sfname
			in let ftype = L.function_type (ltype_of_typ fdecl.sret_type) formal_types in
			StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    function_decl function_decls_loop (end_block)
        in

  (*--- Build function bodies defined in BEGIN block ---*)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let func_builder = L.builder_at_end context (L.entry_block the_function) in
    let string_format_str builder = L.build_global_stringptr "%s\n" "fmt" builder in
    let nstring_format_str builder = L.build_global_stringptr "%s" "fmt" builder in
    
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

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in

      List.fold_left add_local formals fdecl.slocals
   
    in

    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    
    in

    let rec expr builder((ty,e): sexpr) = match e with
      SStringLiteral s -> let l = L.define_global "" (L.const_stringz context s) the_module in
			  L.const_bitcast (L.const_gep l [|L.const_int i32_t 0|]) str_t
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SLiteral i -> L.const_int i32_t i
      | SNoexpr -> L.const_int i32_t 0
      | SArrayLit a -> array_gen builder ty a
      | SId i -> L.build_load (lookup i) i builder
      | SAssign (e1, e2) ->
          let (_, e) = e1 in
          let rhs = expr builder e2 in
         (match e with 
             SId i -> ignore (L.build_store rhs (lookup i) builder); rhs
           | SArrayDeref (ar, idx) ->
                let (ty, _) = ar in
                let arr_type = 
                        match ty with
                        A.ArrayType t -> t
                      | _ -> raise(Failure "ArrayDeref assign is not an array") in
                let long = 
                        match arr_type with
                        A.Int | A.Bool-> L.build_zext rhs i64_t "arrayDerefAssign" builder
                      | A.String | A.Rgx -> L.build_pointercast rhs i64_t "arrayDerefAssign" builder
                      | A.ArrayType _ -> L.build_pointercast rhs i64_t "arrayDerefAssign" builder
                      | _ -> raise(Failure "unmatched type")
                in ignore (L.build_call assign_func [| expr builder ar; expr builder idx; long |] "assignElement" builder); rhs
           | _ -> raise (Failure "No match on left"))
      | SArrayDeref (ar, idx) ->
        let (ty, _) = ar in
        let arr_type = 
                match ty with
                A.ArrayType t -> t
              | _ -> raise(Failure "ArrayDeref is not an array") in
        let v = L.build_call arrayderef_func [| expr builder ar; expr builder idx |] "getElement" builder in
        (match arr_type with
            A.String | A.Rgx -> L.build_inttoptr v str_t "arrayDeref" builder
          | A.Int -> L.build_trunc v i32_t "arrayDeref" builder
          | A.Bool -> L.build_trunc v i1_t "arrayDeref" builder
          | A.ArrayType _ -> L.build_inttoptr v arr_p_t "arrayDeref" builder
          | _ -> raise (Failure "Cannot find array type"))
      | SBinop(e1, op, e2) ->
        let e1' = expr builder e1
        and e2' = expr builder e2 in
        let int_binop op = 
          (match op with
              A.Add -> L.build_add
            | A.Sub -> L.build_sub
            | A.Mult -> L.build_mul
            | A.Div -> L.build_sdiv
            | A.And -> L.build_and
            | A.Or -> L.build_or
            | A.Equal -> L.build_icmp L.Icmp.Eq
            | A.Neq -> L.build_icmp L.Icmp.Ne
            | A.Less -> L.build_icmp L.Icmp.Slt
            | A.Leq -> L.build_icmp L.Icmp.Sle
            | A.Greater -> L.build_icmp L.Icmp.Sgt
            | A.Geq -> L.build_icmp L.Icmp.Sge
          ) e1' e2' "tmp" builder in
        let str_binop op =
          (match op with
              A.Equal -> L.build_call strequals_func [| e1'; e2' |] "streq" builder
            | A.Neq -> L.build_call strnequals_func [| e1'; e2' |] "strneq" builder
            | A.Greater -> L.build_call strgreater_func [| e1'; e2' |] "strgreater" builder
            | A.Less -> L.build_call strless_func [| e1'; e2' |] "strless" builder
            | A.Leq -> L.build_call strleq_func [| e1'; e2' |] "strleq" builder
            | A.Geq -> L.build_call strgeq_func [| e1'; e2' |] "strgeq" builder
            | _ -> raise(Failure "Invalid operation on string")
          ) in
        let bool_binop op = 
          (match op with
              A.Equal -> L.build_icmp L.Icmp.Eq
              | A.Neq -> L.build_icmp L.Icmp.Ne
              | A.And -> L.build_and
              | A.Or -> L.build_or
              | _ -> raise (Failure "Boolean operator not supported")) e1' e2' "tmp" builder in
        if (L.type_of e1' = i32_t) then int_binop op
        else if (L.type_of e1' = i1_t) then bool_binop op
        else str_binop op
      | SUnop(uop, e) ->
        let e' = expr builder e in
        (match uop with
            A.Neg -> L.build_neg
          | A.Not -> L.build_not
        ) e' "tmp" builder; 
      | SStrcat(e1, e2) -> L.build_call concat_func [| expr builder e1; expr builder e2 |] "concat" builder
      | SRgxcomp (e1, e2) -> L.build_call rgxcomp_func [| expr builder e1; expr builder e2 |] "comp" builder
      | SRgxnot (e1, e2) -> L.build_call rgxnot_func [| expr builder e1; expr builder e2 |] "ncomp" builder
      | SRgxeq (e1, e2) -> L.build_call rgxeq_func [| expr builder e1; expr builder e2 |] "equals" builder
      | SRgxneq (e1, e2) -> L.build_call rgxneq_func [| expr builder e1; expr builder e2 |] "nequals" builder
      | SCall ("rgx_to_string", [e]) -> L.build_call rgx_to_str_func [| expr builder e |] "rgx_to_string" builder
      | SCall ("int_to_string", [e]) -> L.build_call int_to_string_func [| expr builder e |] "int_to_string" builder
      | SCall ("string_to_int", [e]) -> L.build_call string_to_int_func [| expr builder e |] "string_to_int" builder
      | SCall ("bool_to_string", [e]) -> L.build_call bool_to_string_func [| expr builder e |] "bool_to_string" builder
      | SCall ("print", [e]) ->
    		L.build_call printf_func [| string_format_str builder; (expr builder e) |] "printf" builder
      | SCall ("nprint", [e]) ->
        L.build_call printf_func [| nstring_format_str builder; (expr builder e) |] "printf" builder
      | SCall ("length", [e]) -> 
        L.build_call length_func [| expr builder e |] "length" builder
      | SCall ("delete", [e1; e2]) -> 
        L.build_call delete_func [| expr builder e1 ; expr builder e2 |] "removeNode" builder
      | SCall ("insert", [e1; e2; e3]) -> 
        L.build_call insert_func [| expr builder e1 ; expr builder e2 ; cast_unsigned builder e3|] "insertElement" builder
      | SCall ("contains", [e1; e2]) ->
        L.build_call contains_func [| expr builder e1 ; cast_unsigned builder e2 ; choose_compar builder e2 |] "contains" builder
      | SCall ("index_of", [e1 ; e2]) ->
          L.build_call indexof_func [| expr builder e1 ; cast_unsigned builder e2 ; choose_compar builder e2 |] "findIndexOfNode" builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (expr builder) (List.rev args)) in
	      let result = (match fdecl.sret_type with 
                        A.Void -> ""
                      | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list llargs) result builder
      | SAccess (a) -> 
         let (loop_func, _) = StringMap.find "loop" function_decls in
         L.build_call access_func [| L.param loop_func 0; expr builder a|] "access" builder

      | SNumFields ->
          let (loop_func, _) = StringMap.find "loop" function_decls in
          L.build_call numfields_func [| L.param loop_func 0 |] "numfields" builder
      | SIncrement(e) -> let e2 = (A.Int, SAssign(e, (A.Int, SBinop(e, A.Add, (A.Int, SLiteral(1)))))) in expr builder e2
      | SDecrement(e) -> let e2 = (A.Int, SAssign(e, (A.Int, SBinop(e, A.Sub, (A.Int, SLiteral(1)))))) in expr builder e2
      | SPluseq(e1, e2) -> let e = (A.Int, SAssign(e1, (A.Int, SBinop(e1, A.Add, e2)))) in expr builder e
      | SMinuseq(e1, e2) -> let e = (A.Int, SAssign(e1, (A.Int, SBinop(e1, A.Sub, e2)))) in expr builder e
      | SRgxLiteral s -> let l = L.define_global "" (L.const_stringz context s) the_module in 
         L.const_bitcast (L.const_gep l [|L.const_int i32_t 0|]) str_t
      | _ -> raise (Failure "Cannot find expression") 
                and

    (* array gen functions *)
    find_arr_type ty = 
      let ast_typ = arr_elem_type ty in 
      L.size_of (ltype_of_typ ast_typ)

    and get_depth ty = 
      let depth_int = arr_depth ty in L.const_int i32_t depth_int

    and array_gen builder ty arr =
      let arr_type = find_arr_type ty in (* type of 1 depth down *)
      let depth = get_depth ty in

      let lst =
        L.build_call initlist_func [| arr_type; depth |] "initList" builder
      in 

      let add_front e = 
        let red_expr = expr builder e in
        let ty = L.type_of red_expr in
        let data = match  L.classify_type ty with
            L.TypeKind.Pointer -> L.build_pointercast red_expr i64_t "addFrontCast" builder
          | L.TypeKind.Integer -> L.build_zext red_expr i64_t "addFrontCast" builder
          | _ -> raise (Failure "Cannot find type of array")
        in

        ignore(L.build_call addfront_func [| lst; data |] "addFront" builder)
    in
    List.iter add_front arr; ignore(L.build_call reverse_func [| lst |] "reverseList" builder); lst

    and cast_unsigned builder e3 =
      let red_expr = expr builder e3 in
      let ty = L.type_of red_expr in match (L.classify_type ty) with
          L.TypeKind.Pointer -> L.build_pointercast red_expr i64_t "castToUnsigned" builder
        | L.TypeKind.Integer -> L.build_sext_or_bitcast red_expr i64_t "castToUnsigned" builder
        | _ -> raise (Failure "Cannot cast data to i64")

    and choose_compar builder e2 =
      let (e2_ty, _) = e2 in
      let rec compar_from_typ ty = match ty with
          A.Int -> L.build_zext_or_bitcast compareint_func compare_p_t "compareCast" builder
        | A.Bool -> L.build_zext_or_bitcast comparebools_func compare_p_t "compareCast" builder
        | A.String -> L.build_zext_or_bitcast comparestr_func compare_p_t "compareCast" builder
        | A.Rgx -> L.build_zext_or_bitcast comparestr_func compare_p_t "compareCast" builder
        | A.ArrayType t -> compar_from_typ t
        | _ -> raise (Failure "Unable to find comparator for list")
      in compar_from_typ e2_ty

  in 


    let rec stmt builder = function
        SExpr ex -> ignore(expr builder ex); builder 
      | SBlock sl -> List.fold_left stmt builder sl
      | SReturn e -> ignore (match fdecl.sret_type with
            A.Void -> L.build_ret_void builder
          | _ -> L.build_ret (expr builder e) builder); builder
      | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = expr builder predicate in
  	    let merge_bb = L.append_block context "merge" the_function in
            let build_br_merge = L.build_br merge_bb in (* partial function *)

  	    let then_bb = L.append_block context "then" the_function in
  	    add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
  	    build_br_merge;

  	    let else_bb = L.append_block context "else" the_function in
  	    add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
  	    build_br_merge;

  	    ignore(L.build_cond_br bool_val then_bb else_bb builder);
  	    L.builder_at_end context merge_bb
      | SWhile (predicate, body) ->
	      let pred_bb = L.append_block context "while" the_function in
	      ignore(L.build_br pred_bb builder);

	      let body_bb = L.append_block context "while_body" the_function in
	      add_terminal (stmt (L.builder_at_end context body_bb) body)
              (L.build_br pred_bb);

	      let pred_builder = L.builder_at_end context pred_bb in
	      let bool_val = expr pred_builder predicate in

	      let merge_bb = L.append_block context "merge" the_function in
	      ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
	      L.builder_at_end context merge_bb
      | SFor (e1, e2, e3, body) -> stmt builder
	      ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
      | _ -> raise (Failure "Cannot pattern match statement")
    in

    let func_builder = stmt func_builder (SBlock fdecl.sbody) in

    (* return types for functions *)
    add_terminal func_builder (match fdecl.sret_type with
        A.Void -> L.build_ret_void
      | A.Int -> L.build_ret (L.const_int i32_t 0)
      | A.Bool -> L.build_ret (L.const_int i1_t 0)
      | A.String -> L.build_ret (L.const_pointer_null str_t)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))

  (*----- END OF build_func_body ----- *)
    
  in
    
  (* Call the things that happen in main *)
  build_config_block config_block;
  List.iter build_function_body (snd begin_block);
  build_function_body loop_block;
  build_function_body end_block;

	the_module
