module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (begin_block, loop_block, end_block, config_block) input_file =

  	let context    = L.global_context () in

	(* Create the LLVM compilation module into which
	 we will generate code *)
	let the_module = L.create_module context "Bawk" in

	(* Get types from the context *)
	let i32_t      = L.i32_type    context
	and i8_t       = L.i8_type     context
	and i1_t       = L.i1_type     context
	and str_t 	   = L.pointer_type ( L.i8_type context ) 
	and void_t     = L.void_type   context in

	(* Return the LLVM type for a MicroC type *)
	let ltype_of_typ = function
	  A.Int   -> i32_t
	| A.Bool  -> i1_t
	| A.Void  -> void_t
	| A.String -> str_t

	in

	let printf_t : L.lltype = 
    	L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  	let printf_func : L.llvalue = 
    	L.declare_function "printf" printf_t the_module in

    let ftype = L.function_type void_t [||] in

    let loop_func = L.define_function "loop" ftype the_module in

    let end_func = L.define_function "end" ftype the_module in

    (* Build loop block *)
    let build_loop_block loop_block =
    	let loop_builder = L.builder_at_end context (L.entry_block loop_func) in
    	let string_format_str builder = L.build_global_stringptr "%s\n" "fmt" loop_builder in
    	(* TODO: add all other patterns to expr and stmt *)
    	let rec expr builder = function
    		A.StringLiteral s -> let l = L.define_global "" (L.const_stringz context s) the_module in
				L.const_bitcast (L.const_gep l [|L.const_int i32_t 0|]) str_t
			| A.BoolLit b  -> L.const_int i1_t (if b then 1 else 0)
    		| A.Call ("print", [e]) ->
    			L.build_call printf_func [| string_format_str loop_builder; (expr loop_builder e)|] "printf" builder

    	in 

    	let add_terminal builder instr =
	     	match L.block_terminator (L.insertion_block loop_builder) with
			Some _ -> ()
	    	| None -> ignore (instr loop_builder) in


    	let rec stmt builder = function
    		A.Expr ex -> ignore(expr loop_builder ex); loop_builder
    		| A.Block sl -> List.fold_left stmt loop_builder sl

    	in

    	let loop_builder = stmt loop_builder (Block (snd loop_block)) in

    	add_terminal loop_builder L.build_ret_void


    in build_loop_block loop_block;

    (* Build end block *)
    let build_end_block end_block =
    	let end_builder = L.builder_at_end context (L.entry_block end_func) in
    	let string_format_str builder = L.build_global_stringptr "%s\n" "fmt" end_builder in

    	(* TODO: add all other patterns to expr and stmt *)
    	let rec expr builder = function
    		A.StringLiteral s -> let l = L.define_global "" (L.const_stringz context s) the_module in
				L.const_bitcast (L.const_gep l [|L.const_int i32_t 0|]) str_t
			| A.BoolLit b  -> L.const_int i1_t (if b then 1 else 0)
    		| A.Call ("print", [e]) ->
    			L.build_call printf_func [| string_format_str end_builder; (expr end_builder e)|] "printf" builder

    	in 

    	let add_terminal builder instr =
	     	match L.block_terminator (L.insertion_block end_builder) with
			Some _ -> ()
	    	| None -> ignore (instr end_builder) in


    	let rec stmt builder = function
    		A.Expr ex -> ignore(expr end_builder ex); end_builder
    		| A.Block sl -> List.fold_left stmt end_builder sl

    	in

    	let end_builder = stmt end_builder (Block (snd end_block)) in 

    	add_terminal end_builder L.build_ret_void

    in build_end_block end_block;

    the_module



