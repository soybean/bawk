module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (begin_block, loop_block, end_block, config_block) =
  	let context    = L.global_context () in

	(* Create the LLVM compilation module into which
	 we will generate code *)
	let the_module = L.create_module context "Bawk" in

	(* Get types from the context *)
	let i32_t      = L.i32_type    context
	and i8_t       = L.i8_type     context
	and i1_t       = L.i1_type     context
	and void_t     = L.void_type   context in

	(* Return the LLVM type for a MicroC type *)
	let ltype_of_typ = function
	  A.Int   -> i32_t
	| A.Bool  -> i1_t
	| A.Void  -> void_t

	in

	let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  	let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t the_module in

    let build_end_block end_block =
    	let builder = L.builder context in

    	let string_format_str builder = L.build_global_stringptr "%s\n" "fmt" builder in

    	let rec expr builder = function
    		A.Call ("print", [e]) ->
    			L.build_call printf_func [| string_format_str builder; (expr builder e)|] "printf" builder

    	in 

    	let add_terminal builder instr =
	     	match L.block_terminator (L.insertion_block builder) with
			Some _ -> ()
	    	| None -> ignore (instr builder) in

    	let rec stmt builder = function
    		A.Expr ex -> ignore(expr builder ex); builder
    		| A.Block sl -> List.fold_left stmt builder sl

    	in

    	add_terminal builder L.build_ret_void

    	(* let builder = stmt builder (snd end_block) *)

    in build_end_block end_block;


    the_module


