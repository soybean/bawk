open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (begin_list, loop_list, end_list, config_list) =

  (* Verify a list of bindings has no void types or duplicate names *)
  let check_binds (kind : string) (binds : bind list) =
    List.iter (function
	(Void, b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
	  raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (**** Check global variables ****)

  let (globals, _) = begin_list in check_binds "global" globals;


  (**** Check functions ****)
  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
  let add_bind map (ftyp, name, flist) = StringMap.add name {
      ret_type = ftyp;
      fname = name; 
      formals = flist;
      locals = []; body = [] } map
    in List.fold_left add_bind StringMap.empty [ (Int, "string_to_int", [(String, "a")]);
			                         (String, "int_to_string", [(Int, "a")]);
						 (String, "bool_to_string", [(Bool, "a")]);
						 (String, "rgx_to_string", [(Rgx, "a")]);
                                                 (Int, "length", [(ArrayType(Int), "a")]);
						 (Void, "print", [(String, "a")]);
						 (Void, "println", [(String, "a")]);
                                                 (Bool, "contains", [(Int, "a");(ArrayType(Int), "b")]);
						 (Bool, "contains", [(String, "a");(ArrayType(String), "b")]);
						 (Bool, "contains", [(Rgx, "a");(ArrayType(Rgx), "b")]);
						 (Bool, "contains", [(Bool, "a");(ArrayType(Bool), "b")]);
                                                 (Int, "index_of", [(ArrayType(Int), "a");(Int, "b")]);
						 (Int, "index_of", [(ArrayType(String), "a");(String, "b")]);
						 (Int, "index_of", [(ArrayType(Rgx), "a");(Rgx, "b")]);
						 (Int, "index_of", [(ArrayType(Bool), "a");(Bool, "b")])] (* need to make generic *)
  in 
	
  
   (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in
   (* Collect all function names into one symbol table *)
  let (_, functions) = begin_list in let function_decls = List.fold_left add_func built_in_decls functions

  in
  
  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;
    check_binds "local" func.locals;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in   

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m) (* this might need to be changed later*)
	                StringMap.empty (globals @ func.formals @ func.locals )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in
    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
        Literal l -> (Int, SLiteral l)
      | StringLiteral l -> (String, SStringLiteral l)
      | BoolLit l  -> (Bool, SBoolLit l)
      | Rgx l  -> (Rgx, SRgx l)
      | RgxLiteral l -> (Rgx, SRgxLiteral l)
      | Noexpr     -> (Void, SNoexpr)
      | Id s       -> (type_of_identifier s, SId s)
      | ArrayLit(l) -> expr (List.nth l 0)
      | NumFields -> (Int, SNumFields)
      | Assign(e1, e2) as ex -> 
          let (lt, e1') = expr e1 
          and (rt, e2') = expr e2 in 
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign((lt, e1'), (rt, e2'))) 
      | Unop(op, e) as ex -> 
          let (t, e') = expr e in
          let ty = match op with
            Neg | Increment | Decrement | Access when t = Int -> Int 
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex)) 
          in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e -> 
          let (t1, e1') = expr e1 
          and (t2, e2') = expr e2 in
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div | Pluseq | Minuseq when same && t1 = Int   -> Int
	  | Strcat when same && t1 = String -> String
	  | Rgxeq | Rgxneq when same && t1 = Rgx -> Bool
	  | Rgxcomp | Rgxnot when ((t1 = Rgx) && (t2 = String)) || ((t1 = String) && (t2 = Rgx)) -> Bool 
          | Equal | Neq            when same               -> Bool
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
	      Failure ("illegal binary operator "  ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e = 
            let (et, e') = expr e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e 
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.ret_type, SCall(fname, args'))
    in

    let check_bool_expr e = 
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e 
      in if t' != Bool then raise (Failure err) else (t', e') 
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, st) ->
	  SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
      | EnhancedFor(s1, st) ->
          SEnhancedFor(s1, check_stmt st) (*unsure about this one? should it be expr*)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Return e -> let (t, e') = expr e in
        if t = func.ret_type then SReturn (t, e') 
        else raise (
	  Failure ("return gives " ^ string_of_typ t ^ " expected " ^
		   string_of_typ func.ret_type ^ " in " ^ string_of_expr e))
	    
	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> 
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []
          in SBlock(check_stmt_list sl)

    in (* body of check_function *)
    { sret_type = func.ret_type;
      sfname = func.fname;
      sformals = func.formals;
      slocals  = func.locals;
      sbody = match check_stmt (Block func.body) with
	SBlock(sl) -> sl
      | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
  in 
    
   let stmt list =
   
  let (locals,_) = list in check_binds "local" locals;
    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
  let (_, stmt) = list in 
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in   

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m) (* this might need to be changed later*)
	                StringMap.empty (globals @ locals)
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in
    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
        Literal l -> (Int, SLiteral l)
      | StringLiteral l -> (String, SStringLiteral l)
      | BoolLit l  -> (Bool, SBoolLit l)
      | Rgx l  -> (Rgx, SRgx l)
      | RgxLiteral l -> (Rgx, SRgxLiteral l)
      | Noexpr     -> (Void, SNoexpr)
      | Id s       -> (type_of_identifier s, SId s)
      | ArrayLit(l) -> expr (List.nth l 0)
      | NumFields -> (Int, SNumFields)
      | Assign(e1, e2) as ex -> 
          let (lt, e1') = expr e1 
          and (rt, e2') = expr e2 in 
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign((lt, e1'), (rt, e2'))) 
      | Unop(op, e) as ex -> 
          let (t, e') = expr e in
          let ty = match op with
            Neg | Increment | Decrement | Access when t = Int -> Int 
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex)) 
          in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e -> 
          let (t1, e1') = expr e1 
          and (t2, e2') = expr e2 in
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div | Pluseq | Minuseq when same && t1 = Int   -> Int
	  | Strcat when same && t1 = String -> String
	  | Rgxeq | Rgxneq when same && t1 = Rgx -> Bool
	  | Rgxcomp | Rgxnot when ((t1 = Rgx) && (t2 = String)) || ((t1 = String) && (t2 = Rgx)) -> Bool 
          | Equal | Neq            when same               -> Bool
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
	      Failure ("illegal binary operator "  ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e = 
            let (et, e') = expr e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e 
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.ret_type, SCall(fname, args'))
    in

    let check_bool_expr e = 
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e 
      in if t' != Bool then raise (Failure err) else (t', e') 
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, st) ->
	  SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
      | EnhancedFor(s1, st) ->
          SEnhancedFor(s1, check_stmt st) (*unsure about this one? should it be expr*)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Return e -> raise (
	  Failure ("return must be in a function"))
	    
	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> 
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []
          in SBlock(check_stmt_list sl)

    in match check_stmt (Block stmt) with
	SBlock(sl) -> sl
      | _ -> raise (Failure ("internal error: block didn't become a block?"))
      
  in ((globals, List.map check_function functions), 
  (loop_locals, let (_, loop_stmts) = loop_list in stmt loop_stmts), 
  (end_locals, let (_, end_stmts) = end_list in List.map check_function end_stmts), config_list)
