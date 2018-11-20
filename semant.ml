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
                                                 (Void, "length", []); 
						 (Void, "print", [(String, "a")]);
                                                 (Void, "contains", []);
                                                 (Int, "index_of", []);
                                                 (Void, "insert", []);
                                                 (Void, "delete", [])]
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
  
  let (loop_locals,_) = loop_list in check_binds "local" loop_locals;
  
  let (end_locals,_) = end_list in check_binds "local" end_locals;

  let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;
    check_binds "local" func.locals;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if (lvaluet = rvaluet) then lvaluet else raise (Failure err)
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
      | ArrayLit(l) -> if List.length l > 0 then 
              let typ = expr(List.nth l 0) in
              let (arraytype, _) = typ in
              let check_array e =
                      let (et, e') = expr e in (et, e')
                      in let l' = List.map check_array l 
                      in (ArrayType(arraytype), SArrayLit(l'))
              else (Void, SArrayLit([])) (* try to find type from what is arund it?*)
      | NumFields -> (Int, SNumFields)
      | Assign(NumFields, e) -> raise (Failure ("illegal assignment of NF"))
      | Assign(e1, e2) as ex ->
          let (lt, e1') = expr e1 
          and (rt, e2') = expr e2 in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign((lt, e1'), (rt, e2'))) 
      | Unop(op, e) as ex -> 
          let (t, e') = expr e in
          let ty = match op with
            Neg | Increment | Decrement when t = Int -> Int
          | Access when t = Int -> String 
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
                     when same && (t1 = Int || t1 = String) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
	      Failure ("illegal binary operator "  ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Call("length", args) as length -> 
          if List.length args != 1 then raise (Failure("expecting one argument for length"))
          else let (et, e') = expr (List.nth args 0) in
          if (et = String || et = Bool || et = Void || et = Rgx || et = Int) then 
                  raise (Failure("illegal argument found " ^ 
                  string_of_typ et ^ " arraytype expected in " ^ string_of_expr (List.nth args 0)))
          else (Int, SCall("length", [(et, e')])) 
     | Call("contains", args) as contains -> 
          if List.length args != 2 then raise (Failure("expecting two arguments for contains"))
	  else let (t1, e1') = expr (List.nth args 0)
            and (t2, e2') = expr (List.nth args 1) in
            if (t1 = String || t1 = Bool || t1 = Void || t1 = Rgx || t1 = Int) 
               then raise (Failure("illegal argument found " ^ 
               string_of_typ t1 ^ " arraytype expected"))
            else let array_string = string_of_typ t1 in
            let n = String.length array_string in
            let array_type = String.sub array_string 0 (n-2) in 
            if (array_type = string_of_typ(t2) && t2 != Void) 
            then (Bool, SCall("contains", [(t1, e1');(t2, e2')]))
            else raise(Failure("cannot perform contains on " ^ array_string ^ " and " ^ string_of_typ(t2))) 
     | Call("index_of", args) as index_of -> 
          if List.length args != 2 then raise (Failure("expecting two arguments for index_of"))
	  else let (t1, e1') = expr (List.nth args 0)
            and (t2, e2') = expr (List.nth args 1) in
            if (t1 = String || t1 = Bool || t1 = Void || t1 = Rgx || t1 = Int) 
               then raise (Failure("illegal argument found " ^ 
               string_of_typ t1 ^ " arraytype expected"))
            else let array_string = string_of_typ t1 in
            let n = String.length array_string in
            let array_type = String.sub array_string 0 (n-2) in 
            if (array_type = string_of_typ(t2) && t2 != Void) 
            then (Int, SCall("index_of", [(t1, e1');(t2, e2')]))
            else raise(Failure("cannot perform index_of on " ^ array_string ^ " and " ^ string_of_typ(t2))) 
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
      | EnhancedFor(s1, s2, st) ->
          let s2_type = type_of_identifier s2 in
          let s2_type_string = string_of_typ s2_type in
          if (s2_type = Bool || s2_type = Rgx || s2_type = String || s2_type = Int || s2_type = Void) then
                  raise (Failure("cannot iterate over type " ^ s2_type_string))
          else let n = String.length s2_type_string in
          let array_type = String.sub s2_type_string 0 (n-2) in
          if (array_type = string_of_typ (type_of_identifier s1)) then SEnhancedFor(s1, s2, check_stmt st) 
          else raise(Failure("mismatch in " ^ string_of_typ (type_of_identifier s1) ^ " and " ^ s2_type_string))
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
    
  let stmt block_list =
   
          let (locals,_) = block_list in 
    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
  let (_, stmt) = block_list in 
    let check_assign lvaluet rvaluet err =

       if (lvaluet = rvaluet) then lvaluet else raise (Failure err)
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
      | ArrayLit(l) -> if List.length l > 0 then 
              let typ = expr(List.nth l 0) in
              let (arraytype, _) = typ in
              let check_array e =
                      let (et, e') = expr e in (et, e')
                      in let l' = List.map check_array l 
                      in (ArrayType(arraytype), SArrayLit(l'))
              else (Void, SArrayLit([]))
      | ArrayDeref (e1, e2) as e ->
          let (arr, e1') = expr e1
          and (num, e2') = expr e2 in
          if num = Int then 
                  if (arr = Bool || arr = String || arr = Rgx || arr = Void || arr = Int) then
                         raise (Failure ("array deref should be called on an array, not " ^ string_of_typ arr))
                  else let type_arr = string_of_typ arr in
                  let n = String.length type_arr in
                  let typ = String.sub type_arr 0 (n-2) in
                  let t = match typ with
                  "bool" -> Bool
                 |"string" -> String
                 |"rgx" -> Rgx
                 |"int" -> Int
                 | _ -> raise (Failure("array type is wrong")) in (t, SArrayDeref((arr, e1'), (num, e2')))
          else raise (Failure ("Int expression expected, " ^ string_of_typ num))
      | NumFields -> (Int, SNumFields)
      | Assign(NumFields, e) -> raise (Failure ("illegal assignment of NF"))
      | Assign(e1, e2) as ex -> 
          let (lt, e1') = expr e1 
          and (rt, e2') = expr e2 in 
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign((lt, e1'), (rt, e2'))) 
      | Unop(op, e) as ex -> 
          let (t, e') = expr e in
          let ty = match op with
            Neg | Increment | Decrement when t = Int -> Int
          | Access when t = Int -> String 
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
                     when same && (t1 = Int || t1 = String) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
	      Failure ("illegal binary operator "  ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Call("length", args) as length -> 
          if List.length args != 1 then raise (Failure("expecting one argument for length"))
          else let (et, e') = expr (List.nth args 0) in
          if (et = String || et = Bool || et = Void || et = Rgx || et = Int) then 
                  raise (Failure("illegal argument found " ^ 
                  string_of_typ et ^ " arraytype expected in " ^ string_of_expr (List.nth args 0)))
          else (Int, SCall("length", [(et, e')])) 
      | Call("contains", args) as contains -> 
          if List.length args != 2 then raise (Failure("expecting two arguments for contains"))
	  else let (t1, e1') = expr (List.nth args 0)
            and (t2, e2') = expr (List.nth args 1) in
            if (t1 = String || t1 = Bool || t1 = Void || t1 = Rgx || t1 = Int) 
               then raise (Failure("illegal argument found " ^ 
               string_of_typ t1 ^ " arraytype expected"))
            else let array_string = string_of_typ t1 in
            let n = String.length array_string in
            let array_type = String.sub array_string 0 (n-2) in 
            if (array_type = string_of_typ(t2) && t2 != Void) 
            then (Bool, SCall("contains", [(t1, e1');(t2, e2')]))
            else raise(Failure("cannot perform contains on " ^ array_string ^ " and " ^ string_of_typ(t2))) 
      | Call("index_of", args) as index_of -> 
          if List.length args != 2 then raise (Failure("expecting two arguments for index_of"))
	  else let (t1, e1') = expr (List.nth args 0)
            and (t2, e2') = expr (List.nth args 1) in
            if (t1 = String || t1 = Bool || t1 = Void || t1 = Rgx || t1 = Int) 
               then raise (Failure("illegal argument found " ^ 
               string_of_typ t1 ^ " arraytype expected"))
            else let array_string = string_of_typ t1 in
            let n = String.length array_string in
            let array_type = String.sub array_string 0 (n-2) in 
            if (array_type = string_of_typ(t2) && t2 != Void) 
            then (Int, SCall("index_of", [(t1, e1');(t2, e2')]))
            else raise(Failure("cannot perform index_of on " ^ array_string ^ " and " ^ string_of_typ(t2))) 
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
      | EnhancedFor(s1, s2, st) ->
          let s2_type = type_of_identifier s2 in
          let s2_type_string = string_of_typ s2_type in
          if (s2_type = Bool || s2_type = Rgx || s2_type = String || s2_type = Int || s2_type = Void) then
                  raise (Failure("cannot iterate over type " ^ s2_type_string))
          else let n = String.length s2_type_string in
          let array_type = String.sub s2_type_string 0 (n-2) in
          if (array_type = string_of_typ (type_of_identifier s1)) then SEnhancedFor(s1, s2, check_stmt st) 
          else raise(Failure("mismatch in " ^ string_of_typ (type_of_identifier s1) ^ " and " ^ s2_type_string))
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
  (loop_locals, stmt loop_list), 
  (end_locals, stmt end_list), config_list)
