(* TypeChecking Exception *)
exception ProtoTypeCheckException of string

let path = ref "./proto/"

(* The list of the already checked function definitions *)
let checked_functions = ref []

(* Checks if a function has already been checked *)
let is_checked n = List.exists (fun x -> ((compare n x) == 0)) (!checked_functions)

(* Loads the given function's type definition from a file .prototp *)
let load_fun_type f =
	let in_channel = (open_in (!path ^ f ^ ".prototp")) in
		let t = ref "" in
			try
	  			while true do
	    			let line = input_line in_channel in
	    				t := (!t ^ line);
	  			done;
	  			!t
			with 
				End_of_file -> close_in in_channel; !t

(* Loads the given function's definition from a file .proto *)
let load_fun_def f =
	let in_channel = (open_in (!path ^ f ^ ".proto")) in
		let d = ref "" in
		try
  			while true do
    			let line = input_line in_channel in
    				d := (!d ^ line);
  			done;
  			!d
		with 
			End_of_file -> close_in in_channel; !d

(* Resets the scanners *)
let reset_scanner () = 
	begin
	    ProtoLangParser.reset_scanner();	
		ProtoTypeParser.reset_scanner();
	end

(* Type-checks a constant *)
let check_constant c tpe =

	match c with 
		
		  ProtoLangParser.BoolC(b) 					(* Boolean Constant *)
			-> 	if (ProtoTypeParser.equal_types tpe (ProtoTypeParser.Local(ProtoTypeParser.Bool))) 	(* Checking that the type is Boolean *)
				then 
					true
				else 
					raise (ProtoTypeCheckException("The checked type for the boolean constant " ^ ProtoLangParser.const_to_string(c) ^ " is not the expected Bool type"))

		| ProtoLangParser.NumC(n)
			-> if (ProtoTypeParser.equal_types tpe (ProtoTypeParser.Local(ProtoTypeParser.Num)))   (* Checking that the type Num *)
				then 
					true
				else raise (ProtoTypeCheckException("The checked type for the numeric constant " ^ ProtoLangParser.const_to_string(c) ^ " is not the expected Num type"))

(* Type-checks a variable *)
let check_var v tpe var_list =
	try
		let list_tpe = snd(List.find(fun x -> let vl = fst(x) in ((compare v vl) == 0)) (var_list))
		in
			if (ProtoTypeParser.equal_types list_tpe tpe) then 
				true
			else
				raise (ProtoTypeCheckException("The variable " ^ ProtoLangParser.var_to_string(v) ^ " has not the expected type"))

	with 
		Not_found -> raise (ProtoTypeCheckException("The variable " ^ ProtoLangParser.var_to_string(v) ^ " is not defined"))

(* Type-checks an expression *)
let rec check_expression expr tpe var_list = 
	
	match expr with
		
		  ProtoLangParser.Const(c) ->					 	(* Constant *)
			(check_constant c tpe)
		
		| ProtoLangParser.Cond(e1, e2, e3) ->				(* Conditional Expression *)
			(check_expression e1 (ProtoTypeParser.Local(ProtoTypeParser.Bool)) var_list) 	    (* The if condition's type must be boolean *)
				&& (check_expression e2 tpe var_list) && (check_expression e3 tpe var_list)	  	(* The then and else branches must have the same type *)

		| ProtoLangParser.Varexp(v) ->						(* Variable expression *)
			(check_var v tpe var_list)					    (* Checks the type of the var according to the list of defined variables *)					

		| ProtoLangParser.Rep(v, e1, e2) ->							(* Rep expression *)
			let et = ProtoTypeParser.Local(ProtoTypeParser.Num)		(* The expected type for e1 is Num *)
			in
				(check_expression e1 et var_list) && (check_expression e2 tpe (var_list @ [(v, et)]))

		| ProtoLangParser.Appop(o, args) -> 
			let op_tpe = 
				begin
					reset_scanner(); 
					ProtoTypeParser.parse_arrow_type(ProtoOperators.get_type(match o with ProtoLangParser.Op(s) -> s))
				end
			in
				begin
					match op_tpe with
						ProtoTypeParser.FunType(argst, ot) ->
							(List.for_all2 (fun e -> fun t -> (check_expression e t var_list)) args argst) 	(* The arguments must have the expected type *)
								&& (ProtoTypeParser.equal_types tpe ot)					(* The codomain's type must be the exptected one *)
						| _	-> 
							raise (ProtoTypeCheckException("Invalid Operator Type"))
				end									

		| ProtoLangParser.Appfun(v, args) ->
			let fun_name = match v with ProtoLangParser.Var(s) -> s 					(* The function name *)
			in
				let fun_type = 															(* The function type *)
					begin
						reset_scanner(); 														
						match ProtoTypeParser.parse_type_def(load_fun_type(fun_name)) with 
							ProtoTypeParser.TypeDef(s, t) -> 
								if ((compare s fun_name) == 0) then
									t
								else 
									raise (ProtoTypeCheckException ("The file " ^ !path ^ fun_name ^ ".prototp does not contain a type definition for the function " ^ fun_name))
					end
				in 
					let fun_def = 
						begin
							reset_scanner();
							ProtoLangParser.parse_def(load_fun_def(fun_name))		(* The function definition *)
						end
					in
						begin
							match fun_type with
								  ProtoTypeParser.FunType(argst, ot) ->
								  	if (List.for_all2 (fun e -> fun t -> (check_expression e t var_list)) args argst) 	  (* The arguments must have the expected type *)
										&& (ProtoTypeParser.equal_types tpe ot)								          	  (* The codomain's type must be the exptected one *)
									then
										if is_checked fun_name 
											then true
										else
											begin
												(* print_string ("Type checking the \"" ^ fun_name ^ "\" function...\n"); *)
												let ris = (check_definition fun_def (ProtoTypeParser.TypeDef(fun_name, fun_type))) (* The function itself must be well-typed *)
												in
													checked_functions := !checked_functions @ [fun_name];
													ris
											end
									else
										false
								| _	-> 
									raise (ProtoTypeCheckException("Invalid Function Type"))
						end
						
(* Type-checks a function definition *)
and check_definition def tpe =
	
	match def with 
		ProtoLangParser.Def(v, args, exp) ->
			let fun_name = match v with ProtoLangParser.Var(s) -> s 
			in
				match tpe with 
					ProtoTypeParser.TypeDef(s, t) -> 
						if ((compare s fun_name) == 0) then
							match t with
								ProtoTypeParser.FunType(argst, ot) ->
									let var_list = (List.map2 (fun v -> fun t -> (v, t)) args argst)		(* The list of function arguments *)
									in 
										(check_expression exp ot var_list)	
								| _ -> raise (ProtoTypeCheckException ("Invalid type declaration for the function " ^ fun_name))			
						else 
							raise (ProtoTypeCheckException ("The given type defintion does not contain a type declaration for the function " ^ fun_name))

        





