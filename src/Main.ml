(* 	Main module of the ProtoTypeChecker. *)

(* CLI Arguments *)
let argExpr = ref ""
let argType = ref ""

(* CLI usage string *)
let usage = "usage: " ^ Sys.argv.(0) ^ " -t type-string -e expr-string"
 
(* List of CLI options with the corresponding actions to store the command line arguments values and a short inline help *)
let speclist = [
    ("-t", Arg.String (fun s -> argType := s), ": the type that has to be checked for the given expression");
    ("-e", Arg.String (fun s -> argExpr := s), ": the expression that has to be type-checked");
  ]

let run_expression e t =
	ProtoLangParser.reset_scanner();	(* Resetting the scanners *)
	ProtoTypeParser.reset_scanner();
	print_string "Running with Expression...\n";
	let expr = (ProtoLangParser.parse_expr e) in
		print_string ("Parsed Expression: " ^ (ProtoLangParser.expr_to_string expr) ^ "\n");
		let tpe = (ProtoTypeParser.parse_ground_type t) in
			print_string ("Parsed Type: " ^ (ProtoTypeParser.type_to_string tpe) ^ "\n");
			(ProtoTypeChecker.check_expression expr tpe []) 

let run_definition e t =
	ProtoLangParser.reset_scanner();	(* Resetting the scanners *)
	ProtoTypeParser.reset_scanner();
	print_string "Running with Definition...\n";
	let expr = (ProtoLangParser.parse_def e) in
		print_string ("Parsed Definition: " ^ (ProtoLangParser.def_to_string expr) ^ "\n");
		let tpe = (ProtoTypeParser.parse_type_def t) in
			print_string ("Parsed Type: " ^ (ProtoTypeParser.typedef_to_string tpe) ^ "\n");
			(ProtoTypeChecker.check_definition expr tpe) 


(* Main function *)
let () =
  	
  	(* Reads the arguments *)
	Arg.parse speclist (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) usage;
 	
 	(* Checking the consistency of the argument *)
 	if (compare !argExpr "") == 0 then 
 		print_string "Bad argument: You must specify the Proto Expression that has to be type-checked\n"
 	else 
 		if (compare !argType "") == 0 then 
			print_string "Bad argument: You must specify a Type for the given Proto Expression\n"
		else
			try
				if run_definition !argExpr !argType
				then 
					print_string "The given type is correct for the given function definition!\n"
				 else
				 	print_string "The given type is not correct for the given expression!\n"
			with
				_ -> try 
						if run_expression !argExpr !argType
						then 
						 	print_string "The given type is correct for the given expression!\n"
					 	else
					 	 	print_string "The given type is not correct for the given expression!\n"
					with
					 	e -> print_string("Raised Exception: " ^ (Printexc.to_string e) ^ "\n");
					 		 (* print_string((Printexc.get_backtrace()) ^ "\n"); *)
					 	     print_string "The given type is not correct for the given expression!\n"



