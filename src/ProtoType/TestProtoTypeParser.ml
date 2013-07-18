(* Tests the ProtoType scanning and parsing functionalities *)

let test_def s = 
	ProtoTypeParser.reset_scanner();
	print_string (ProtoTypeParser.typedef_to_string(ProtoTypeParser.parse_type_def s));
	print_string "\n";
and test_arrow_type s =
	ProtoTypeParser.reset_scanner();
	print_string (ProtoTypeParser.type_to_string(ProtoTypeParser.parse_arrow_type s));
	print_string "\n";
in

	(* Prints the result of the parsing of the type signature of the handled Proto operators *)
	print_string "Test: Parsing the list of handled Proto operators defined in the ProtoOperators module\n";
	List.iter (fun x -> test_def(snd(x))) !ProtoOperators.operators_list;
	print_string "\n\t\t[SUCCESS]\n\n";

	(* Prints the result of the parsing of the arrow type from the list of the handled Proto operators *)
	print_string "Test: Parsing the list of the arrow type parse from the type definitions of the handled Proto operators\n";
	List.iter (fun x -> print_string (fst(x) ^ ": "); test_arrow_type(snd(x))) !ProtoOperators.operators_list;
	print_string "\n\t\t[SUCCESS]\n\n";

	(* Prints the result of the parsing of the ground types *)
	print_string "Test: Parsing the ground types\n";
	ProtoTypeParser.reset_scanner();
	print_string ("Num: " ^ ProtoTypeParser.type_to_string(ProtoTypeParser.parse_ground_type "N") ^ "\n");
	ProtoTypeParser.reset_scanner();
	print_string ("Bool: " ^ ProtoTypeParser.type_to_string(ProtoTypeParser.parse_ground_type "B") ^ "\n");
	ProtoTypeParser.reset_scanner();
	print_string ("Field: " ^ ProtoTypeParser.type_to_string(ProtoTypeParser.parse_ground_type "F") ^ "\n");
	print_string "\n\t\t[SUCCESS]\n\n";

