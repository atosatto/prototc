print_string (ProtoLangParser.expr_to_string(ProtoLangParser.parse_expr "(min-hood 0)"));
print_string "\n";

ProtoLangParser.reset_scanner();

print_string (ProtoLangParser.expr_to_string(ProtoLangParser.parse_expr "(0)"));
print_string "\n";

ProtoLangParser.reset_scanner();

print_string (ProtoLangParser.expr_to_string(ProtoLangParser.parse_expr "0"));
print_string "\n";

ProtoLangParser.reset_scanner();

ProtoLangParser.def_vars := !ProtoLangParser.def_vars @ [ProtoLangParser.Var("k")];

print_string (ProtoLangParser.expr_to_string(ProtoLangParser.parse_expr "k"));
print_string "\n";

ProtoLangParser.reset_scanner();

print_string (ProtoLangParser.expr_to_string(ProtoLangParser.parse_expr "(if #t k 0)"));
print_string "\n";

ProtoLangParser.reset_scanner();

print_string (ProtoLangParser.expr_to_string(ProtoLangParser.parse_expr "(+ 0 1)"));
print_string "\n";

ProtoLangParser.reset_scanner();

print_string (ProtoLangParser.expr_to_string(ProtoLangParser.parse_expr "(k 1 2)"));
print_string "\n";

ProtoLangParser.reset_scanner();

print_string (ProtoLangParser.expr_to_string(ProtoLangParser.parse_expr "(+ 2 (+ 4 6))"));
print_string "\n";

ProtoLangParser.reset_scanner();

print_string (ProtoLangParser.def_to_string(ProtoLangParser.parse_def "(def f (x y z) 0)"));
print_string "\n";

ProtoLangParser.reset_scanner();

print_string (ProtoLangParser.def_to_string(ProtoLangParser.parse_def "(def g ( s ) s)"));
print_string "\n";

ProtoLangParser.reset_scanner();

print_string (ProtoLangParser.def_to_string(ProtoLangParser.parse_def "(def add (x y z)	(+ x (+ y z)) )"));
print_string "\n";