(* Parsing Exception *)
exception ProtoParseException of string

(* Exception used to handle the termination of the parsing of inner expressions *)
exception ProtoEndExprException of string

(* Proto language's abstract syntax definition *)
type const  = BoolC of string 
            | NumC of int
type var    = Var of string
type op     = Op of string
type exp    = Const of const
            | Varexp of var
            | Appfun of var * (exp list)
            | Appop of op * (exp list)
            | Cond of exp * exp * exp
            | Rep of var * exp * exp
type def    = Def of var * (var list) * exp


(* Resets the LangScanner *)
let reset_scanner () = 
    begin
        ProtoLangScanner.i := 0
    end

(* The list of the functions defined by the user *)
let functions_list = ref []

(* The list of the defined variables names *)
let def_vars = ref []

(* Checks if the name n is defined in the operators list *)
let is_operator n = ProtoOperators.exists n

(* Checks if the name n is defined in the user's functions list *)
let is_user_function n = List.exists (fun x -> ((compare n x) == 0)) (!functions_list)

(* Checks if the name n is defined in the function function arguments list *)
let is_defined_var n = List.exists (fun x -> (match x with Var(s) -> ((compare n s) == 0))) (!def_vars)

(* Checks if the the name is valid or reserved/already used *)
let is_defined_name s = ((compare s "def") == 0) or            (* def is a language's special reserved key *)
                            (is_operator s) or (is_user_function s) or (is_defined_var s)

(* The currently scanned token *)
let token = ref ""

(****************************************************************************************************************)

(* Parse a top-level expressions *)
let rec parse_expr s =

    let is_name s = 
        if (ProtoLangScanner.is_letter(!token.[0]) || !token.[0] == '+' 
                          || !token.[0] == '-' || !token.[0] == '*' 
                          || !token.[0] == '/' || !token.[0] == '='
                          || !token.[0] == '<' || !token.[0] == '>') 
        then 
            true
        else
            false
    in

    (* Parse a list of expressions *)
    let parse_expr_list s =
        let expr_list = ref [] in    
            let end_loop = ref false in
                while not !end_loop do
                    try
                        expr_list := !expr_list @ [(parse_expr s)];
                    with ProtoEndExprException(s) -> end_loop := true;
                done;
                !expr_list        
    in

    (* Parse a variable *)
    let parse_var s =
        token := ProtoLangScanner.next s;   (* fetching the next token *)
        match !token with
              v when ProtoLangScanner.is_letter(!token.[0]) -> Var(!token)
            | _                                         -> raise (ProtoParseException ("I was expecting a name but I found " ^ !token))    

    in
    
    (* Inner expressions parser *)
    let parse_inner_exp s = 
        
        token := ProtoLangScanner.next s;   (* fetching the next token *)
        (* print_string ("parse_inner_exp: " ^ !token ^ "\n"); *)
        match !token with
            
              b when (!token.[0] == '#')    -> Const(BoolC(!token))                     (* Boolean Constant *)
            
            | n when ProtoLangScanner.is_digit(!token.[0])                              (* Numeric Constant *)      
                                            -> Const(NumC(int_of_string(!token)))                
            
            | c when ((compare !token "if") == 0)                                       (* if e1 e2 e3 *)
                                            ->  let e1 = (parse_expr s) in
                                                    let e2 = (parse_expr s) in
                                                        let e3 = (parse_expr s) in  
                                                            Cond(e1, e2, e3) 
            
            | r when ((compare !token "rep") == 0)                                      (* rep v e1 e2 *)                                                
                                            ->  let v = (parse_var s) in
                                                    let e1 = (parse_expr s) in 
                                                        def_vars := !def_vars @ [v];                (* adding the rep variable to the set of the defined variables *)
                                                        let e2 = (parse_expr s) in
                                                            Rep(v, e1, e2) 
            
            | o when is_name !token -> 
                                            if (is_operator !token) then                (* op e1 e2 .. *)
                                                let op = Op(!token) in
                                                    Appop(op, parse_expr_list s)
                                            else 
                                                let f = Var(!token) in                  (* f e1 e2 .. *)
                                                    Appfun(f, parse_expr_list s)           
            
            | _                            -> raise (ProtoParseException ("Parse error: " ^ !token))        
        
    in 
        token := ProtoLangScanner.next s;   (* fetching the next token *)
        (* print_string ("parse_expr: " ^ !token ^ "\n"); *)
        match !token with
              
              b when (!token.[0] == '#')   -> Const(BoolC(!token))                     (* Boolean Constant *)
            
            | n when ProtoLangScanner.is_digit(!token.[0])                                 (* Numeric Constant *)      
                                           -> Const(NumC(int_of_string(!token)))                
            
            | v when ProtoLangScanner.is_letter(!token.[0]) -> 
                                            if (is_defined_var !token) then           (* Variables in Expressions *) 
                                                Varexp(Var(!token))
                                            else 
                                                raise (ProtoParseException ("The variable " ^ !token ^ " is not defined"))                                   (* Variabili *)
            
            | po when (!token.[0] == '(')  -> 
                                            let e = (parse_inner_exp s) in            (* ( inner_expression ) *)
                                                if(!token.[0] == ')') then            (* if the token is ) it means that I was parsing an expr_list *)
                                                    e
                                                else                                  (* otherwise I still have to fetch the token *)
                                                    begin 
                                                        token := ProtoLangScanner.next s;
                                                        if(!token.[0] == ')') then    
                                                            e
                                                        else             
                                                            raise (ProtoParseException ("Parse error. I was expecting \")\" but I found \"" ^ !token ^ "\""))
                                                    end
            | pc when (!token.[0] == ')')  -> raise (ProtoEndExprException ("Parse error: " ^ !token))
            
            | _                            -> raise (ProtoParseException ("Illegal token: " ^ !token))


(* Pretty printing of the inner representation of constants *)
let const_to_string c = 
    match c with 
          BoolC(s) -> s
        | NumC(n)  -> string_of_int n
    (*    | _ -> raise Exception("The argument is not a constant")        *)

(* Pretty printing of the inner representation of variables *)
let var_to_string v =
    match v with 
        Var(s) -> s
    (*    | _ -> raise Exception("The argument is not a varible")         *)

(* Pretty printing of the inner representation of operators *)
let op_to_string o =
    match o with 
        Op(s) -> s
    (*        | _ -> raise Exception("The argument is not an operator")   *)

(* Pretty printing of the inner representation of expressions *)
let rec expr_to_string e = match e with
      
      Const c -> const_to_string c  
    
    | Varexp v -> var_to_string v
    
    | Appfun(v, es) ->
        let fun_name = (var_to_string v) in
            let expr_string = String.concat ", " (List.map (fun x -> (expr_to_string x)) es)
            in
                fun_name ^ "(" ^ expr_string ^ ")"
    
    | Appop(o, es) ->
        let fun_name = (op_to_string o) in
            let expr_string = String.concat ", " (List.map (fun x -> (expr_to_string x)) es)
            in
                fun_name ^ "(" ^ expr_string ^ ")"

    | Cond(e1, e2, e3) ->
        "if " ^  expr_to_string(e1) ^ " then " ^ expr_to_string(e2) ^ " else " ^ expr_to_string(e3)

    | Rep(v, e1, e2) ->
        "rep " ^ var_to_string(v) ^ " " ^  expr_to_string(e1) ^ " " ^  expr_to_string(e2)

 (*   | _ -> raise Exception("The argument is not an expression")  *) 



(****************************************************************************************************************)

(* Parse a function definition *)
let parse_def s = 

    (* Parse a list of functions paramethers *)
    let parse_var_list s =
        let varList = ref [] in 
            let end_loop = ref false in
                while not !end_loop do
                    if ProtoLangScanner.is_letter(!token.[0]) then
                        begin
                            varList := !varList @ [Var(!token)];
                            token := ProtoLangScanner.next s             (* fetching the next token *)
                        end
                    else 
                        if (!token.[0] == ')') then
                        begin
                            end_loop := true;
                        end
                        else
                            raise (ProtoParseException ("I was expecting a name but I found \"" ^ !token ^ "\""))
                done;
                !varList
    in
        token := ProtoLangScanner.next s;           (* fetching the next token *)
        if (!token.[0] == '(') then                 (* a definition starts with an open bracket *)
        begin
            token := ProtoLangScanner.next s;       (* fetching the next token *)
            if ((compare !token "def") == 0) then   (* we expect the def keyword *)
            begin
                token := ProtoLangScanner.next s;                    (* fetching the next token *)
                if ProtoLangScanner.is_letter(!token.[0]) then       (* parsing the function name *)
                    let fun_name = !token in        
                        begin
                            functions_list := !functions_list @ [fun_name];       (* adding the name to the list of the defined functions *)
                            token := ProtoLangScanner.next s;
                            if (!token.[0] == '(') then         (* the list of the function's argument starts with an open bracket *)
                            begin
                                token := ProtoLangScanner.next s;
                                let fargs = parse_var_list s in         (* parsing the args *)
                                    def_vars := fargs;
                                    let fun_def = Def(Var(fun_name), fargs, (parse_expr s)) in
                                        token := ProtoLangScanner.next s;
                                        if(!token.[0] != ')') then      (* if the input ends with ')' the definition is well formed *)
                                            raise (ProtoParseException ("Parse error. I was expecting \")\" but I found \"" ^ !token ^ "\""))
                                        else 
                                            fun_def
                            end
                            else
                                raise (ProtoParseException ("I was expecting a \"(\" but I found \"" ^ !token ^ "\""))
                        end
                else
                    raise (ProtoParseException ("I was expecting a name but I found \"" ^ !token ^ "\""))
            end
            else
                raise (ProtoParseException ("I was expecting the \"def\" keyword but I found \"" ^ !token ^ "\""))
        end
        else
            raise (ProtoParseException ("I was expecting a \"(\" but I found \"" ^ !token ^ "\""))


(* Pretty printing of the inner representation of function definitions *)
let rec def_to_string d = match d with
    Def(v, vars, e) ->
        let fun_name = (var_to_string v) in
            let args_string = String.concat ", " (List.map (fun x -> (var_to_string x)) vars)
                in
                fun_name ^ "(" ^ args_string ^ ") = " ^ expr_to_string(e) 
