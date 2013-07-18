(* Proto type's abstract syntax definition *)
type local         = Bool 
                   | Num
type protoType     = Local of local
                   | Field
                   | FunType of (protoType list) * protoType
type protoTypeDef  = TypeDef of string * protoType

(* Definition of the parsing exception *)
exception ProtoTypeParsingException of string

(* The currently scanned token *)
let token = ref ""

(* Checks if two types are equals *)
let rec equal_types t1 t2 =
    match t1 with

          Local(Bool) -> 
            begin
                match t2 with Local(Bool) -> true | _ -> false
            end

        | Local(Num) -> 
            begin
                match t2 with Local(Num) -> true | _ -> false
            end

        | Field ->
            begin
                match t2 with Field -> true | _ -> false
            end

        | FunType(l1, t1) ->
            begin
                match t2 with
                      FunType(l2, t2) ->
                        let ltpe1 = l1 @ [t1] in       (* the list of type that has to be equals *)
                            let ltpe2 = l2 @ [t2] in 
                                List.for_all2 (fun x -> fun y -> (equal_types x y)) ltpe1 ltpe2        
                    | _               -> false
            end


(* Resets the TypeScanner *)
let reset_scanner () = 
    begin
        ProtoTypeScanner.i := 0
    end

(* Parses a ground type *)
let parse_ground_type s = 
    token := ProtoTypeScanner.next s;
    begin
        match token with
              b when ((compare !token "B") == 0) -> Local(Bool)
            | n when ((compare !token "N") == 0) -> Local(Num)
            | f when ((compare !token "F") == 0) -> Field
            | _                                  -> raise (ProtoTypeParsingException ("Parse error: I was expecting a type but I found " ^ !token))
    end

(* Parses a library function's type definition *)
let parse_type_def s =

    (* Parses a name *)
    let parse_name s =
        token := ProtoTypeScanner.next s;
        if (ProtoTypeScanner.is_letter(!token.[0]) || !token.[0] == '+' 
                              || !token.[0] == '-' || !token.[0] == '*' 
                              || !token.[0] == '/' || !token.[0] == '='
                              || !token.[0] == '<' || !token.[0] == '>') 
        then 
            !token
        else
            raise (ProtoTypeParsingException ("Parse error: I was expecting a name but I found " ^ !token))

    in 
        (* Parses the type arguments of a function *)
        let parse_args s =

            let rec parse_args_aux s args_list =            
                match token with 
                    | p when (!token.[0] == ')') -> args_list
                    | _                          -> (* let arg_name = parse_name s in *)
                                                    ((fun s -> ()) (parse_name s));       (* to avoid type error I read the arg name and return () *)
                    								token := ProtoTypeScanner.next s;
                                                    if ((compare !token "|") == 0) then
                                                        let arg_type = parse_ground_type s in
                                                        	token := ProtoTypeScanner.next s;
                                                            (parse_args_aux s (args_list @ [arg_type]))
                                                   
                                                    else
                                                        raise (ProtoTypeParsingException ("Parse error: I was expecting \'|\' but I found " ^ !token))      
        in (parse_args_aux s [])

    
    in 
        token := ProtoTypeScanner.next s;
        if (!token.[0] == '(') then                         (* the type declaration starts with an open bracket *)
        let fun_name = parse_name s in  
            token := ProtoTypeScanner.next s;
            match token with
                 
                 c when (!token.[0] == ',')  ->             (* Parsing the type of a function with arguments *)
                    let fun_args = parse_args s in
                    if (!token.[0] == ')') then 
                    begin
                    	token := ProtoTypeScanner.next s;
    					if ((compare !token "->") == 0) then
                            let fun_output = parse_ground_type s in 
                                TypeDef(fun_name, (FunType(fun_args, fun_output)))
                        else 
                            raise (ProtoTypeParsingException ("Parse error: I was expecting a \'->\' but I found " ^ !token))            
                    end
                    else 
                        raise (ProtoTypeParsingException ("Parse error: I was expecting a \')\' but I found " ^ !token))
                
                | p when (!token.[0] == ')') ->             (* Parsing the type of a function without arguments *)
					token := ProtoTypeScanner.next s;
					if ((compare !token "->") == 0) then         
                    	let fun_output = parse_ground_type s in 
                            TypeDef(fun_name, (FunType([], fun_output)))
                    else 
                        raise (ProtoTypeParsingException ("Parse error: I was expecting a \'->\' but I found " ^ !token))
                      
                | _                          -> raise (ProtoTypeParsingException ("Parse error: " ^ !token))
    
    else 
        raise (ProtoTypeParsingException ("Parse error: I was expecting a \"(\" but I found " ^ !token))

(* Parses an arrow type *)
let parse_arrow_type s = 
    
    let t = parse_type_def s
    in 
        match t with
            TypeDef(s, t) -> t


(* Returns the string representation of a type *)
let rec type_to_string t = 

	let rec args_to_string args = 
        match args with
		  [] 	-> ""
		| [x] -> type_to_string x
		| x::xs -> (type_to_string x) ^ ", " ^ (args_to_string xs)

	in 
        match t with
		      Field	 		     -> "F"
		    | Local(l) 			 -> (match l with Bool -> "B" | Num -> "N")
		    | FunType(args, out) -> "(" ^ (args_to_string args)  ^ ") -> " ^ (type_to_string out)

(* Returns the string representation of a type definition *)
let typedef_to_string td =
	
    match td with 
        TypeDef(fun_name, fun_type) -> fun_name ^ ": " ^ (type_to_string fun_type)


