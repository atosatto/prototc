exception ProtoScanException of string

let i = ref 0

let is_digit v = let c = Char.code v in (47 < c & c < 58)

let is_letter n = let c = Char.code n in (96 < c & c < 123)

let is_simbol s = ((s == '_' || s == '-') || s == '+')

(* Scanner *)
let rec next s = 
    (* print_string ("Input string: " ^ s ^ "\n");  *)
    match s.[!i] with
    
          ' ' | '\n' | '\t'            ->  i := !i + 1; next s
        | '(' | ')'             ->  i := !i + 1; (String.sub s (!i - 1) 1)
        | '#'                   ->  i := !i + 1; 
                                    let bool_string = next s in (* Devo riconoscere esattamente il nome t o il nome f *)
                                        if (compare bool_string "t" == 0 || compare bool_string "f" == 0) then 
                                            "#" ^ bool_string
                                        else 
                                            raise (ProtoScanException ("Invalid token: " ^ "#" ^ bool_string ^ " at " ^ string_of_int(!i)))
        
        | v when is_digit(v)    ->  let rec take_digits j =
                                        let index = !i + j in
                                    (*  print_char s.[index]; print_string " "; *)
                                        try
                                            if is_digit(s.[index]) then (take_digits (j + 1)) else j
                                        with Invalid_argument("index out of bounds") -> j
                                    in
                                        let a = (take_digits 1) and 
                                            b = !i 
                                        in
                                            (*print_int a; print_string " "; print_int b; *) 
                                            i := !i + a; (String.sub s b a)

        | x when is_letter(x)   ->  let rec take_names j =
                                        let index = !i + j in
                                        try
                                            if ((is_digit(s.[index]) || is_letter(s.[index])) || is_simbol(s.[index])) then (take_names (j + 1)) else j
                                        with Invalid_argument("index out of bounds") -> j
                                    in
                                        let a = (take_names 1) and
                                            b = !i 
                                        in
                                            i := !i + a; (String.sub s b a)

         | a when (a == '>' || a == '<')
                                ->  i := !i +1; 
                                    if (s.[!i] == '=') then 
                                        (i := !i + 1; (String.make 1 a) ^ "=") 
                                    else (String.make 1 a)

        | _                     ->  i := !i +1; (String.sub s (!i - 1) 1)