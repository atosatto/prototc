exception ProtoTypeScanException of string

let i = ref 0

let is_digit v = let c = Char.code v in (47 < c & c < 58)

let is_letter n = let c = Char.code n in (96 < c & c < 123)

let is_simbol s = ((s == '_' || s == '-') || s == '+')

(* Type Scanner *)
let rec next s = try match s.[!i] with
    
      ' ' | '\n' | '\t'     ->  i := !i + 1; next s
    | '(' | ')'             ->  i := !i + 1; (String.sub s (!i - 1) 1)
    | '-'                   ->  i := !i + 1; 
                                if (s.[!i] == '>') then 
                                    (i := !i + 1; "->") 
                                else "-"
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
    
    with Invalid_argument "index out of bounds" -> "fine" 