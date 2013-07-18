(* Handles the list of the Proto operators *)

(* The list of the default language's operators *)
let operators_list = ref [
    
    ("inf",     "(inf) -> N)");                                    (* Constants *)
                                    
    ("not",     "(not ,x|B) -> B");                                (* Logical Operators *)
    ("and",     "(and , x|B ,y|B) -> B"); 
    ("or",      "(or , x|B ,y|B) -> B"); 
    ("muxand",  "(muxand , x|B ,y|B) -> B");
    ("muxor",   "(muxor , x|B ,y|B) -> B");               
    ("xor",     "(xor , x|B ,y|B) -> B");               
    
    ("+",       "(+ ,x|N ,y|N) -> N");                             (* Arithmetic Operators *)
    ("-",       "(- ,x|N ,y|N) -> N");                                                             
    ("/",       "(/ ,x|N ,y|N) -> N");
    ("*",       "(* ,x|N ,y|N) -> N");
    ("max",     "(max ,x|N ,y|N) -> N"); 
    ("min",     "(min ,x|N ,y|N) -> N");

    ("=",       "(= ,x|N ,y|N) -> B");                             (* Comparison Operators *)
    ("<",       "(< ,x|N ,y|N) -> B"); 
    ("<=",      "(<= ,x|N ,y|N) -> B"); 
    (">",       "(> ,x|N ,y|N) -> B"); 
    (">=",      "(>= ,x|N ,y|N) -> B"); 
    ("is-zero", "(is-zero ,x|N) -> B"); 
    ("is-neg",  "(is-neg ,x|N) -> B"); 
    ("is-pos",  "(is-pos ,x|N) -> B");            
    
    ("nbr",       "(nbr ,expr|N) -> F");                           (* Field Functions *)
    ("nbr-range", "(nbr-range) -> F"); 
    ("nbr-angle", "(nbr-angle) -> F"); 
    ("nbr-lag",   "(nbr-lag) -> F"); 

    ("min-hood",  "(min-hood ,expr|F ) -> N");                     (* Summary Functions *)
    ("min-hood+", "(min-hood+ ,expr|F) -> N"); 
    ("max-hood",  "(max-hood ,expr|F) -> N");
    ("max-hood+", "(max-hood+ ,expr|F) -> N");
    ("all-hood",  "(all-hood ,expr|F) -> B"); 
    ("all-hood+", "(all-hood+ ,expr|F) -> B"); 
    ("any-hood",  "(any-hood ,expr|F) -> B"); 
    ("any-hood+", "(any-hood+ ,expr|F) -> B"); 
    ("sum-hood",  "(sum-hood ,expr|F) -> N"); 
    ("int-hood",  "(int-hood ,expr|F) -> N"); 

]

(* Checks if the operator s is defined *)
let exists s = 
    List.exists (fun x -> let op = fst(x) in ((compare s op) == 0)) (!operators_list)

(* Returns the string representation of the type of the s operator *)
let get_type s = 
    snd(List.find  (fun x -> let op = fst(x) in ((compare s op) == 0)) (!operators_list))