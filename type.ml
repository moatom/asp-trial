type t = 
  | Fun    of t * t
  | Tuple  of t * t
  | Ref    of t
  | List   of t
  | Array  of t
  | Var    of string
  | Int    
  | Bool   
  | Unit   
  | Float  
  | String 

let rec to_string : t -> string = 
  let open Printf in
  function
  | Fun (((Fun (_, _)) as e1), e2) ->
      sprintf "(%s) -> %s" (to_string e1) (to_string e2)
  | Fun (x, y) ->
      sprintf "%s -> %s" (to_string x) (to_string y)
  | Tuple (e1, ((Tuple (_, _)) as e2)) -> 
      sprintf "%s * (%s)" (to_string e1) (to_string e2)
  | Tuple (x,y) ->
      sprintf "%s * %s" (to_string x) (to_string y)
  | Ref x   -> sprintf "%s ref" @@ to_string x
  | List x  -> sprintf "%s list" @@ to_string x
  | Array x -> sprintf "%s array" @@ to_string x
  | Var x   -> x
  | Int     -> "int"
  | Bool    -> "bool"
  | Unit    -> "unit"
  | Float   -> "float"
  | String  -> "string"
