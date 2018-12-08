module PlcInterp

open Absyn
open Environ

let rec eval (e : expr) (env : plcVal env) : plcVal = 
    match e with 
    | ConI i -> IntV i
    
    | ConB b -> BooV b
    
    | EList _ -> LisV []
    
    | Var v -> lookup env v
    
    | Let (x, e1, e2) -> let e1' = eval e1 env in
                         let nenv = (x,e1') :: env in
                           eval e2 nenv
    
    | Letrec (f, x, _, e1, _, e2) -> 
        let fclos = Clos (f, x, e1, env) in
        let nenv = (f, fclos) :: env in
            eval e2 nenv
    
    | Prim1 (op, e) ->
        let re = eval e env in
        match (op, re) with
        | ("!", BooV b)   -> if b then BooV false else BooV true
        | ("-", IntV i)   -> IntV (-i)
        | ("hd", LisV l)  -> l.Head
        | ("tl", LisV l)  -> LisV l.Tail
        | ("ise", LisV l) -> if List.isEmpty l then BooV true else BooV false
        | ("print", _)    -> printfn "%s" (val2string re) ; TupV []
        | _ -> failwith ("Interpreter: Invalid operation " + op + " on " + val2string re)
    
    | Prim2 (op, e1, e2) ->
        let re1 = eval e1 env in
        let re2 = eval e2 env in
        match (op, re1, re2) with
        | ("&&", BooV b1, BooV b2) -> if b1 && b2 then BooV true else BooV false 
        | ("::", v , LisV l)       -> LisV (v :: l)
        | ("+", IntV i1, IntV i2)  -> IntV (i1 + i2)
        | ("*", IntV i1, IntV i2)  -> IntV (i1 * i2)
        | ("-", IntV i1, IntV i2)  -> IntV (i1 - i2)
        | ("/", IntV i1, IntV i2)  -> IntV (i1 / i2)
        | ("<", IntV i1, IntV i2)  -> if i1 < i2 then BooV true else BooV false
        | ("<=", IntV i1, IntV i2) -> if i1 <= i2 then BooV true else BooV false
        | ("=", r1, r2)            -> if r1 = r2 then BooV true else BooV false
        | ("!=", r1, r2)           -> if r1 = r2 then BooV false else BooV true
        | (";", _, r)              -> r
        | _ -> failwith ("Interpreter: Unknown operation " + op + " applied to type " + val2string re1 + " and type " + val2string re2)
    
    | If (e1, e2, e3) -> 
        let re1 = eval e1 env in
        match re1 with
        | BooV b when b -> eval e2 env
        | BooV _ -> eval e3 env                   
        | _ -> failwith ("Interpreter: First expression of 'if' didn't return a Bool")

    | Call (Var f, e) -> 
        let clos = lookup env f
        match clos with
        | Clos (f, x, e1, fenv) -> 
            let valx = eval e env in
            let nenv = (x,valx) :: (f, clos) :: fenv in
                eval e1 nenv
        | _ -> failwith ("Interpreter: Function " + f + " is undefined")
    | Call (Call (f,e'), e) -> 
        let cr = eval (Call (f,e')) env in
        match cr with
        | Clos (f, x, e1, fenv) ->
            let valx = eval e env in
            let nenv = (x,valx) :: fenv in
                eval e1 nenv
        | _ -> failwith ("Interpreter: Internal function call returned insufficient type")

    | Call _ -> failwith ("Interpreter: Illegal call to a function")

    | Tuple l -> TupV (List.map (fun e -> eval e env) l)

    | Sel (e, num) -> 
        match eval e env with
        | TupV t -> List.item (num - 1) t                
        | _ -> failwith ("Interpreter: Illegal operation #" + string num + "applied to non-tuple")
        
    | Anon (x, _, e) -> Clos ("", x, e, env)

