module PlcChecker

open Absyn
open Environ

let rec teval (e : expr) (env : plcType env) : plcType = 
    match e with 
    | ConI _ -> IntT
    
    | ConB _ -> BooT
    
    | EList t -> t
    
    | Var v -> lookup env v
    
    | Let (x, e1, e2) -> let t = teval e1 env in
                         let nenv = (x,t) :: env in
                           teval e2 nenv
    
    | Letrec (f, x, xt, e1, rt, e2) -> 
        let ft = FunT (xt, rt) in
        let e1env = (x, xt) :: (f, ft) :: env in
        let e2env = (f,ft) :: env in
        if teval e1 e1env = rt then
            teval e2 e2env
        else
            failwith ("TypeChecker: Return type differs from expression result " + f)
    
    | Prim1 (op, e) ->
        let eType = teval e env in
        match (op, eType) with
        | ("!", BooT)     -> BooT
        | ("-", IntT)     -> IntT
        | ("hd", LisT t)  -> t
        | ("tl", LisT _)  -> eType
        | ("ise", LisT _) -> BooT
        | ("print", _)    -> TupT []
        | _ -> failwith ("TypeChecker: Operation " + op + " applied to invalid type")
    
    | Prim2 (op, e1, e2) ->
        let te1 = teval e1 env in
        let te2 = teval e2 env in
        match (op, te1, te2) with
        | ("&&", BooT, BooT) -> BooT
        | ("::", t , LisT t') when t = t' -> LisT t
        | ("+", IntT, IntT) -> IntT
        | ("*", IntT, IntT) -> IntT
        | ("-", IntT, IntT) -> IntT
        | ("/", IntT, IntT) -> IntT
        | ("<", IntT, IntT) -> BooT
        | ("<=", IntT, IntT) -> BooT
        | ("=", t1, t2) when t1 = t2 -> BooT
        | ("!=", t1, t2) when t1 = t2 -> BooT
        | (";", _, t) -> t
        | _ -> failwith ("TypeChecker: Unknown operation " + op + " applied to type " + type2string te1 + " and type " + type2string te2)
    
    | If (e1, e2, e3) -> 
        let te1 = teval e1 env in
        match te1 with
        | BooT -> let te2 = teval e2 env in
                  let te3 = teval e3 env in
                  if te2 = te3 then
                    te2
                  else
                    failwith ("TypeCheker: Resulting expressions of 'if' must have same type; not " + type2string te2 + " != " + type2string te3)
        | _ -> failwith ("TypeChecker: First expression of 'if' must be of type Bool")

    | Call (Var f, e) -> 
        match lookup env f with
        | FunT (xt, rt) -> 
            let eType = teval e env
            if  eType = xt then
                rt
            else 
                failwith ("TypeChecker: Expected input type " + type2string xt + "; Observed input type " + type2string eType)
        | _ -> failwith ("TypeChecker: Function " + f + " is undefined")
    | Call (Call (f,e'), e) -> 
        let crt = teval (Call (f,e')) env in
        match crt with
        | FunT (xt, rt) ->
            let eType = teval e env
            if eType = xt then
                rt
            else failwith ("TypeChecker: Expected input type " + type2string xt + "; Observed input type " + type2string eType)
        | _ -> failwith ("TypeChecker: Internal function call returned insufficient type")

    | Call _ -> failwith ("TypeChecker: Illegal call to a function")

    | Tuple eList -> TupT (List.map (fun e -> teval e env) eList)

    | Sel (e, num) -> 
        match teval e env with
        | TupT t ->
            if 0 < num && num <= List.length t then
                List.item (num - 1) t
            else
                failwith ("TypeChecker: Index out of range for TupT " + list2string type2string "" t )
        | _ -> failwith ("TypeChecker: Illegal operation #" + string num + "applied to non-tuple")
    | Anon (x, xt, e) -> 
        let aenv = (x, xt) :: env in
        let eType = teval e aenv in
            FunT (xt, eType)

