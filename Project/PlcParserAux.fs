module ParAux

open Absyn
open PlcChecker
let tup =  "$tuple"
let etup =  "()"
let rec makeFunAux (n: int) (xs: (string * plcType) list) (e: expr) : expr = 
  match xs with
  | []     -> e
  | (x, t) :: r -> Let (x, Sel (Var tup, n), makeFunAux (n + 1) r e)  

let makeListType (e: expr) : expr = EList (LisT (teval e []))

let makeType (l: (string * plcType) list): plcType = TupT (List.map (fun (x,y) -> y) l)

let rec makeHighReturnType (l : (string * plcType) list list) (rt: plcType): plcType =
  match l with
  | []       -> rt
  | h :: []  -> let hType = makeType h in
                FunT(hType, rt)
  | h :: t   -> let tType = makeHighReturnType t rt in
                let hType = makeType h in
                FunT (hType, tType)

let makeFun (f: string) (xs: (string * plcType) list) (rt: plcType) (e1: expr) (e2: expr) : expr =
  match xs with
  | []           -> Letrec (f, etup, TupT [], e1, rt, e2)   
  | (x, t) :: [] -> Letrec (f, x, t, e1, rt, e2)
  | _            -> 
    let t = makeType xs in
    let e1' = makeFunAux 1 xs e1 in
    Letrec (f, tup, t, e1', rt, e2)

let rec makeFunHigh (f: string) (xs: (string * plcType) list list) (rt: plcType) (e1: expr) (e2: expr) : expr =
  match xs with
  | []      -> Letrec (f, etup, TupT [], e1, rt, e2)   
  | l :: [] -> makeFun f l rt e1 e2
  | l :: t  -> let retFun = makeFunHigh "" t rt e1 (Tuple []) in
               makeFun f l (makeHighReturnType xs rt) retFun e2

let makeAnon (xs: (string * plcType) list) (e: expr) : expr =
  match xs with
  | []           -> Anon (etup, TupT [], e)   
  | (x, t) :: [] -> Anon (x, t, e)
  | _            -> 
   let t = makeType xs in
   let e' = makeFunAux 1 xs e in
   Anon (tup, t, e') 

let rec makeAnonHigh (xs: (string * plcType) list list) (e: expr) : expr =
  match xs with
  | []       -> Anon (etup, TupT [], e)   
  | l :: []  -> makeAnon l e
  | l :: t   -> 
      let lType = makeType l in
      let e' = makeAnonHigh t e in
      Anon (tup, lType, e') 
