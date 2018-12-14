module Test

open Absyn

let cases = 
  (
    let s = "0"
    let e = ConI 0
    let t = IntT
    let r = IntV 0 
    (s, e, t, r)
  ) :: (
   let s = "5+3*4"  
   let e = Prim2 ("+",ConI 5,Prim2 ("*",ConI 3,ConI 4))
   let t = IntT
   let r = IntV 17 
   (s, e, t, r) 
  ) :: ( 
   let s = "-3 < 4" 
   let e = Prim2 ("<",Prim1 ("-",ConI 3),ConI 4)
   let t = BooT
   let r = BooV true
   (s, e, t, r)
  ) :: (
    let s = "!(3 = 4)" 
    let e = Prim1 ("!",Prim2 ("=",ConI 3,ConI 4))
    let t = BooT
    let r = BooV true
    (s, e, t, r)
  ) :: (
    let s = "3+1 = 4 && 4 <= 3"
    let e = Prim2 ("&&",Prim2 ("=",Prim2 ("+",ConI 3,ConI 1),ConI 4), Prim2 ("<=",ConI 4,ConI 3))
    let t = BooT
    let r = BooV false
    (s, e, t, r)
  ) :: (
    let s = "if 3 = 2 then 0 else 1 + 4"
    let e = If (Prim2 ("=",ConI 3,ConI 2),ConI 0,Prim2 ("+",ConI 1,ConI 4))
    let t = IntT
    let r = IntV 5
    (s, e, t, r)
  ) :: (
    let s = "3 + if 3 = 2 then 0 else 1"
    let e = Prim2 ("+",ConI 3,If (Prim2 ("=",ConI 3,ConI 2),ConI 0,ConI 1))
    let t = IntT
    let r = IntV 4
    (s, e, t, r)
  ) :: (
    let s = "4; true"
    let e = Prim2 (";",ConI 4,ConB true)
    let t = BooT
    let r = BooV true
    (s, e, t, r)
  ) :: (
    let s = "()"
    let e = Tuple []
    let t = TupT []
    let r = TupV []
    (s, e, t, r)
  ) :: (
    let s = "4 * (true; 6)"
    let e = Prim2 ("*",ConI 4,Prim2 (";",ConB true,ConI 6))
    let t = IntT
    let r = IntV 24
    (s, e, t, r)
  ) :: (
    let s = "( )"
    let e = Tuple []
    let t = TupT []
    let r = TupV []
    (s, e, t, r)
  ) :: (
    let s = "(1,false,())"
    let e = Tuple [ConI 1; ConB false; Tuple []]
    let t = TupT [IntT; BooT; TupT []]
    let r = TupV [IntV 1; BooV false; TupV[]]
    (s, e, t, r)
  ) :: (
    let s = "(1,(2,3),4)"
    let e = Tuple [ConI 1; Tuple [ConI 2; ConI 3]; ConI 4]
    let t = TupT [IntT; TupT [IntT; IntT]; IntT]
    let r = TupV [IntV 1; TupV [IntV 2; IntV 3]; IntV 4]
    (s, e, t, r)
  ) :: (
    let s = "(true,false)#1"
    let e = Sel (Tuple [ConB true; ConB false],1)
    let t = BooT
    let r = BooV true
    (s, e, t, r)
  ) :: (
    let s = "((5,6),false)#1#2"
    let e = Sel (Sel (Tuple [Tuple [ConI 5; ConI 6]; ConB false],1),2)
    let t = IntT
    let r = IntV 6
    (s, e, t, r)
  ) :: (
    let s = "1 + {3}"
    let e = Prim2 ("+",ConI 1,ConI 3)
    let t = IntT
    let r = IntV 4
    (s, e, t, r)
  ) :: (
    let s = "print false"
    let e = Prim1 ("print",ConB false)
    let t = TupT []
    let r = TupV []
    (s, e, t, r)
  ) :: (
    let s = "print (1 - 3)"
    let e = Prim1 ("print",Prim2 ("-",ConI 1,ConI 3))
    let t = TupT []
    let r = TupV []
    (s, e, t, r)
  ) :: (
    let s = "([]: List[Int])"
    let e = EList (LisT IntT)
    let t = LisT IntT
    let r = LisV []
    (s, e, t, r)
  ) :: (
    let s = "([]: List[Int])"
    let e = EList (LisT IntT)
    let t = LisT IntT
    let r = LisV []
    (s, e, t, r)
  ) :: (
    let s = "([]: List[Bool])"
    let e = EList (LisT BooT)
    let t = LisT BooT
    let r = LisV []
    (s, e, t, r)
  ) :: (
    let s = "([]: List[Unit])"
    let e = EList (LisT (TupT []))
    let t = LisT (TupT [])
    let r = LisV []
    (s, e, t, r)
  ) :: (
    let s = "([]: List[List[Int]])"
    let e = EList (LisT (LisT IntT))
    let t = LisT (LisT IntT)
    let r = LisV []
    (s, e, t, r)
  ) :: (
    let s = "([]: List[Int -> Unit])"
    let e = EList (LisT (FunT (IntT,TupT [])))
    let t = LisT (FunT (IntT, TupT []))
    let r = LisV []
    (s, e, t, r)
  ) :: (
    let s = "
    ([]: List[Int -> Int -> Bool])"
    let e = EList (LisT (FunT (IntT,FunT (IntT,BooT))))
    let t = LisT (FunT (IntT, FunT (IntT, BooT)))
    let r = LisV []
    (s, e, t, r)
  ) :: (
    let s = "([]: List[Tuple[Unit, Int, Bool]])"
    let e = EList (LisT (TupT [TupT []; IntT; BooT]))
    let t = LisT (TupT [TupT []; IntT; BooT])
    let r = LisV []
    (s, e, t, r)
  ) :: (
    let s = "1 :: ([]: List[Int])"
    let e = Prim2 ("::",ConI 1,EList (LisT IntT))
    let t = LisT IntT
    let r = LisV [IntV 1]
    (s, e, t, r)
  ) :: (
    let s = "1 :: 2 :: ([]: List[Int])"
    let e = Prim2 ("::",ConI 1,Prim2 ("::",ConI 2,EList (LisT IntT)))
    let t = LisT IntT
    let r = LisV [IntV 1; IntV 2]
    (s, e, t, r)
  ) :: (
    let s = "(1,2) :: (3,4) :: ([]: List[Tuple[Int,Int]])"
    let e = Prim2 ("::",Tuple [ConI 1; ConI 2], Prim2 ("::",Tuple [ConI 3; ConI 4],EList (LisT (TupT [IntT; IntT]))))
    let t = LisT (TupT [IntT; IntT])
    let r = LisV [TupV [IntV 1; IntV 2]; TupV [IntV 3; IntV 4]]
    (s, e, t, r)
  ) :: (
    let s = "hd (1 :: 2 :: ([]: List[Int]))"
    let e = Prim1 ("hd",Prim2 ("::",ConI 1,Prim2 ("::",ConI 2,EList (LisT IntT))))
    let t = IntT
    let r = IntV 1
    (s, e, t, r)
  ) :: (
    let s = "tl (1 :: 2 :: ([]: List[Int]))"
    let e = Prim1 ("tl",Prim2 ("::",ConI 1,Prim2 ("::",ConI 2,EList (LisT IntT))))
    let t = LisT IntT
    let r = LisV [IntV 2]
    (s, e, t, r)
  ) :: (
    let s = "ise([]: List[Int])"
    let e = Prim1 ("ise",EList (LisT IntT))
    let t = BooT
    let r = BooV true
    (s, e, t, r)
  ) :: (
    let s = "ise(true::([]: List[Bool]))"
    let e = Prim1 ("ise",Prim2 ("::",ConB true,EList (LisT BooT)))
    let t = BooT
    let r = BooV false
    (s, e, t, r)
  ) :: (
    let s = "var x = 4; x+1"
    let e = Let ("x",ConI 4,Prim2 ("+",Var "x",ConI 1))
    let t = IntT
    let r = IntV 5
    (s, e, t, r)
  ) :: (
    let s = "{var x = 4; x+1}"
    let e = Let ("x",ConI 4,Prim2 ("+",Var "x",ConI 1))
    let t = IntT
    let r = IntV 5
    (s, e, t, r)
  ) :: (
    let s = "
    var x = 4; 
    var y = 6;
    x + y"
    let e = Let ("x",ConI 4,Let ("y",ConI 6,Prim2 ("+",Var "x",Var "y")))
    let t = IntT
    let r = IntV 10
    (s, e, t, r)
  ) :: (
    let s = "
    var x = 4; 
    print x;
    {var y = 6;
     print y
    }"
    let e = Let ("x",ConI 4,Prim2 (";",Prim1 ("print",Var "x"),Let ("y",ConI 6,Prim1 ("print",Var "y"))))
    let t = TupT []
    let r = TupV []
    (s, e, t, r)
  ) :: (
    let s = "1 + {var x = 9; x + x}"
    let e = Prim2 ("+",ConI 1,Let ("x",ConI 9,Prim2 ("+",Var "x",Var "x")))
    let t = IntT
    let r = IntV 19
    (s, e, t, r)
  ) :: (
    let s = "
    var a = (3,4);
    a#1 < a#2 
    "
    let e = Let ("a",Tuple [ConI 3; ConI 4],Prim2 ("<",Sel (Var "a",1),Sel (Var "a",2)))
    let t = BooT
    let r = BooV true
    (s, e, t, r)
  ) :: (
    let s = "
    var e = ([]:List[Bool]);
    true::false::e 
    "
    let e = Let ("e",EList (LisT BooT),Prim2 ("::",ConB true,Prim2 ("::",ConB false,Var "e")))
    let t = LisT BooT
    let r = LisV [BooV true; BooV false]
    (s, e, t, r)
  ) :: (
    let s = "fn (x:Int) => x end"
    let e = Anon ("x",IntT,Var "x")
    let t = FunT (IntT, IntT)
    let r = Clos ("", "x", Var "x", [])
    (s, e, t, r)
  ) :: (
    let s = "var f = fn (x:Int) => x end; f"
    let e = Let ("f",Anon ("x",IntT,Var "x"),Var "f")
    let t = FunT (IntT, IntT)
    let r = Clos ("", "x", Var "x", [])
    (s, e, t, r)
  ) :: (
    let s = "var f = fn (x:Int) => x end; f"
    let e = Let ("f",Anon ("x",IntT,Var "x"),Var "f")
    let t = FunT (IntT, IntT)
    let r = Clos ("", "x", Var "x", [])
    (s, e, t, r)
  ) :: (
    let s = "var f = fn (x:Int) => x end; f(10)"
    let e = Let ("f",Anon ("x",IntT,Var "x"),Call (Var "f",ConI 10))
    let t = IntT
    let r = IntV 10
    (s, e, t, r)
  ) :: (
    let s = "fun f (x:Int) = x; f"
    let e = Let ("f",Anon ("x",IntT,Var "x"),Var "f")
    let t = FunT (IntT, IntT)
    let r = Clos ("", "x", Var "x", [])
    (s, e, t, r)
  ) :: (
    let s = "fun f (x:Int) = {fun g(y:Int) = x+y; g}; f(3)(4)"
    let e = Let ("f", Anon ("x",IntT,Let ("g",Anon ("y",IntT,Prim2 ("+",Var "x",Var "y")),Var "g")),Call (Call (Var "f",ConI 3),ConI 4))
    let t = IntT
    let r = IntV 7
    (s, e, t, r)
  ) :: (
    let s = "fun f (x:Int) = fn (y:Int) => x+y end; f(3)(4)"
    let e = Let ("f",Anon ("x",IntT,Anon ("y",IntT,Prim2 ("+",Var "x",Var "y"))), Call (Call (Var "f",ConI 3),ConI 4))
    let t = IntT
    let r = IntV 7
    (s, e, t, r)
  ) :: (
    let s = "fun h (x:Int) = fn (y:Int) => fn (z:Int)  => x+y+z end end; h(3)(4)(5)"
    let e = Let ("h",Anon ("x",IntT,Anon ("y",IntT, Anon ("z", IntT, Prim2 ("+",Prim2("+", Var "x", Var "y"), Var "z")))), Call (Call (Call(Var "h", ConI 3),ConI 4),ConI 5))
    let t = IntT
    let r = IntV 12
    (s, e, t, r)
  ) :: (
    let s = "fun h (x:Int, y:Int, z:Int) = x+y+z; h(3,4,5)"
    let e = Let ("h",Anon ("$tuple",TupT [IntT; IntT; IntT], Let ("x",Sel (Var "$tuple",1), Let ("y",Sel (Var "$tuple",2), Let ("z", Sel (Var "$tuple", 3), Prim2 ("+",Prim2("+", Var "x", Var "y"), Var "z"))))), Call (Var "h", Tuple [ConI 3; ConI 4; ConI 5]))
    let t = IntT
    let r = IntV 12
    (s, e, t, r)
  ) :: (
    let s = "fun f (x:Int) = fn (y:Int) => x+y end; f(3)"
    let e = Let ("f",Anon ("x",IntT,Anon ("y",IntT,Prim2 ("+",Var "x",Var "y"))), Call (Var "f",ConI 3))
    let t = FunT (IntT, IntT)
    let r = Clos ("", "y", Prim2 ("+", Var "x", Var "y"), [("x", IntV 3); ("", Clos ("","x",Anon ("y",IntT,Prim2 ("+",Var "x",Var "y")),[]))])
    (s, e, t, r)
  ) :: (
    let s = "
    fun f (g: Int -> Bool) = if g(1) then 10 else 11; 
    fun h (x: Int) = 0 < x;
    f(h)
    "
    let e = Let ("f",Anon ("g",FunT (IntT,BooT),If (Call (Var "g",ConI 1),ConI 10,ConI 11)), Let ("h",Anon ("x",IntT,Prim2 ("<",ConI 0,Var "x")),Call (Var "f",Var "h")))
    let t = IntT
    let r = IntV 10
    (s, e, t, r)
  ) :: (
    let s = "fun rec f(x:Int):Int = if x <= 0 then 1 else x + f(x-1); f(5)"
    let e = Letrec ("f","x",IntT, If (Prim2 ("<=",Var "x",ConI 0),ConI 1, Prim2 ("+",Var "x",Call (Var "f",Prim2 ("-",Var "x",ConI 1)))),IntT, Call (Var "f",ConI 5))
    let t = IntT
    let r = IntV 16
    (s, e, t, r)
  ) :: (
    let s = "
    fun rec pr(x:Int): Unit = 
      if x <= 0 then 
        print(0)
      else { 
        print(x);
        pr(x-1)
      };
    pr(5)"
    let e = Letrec ("pr","x",IntT, If (Prim2 ("<=",Var "x",ConI 0),Prim1 ("print",ConI 0), Prim2 (";",Prim1 ("print",Var "x"),Call (Var "pr",Prim2 ("-",Var "x",ConI 1)))), TupT [],Call (Var "pr",ConI 5))
    let t = TupT []
    let r = TupV []
    (s, e, t, r)
  ) :: (
    let s = "
    fun rec len(l : List[Int]): Int = if ise(l) then 0 else 1 + len(tl(l)); 
    len(1::2::([]:List[Int]))"
    let e = Letrec ("len","l",LisT IntT, If (Prim1 ("ise",Var "l"),ConI 0, Prim2 ("+",ConI 1,Call (Var "len",Prim1 ("tl",Var "l")))),IntT, Call (Var "len",Prim2 ("::",ConI 1,Prim2 ("::",ConI 2,EList (LisT IntT)))))
    let t = IntT
    let r = IntV 2
    (s, e, t, r)
  ) :: (
    let s = "fn (x:Int, y:Int) => print(x - y) end"
    let e = Anon ("$tuple",TupT [IntT; IntT], Let ("x",Sel (Var "$tuple",1), Let ("y",Sel (Var "$tuple",2), Prim1("print", Prim2 ("-",Var "x",Var "y")))))
    let t = FunT (TupT [IntT; IntT], TupT [])
    let r = Clos ("", "$tuple", Let ("x",Sel (Var "$tuple",1), Let ("y",Sel (Var "$tuple",2), Prim1("print", Prim2 ("-",Var "x",Var "y")))), [])
    (s, e, t, r)
  ) :: (
    let s = "fn (x:Int, y:Int) => x - y end"
    let e = Anon ("$tuple",TupT [IntT; IntT], Let ("x",Sel (Var "$tuple",1), Let ("y",Sel (Var "$tuple",2),Prim2 ("-",Var "x",Var "y"))))
    let t = FunT (TupT [IntT; IntT], IntT)
    let r = Clos ("", "$tuple", Let ("x",Sel (Var "$tuple",1), Let ("y",Sel (Var "$tuple",2),Prim2 ("-",Var "x",Var "y"))), [])
    (s, e, t, r)
  ) :: (
    let s = "fun f(x:Int, y:Int) = x - y; f(5,4)"
    let e = Let ("f", Anon ("$tuple",TupT [IntT; IntT], Let ("x",Sel (Var "$tuple",1), Let ("y",Sel (Var "$tuple",2),Prim2 ("-",Var "x",Var "y")))), Call (Var "f",Tuple [ConI 5; ConI 4]))
    let t = IntT
    let r = IntV 1
    (s, e, t, r)
  ) :: (
    let s = "
    var p = (1,3);
    fun f(x:Int, y:Int) = x - y; 
    f(p)"
    let e = Let ("p",Tuple [ConI 1; ConI 3], Let ("f", Anon ("$tuple",TupT [IntT; IntT], Let ("x",Sel (Var "$tuple",1), Let ("y",Sel (Var "$tuple",2),Prim2 ("-",Var "x",Var "y")))), Call (Var "f",Var "p")))
    let t = IntT
    let r = IntV -2
    (s, e, t, r)
  ) :: (
    let s = "fun f(x:Int, y:Int, z: Int) = x - y * z ; f(5,4,2)"
    let e = Let ("f", Anon ("$tuple",TupT [IntT; IntT; IntT], Let ("x",Sel (Var "$tuple",1), Let ("y",Sel (Var "$tuple",2), Let ("z",Sel (Var "$tuple",3), Prim2 ("-",Var "x",Prim2 ("*",Var "y",Var "z")))))), Call (Var "f",Tuple [ConI 5; ConI 4; ConI 2]))
    let t = IntT
    let r = IntV -3
    (s, e, t, r)
  ) :: (
    let s = "
    fun rec mem(x: Int, l : List[Int]): Bool = 
      if ise(l) then false 
      else if x = hd(l) then true else mem(x, tl(l)); 
    mem(2, 1::2::([]:List[Int]))"
    let e = Letrec ("mem","$tuple",TupT [IntT; LisT IntT],Let ("x",Sel (Var "$tuple",1), Let ("l",Sel (Var "$tuple",2),If (Prim1 ("ise",Var "l"),ConB false,If (Prim2 ("=",Var "x",Prim1 ("hd",Var "l")),ConB true, Call (Var "mem",Tuple [Var "x"; Prim1 ("tl",Var "l")]))))),BooT, Call (Var "mem", Tuple [ConI 2; Prim2 ("::",ConI 1,Prim2 ("::",ConI 2,EList (LisT IntT)))]))
    let t = BooT
    let r = BooV true
    (s, e, t, r)
  ) :: (
    let s = "
    var E = ([] : List[Int]);
    fun reverse (l : List[Int]) = {
      fun rec rev (l1 : List[Int], l2 : List[Int]): List[Int] =
        if ise(l1) then
          l2 
        else
          rev(tl(l1), hd(l1)::l2);
      rev(l, E)
    };
    reverse (1::2::3::E)"
    let e = Let ("E",EList (LisT IntT),Let ("reverse",Anon ("l",LisT IntT,Letrec ("rev","$tuple",TupT [LisT IntT; LisT IntT],Let ("l1",Sel (Var "$tuple",1),Let ("l2",Sel (Var "$tuple",2), If (Prim1 ("ise",Var "l1"),Var "l2",Call (Var "rev",Tuple [Prim1 ("tl",Var "l1");Prim2 ("::",Prim1 ("hd",Var "l1"),Var "l2")])))),LisT IntT,Call (Var "rev",Tuple [Var "l"; Var "E"]))),Call (Var "reverse",Prim2 ("::",ConI 1,Prim2 ("::",ConI 2,Prim2 ("::",ConI 3,Var "E"))))))
    let t = LisT IntT
    let r = LisV [IntV 3; IntV 2; IntV 1]
    (s, e, t, r)
  ) :: (
    let s = "
    fun rec map (f : Int -> Int) : (List[Int] -> List[Int]) =
      fn (l: List[Int]) =>
        if ise(l) then l else f(hd(l)) :: map(f)(tl(l))
      end ;
    map (fn (x:Int) => 2*x end) ([10;20;30])"
    let e = Letrec ("map","f",FunT (IntT,IntT),Anon("l",LisT IntT,If(Prim1 ("ise",Var "l"),Var "l",Prim2("::",Call (Var "f",Prim1 ("hd",Var "l")),Call (Call (Var "map",Var "f"),Prim1 ("tl",Var "l"))))),FunT (LisT IntT,LisT IntT),Call(Call (Var "map",Anon ("x",IntT,Prim2 ("*",ConI 2,Var "x"))),Prim2 ("::",ConI 10,Prim2 ("::",ConI 20,Prim2 ("::",ConI 30,EListNT)))))
    let t = LisT IntT
    let r = LisV [IntV 20; IntV 40; IntV 60]
    (s, e, t, r)
  ) :: (
    let s = "[1;2;3;4]"
    let e = Prim2("::", ConI 1, Prim2("::", ConI 2, Prim2("::", ConI 3, Prim2("::", ConI 4, EListNT))))
    let t = LisT IntT
    let r = LisV [IntV 1; IntV 2; IntV 3; IntV 4]
    (s, e, t, r)
  ) :: (
    let s = "[1;2;3] = (1::2::3::([]:List[Int]))"
    let e = Prim2("=",Prim2("::", ConI 1, Prim2("::", ConI 2, Prim2("::", ConI 3, EListNT))), Prim2 ("::",ConI 1,Prim2 ("::",ConI 2,Prim2 ("::",ConI 3, EList (LisT IntT)))))
    let t = BooT
    let r = BooV true
    (s, e, t, r)
  ) :: (
    let s = "var highAdd = fn (x:Int) (y:Int) => x + y end ; {var add3 = highAdd(3); add3 (4)}"
    let e = Let ("highAdd",Anon ("x",IntT,Anon ("y",IntT,Prim2 ("+",Var "x",Var "y"))), Let ("add3",Call (Var "highAdd",ConI 3),Call (Var "add3",ConI 4)))
    let t = IntT
    let r = IntV 7
    (s, e, t, r)
  ) :: (
    let s = "
    fun rec map (f : Int -> Int) (l: List[Int]) : List[Int] =
        if ise(l) then l else f(hd(l)) :: map(f)(tl(l))
      ;
    map (fn (x:Int) => 2*x end) ([10;20;30])"
    let e = Letrec ("map","f",FunT (IntT,IntT),Anon("l",LisT IntT,If(Prim1 ("ise",Var "l"),Var "l",Prim2("::",Call (Var "f",Prim1 ("hd",Var "l")),Call (Call (Var "map",Var "f"),Prim1 ("tl",Var "l"))))),FunT (LisT IntT,LisT IntT),Call(Call (Var "map",Anon ("x",IntT,Prim2 ("*",ConI 2,Var "x"))),Prim2 ("::",ConI 10,Prim2 ("::",ConI 20,Prim2 ("::",ConI 30,EListNT)))))
    let t = LisT IntT
    let r = LisV [IntV 20; IntV 40; IntV 60]
    (s, e, t, r)
  )
  :: []
