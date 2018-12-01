(* 
  Make sure you regenerate the Parser and Lexer
  every time you modify PlcLexer.fsl or PlcParser.fsy
*)

(*
// Windows only
   #r "\bin\FsLexYacc.Runtime.dll"
   #load "H:\Desktop\Project\Environ.fs" 
   #load "H:\Desktop\Project\Absyn.fs" 
   #load "H:\Desktop\Project\PlcParserAux.fs"
   #load "H:\Desktop\Project\PlcParser.fs"
   #load "H:\Desktop\Project\PlcLexer.fs" 
   #load "H:\Desktop\Project\Parse.fs" 
   #load "H:\Desktop\Project\PlcInterp.fs"
   #load "H:\Desktop\Project\PlcChecker.fs"
   #load "H:\Desktop\Project\Plc.fs"

   // Mac Os ony
#r "bin/FsLexYacc.Runtime.dll"
   #load "Environ.fs"
   #load "Absyn.fs"
   #load "PlcParserAux.fs"
   #load "PlcParser.fs"
   #load "PlcLexer.fs"
   #load "Parse.fs"
   #load "PlcInterp.fs"
   #load "PlcChecker.fs"
   #load "Plc.fs"
*)



open Absyn
let fromString = Parse.fromString // string parser function

let run e = printfn "\nResult is  %s\n" (Plc.run e)   // execution function




(* Examples in concrete syntax *)
