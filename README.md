# PLC Project
Topics to discuss:
    - Non-recursive functions need to be treated as variable declarations of anonymous functions
        e.g. fun f(x:t) = e; e1  is  to be treated as var f = fn (x:t) => e end; e1
        -> This can be done in the makeFun function in PlcParserAux
    - Disucss if we want to copy Hw6's use of n-nary functions (which I think the specs say we can or the email he sent out)
    - What is the point of using the etup = "()" instead of tup = "$tuple" in the PlcParserAux?
        -> Can we remove this and just use tup = "$tuple" everywhere?