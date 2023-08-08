# Minischeme-Interpreter
An interpreter designed for Minischeme using Racket

Grammar for Minischeme that this interpreter supports:

EXP → number                     
    | symbol                      
    | (if EXP EXP EXP)            
    | (let (LET-BINDINGS) EXP)    
    | (lambda (PARAMS) EXP)       
    | (set! symbol EXP)           
    | (begin EXP*)                
    | (letrec (LET-BINDINGS) EXP) 
    | (EXP EXP*)                  
LET-BINDINGS → LET-BINDING*
LET-BINDING → [symbol EXP]
PARAMS → symbol*

Write syntactically correct scheme code using the above grammar in minischeme.rkt's REPL(Can use DrRacket). This should produce respective outputs according to the given inputs. 
