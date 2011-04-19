reset
;------------------------------------
def factorial : (eq o [id, ~0] -> ~1; / * o iota)
factorial : 3
factorial : 4
;------------------------------------
def may : (> -> (1#); (2#))
may : <3, 1>
may : <3, 4>
;------------------------------------
def max : / may
max : <1, 5, 7, 20, 4, 10>
;------------------------------------
; 
def firstatom : 1# o  / appendl o appendr o [alpha (atom -> id; ~<>), ~<>]
firstatom : <<21>, 3, 8>
firstatom : <1, <2, 3>, <8>, 4, 5, <6, 7>>
;------------------------------------
def prodint : alpha * o trans
prodint o [iota, iota] : 4
;------------------------------------
def menor : (< -> (1#) ; (2#))
def min : / menor
def minimax : min o (alpha max)
minimax : <<1, 2, 3>, <4, 5, 6>, <7, 8, 9>>
;------------------------------------
def pert : (/ or) o (alpha eq) o distl
pert : <1, <3, 4, 5>>
pert : <1, <3, 4, 1, 5>>
;------------------------------------
def single : eq o [length, ~1]
single : <1, 2>
single : <1>
;------------------------------------
def espar : eq o [* o [% o [id, ~2], ~2],  id]
espar : 2
espar : 3
;/ appendl o appendr o [, ~<>]
def delsecuencias : alpha (atom -> ~1; ~0)
delsecuencias : <1, <2, 3>, 4>
def cantatomos : / + o delsecuencias
cantatomos : <1, <2, 3>, 4>
def cantatompar : espar o cantatomos
cantatompar : <1, <2, 3>, 4>
cantatompar : <1, <2, 3>, 4, 5>
;------------------------------------
def concat : / appendl o appendr
concat : <<1, 2 3> <a, b, c>>
;;------------------------------------
;; Delete empty lists from list
;;------------------------------------
def filterempty : (null o (1#) -> (2#); appendl)
def removeempty : / filterempty o appendr o [id, ~<>]
removeempty : <1, 2, b, c, <>, d>