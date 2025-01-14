# Este programa de ejemplo hace uso de la recursion, basicamente va tirando una moneda y hasta que salga 0 recursa 
# aplicando una compuerta Hadamard a un qbit con valor inicial |0⟩ en cada paso. 
# De esta forma si salio 1 un numero impar de veces el qbit quedara balanceado y si salio 1 un numero par de veces 
# el qbit colapsara siempre a 0.

def c = \x.meas (H (new 0))

def M = let rec f x = if c * then H (f x) else x 
            in printState "result: " (f (new 0))