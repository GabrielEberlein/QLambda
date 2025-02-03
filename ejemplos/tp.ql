def qbit = printState "Qbit to teleport: " (H (new 0))

def BellMeasure = \q2.\q1.let <x,y> = CNOT <q1,q2> in <meas(H x), meas y>

def EPR = \x.CNOT <H(new 0), new 0>

def U11 = \q. Z (X q)
def U10 = \q. Z q
def U01 = \q. X q
def U00 = \q. q

def U = \q.\<x,y>. if x then (if y then U11 q else U10 q)
                        else (if y then U01 q else U00 q)

def telep = let <x,y> = printState "\nState after entangled qbits are added: " (EPR *) in
                let f = BellMeasure x in
                let g = U y
                 in <f,g>

def main = let <f,g> = telep in printState "\nState after teleportation: " (g (f qbit))