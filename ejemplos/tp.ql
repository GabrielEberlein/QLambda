def EPR = (CNOT(H(new 0), new 0))

def BellMeasure = \q1.\q2.let (x,y) = CNOT (q1,q2) in (meas(H x), meas y)

def U = \q.\x.\y.match x with (_-> match y with () |
                               _->)

def telep = let (x,y) = EPR in
                let f = BellMeasure x in
                let g = U 