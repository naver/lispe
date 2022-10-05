;Date: 11/08/2022
;Author: Claude Roux
;Description: Example of Basicois code

(load (+ _current "hellenica.lisp")) 
(load (+ _current "transpiler.lisp"))

(setq code 
   ` 
συνάρτηση appel(A, B)
   I = 20
   αν A == B τότε
      I = I + 100
   αλλιώς
      I = I + A * B
      εμφάνισεγρ "Appel:", I
   τέλοςαν
   επιστροφή I
τέλοςσυνάρτησης

συνάρτηση fact(a)
   αν a <> 1 τότε
      a * fact(a-1)
   αλλιώς
      1
   τέλοςαν
τέλοςσυνάρτησης

διάσταση R[5,5,5]
διάσταση R$[5,5,5]
διάσταση D[100]
διάσταση U$[100]

R = εύρος(0, 125,1)
R[1,1,1] = 100
εμφάνισεγρ "R[0,1]=", R[0,1]
εμφάνισεγρ "R[1,4,4]=", R[1,4,4]

e = 10 + 20 / 3
εμφάνισεγρ "e=",e
e = 10 + (20 / 3)
εμφάνισεγρ "e=",e

AZ = ακέραιοι(1, 2, 3)
AZ.στοιβάζω(100, 200, 300*-1)
εμφάνισεγρ AZ[2], AZ.μέγεθος(), AZ, AZ[1].cos()

A12 = -9.987
A = 100
B = 30
E = 3
C$ = "Chaine de test"

U$[10] = "test de dim de strings"
D[10] = -1 + A + B + 10

A12 =  -1 * B

αν A12 < 0 τότε
   εμφάνισεγρ "Négatif:", A12
   A12 = A12 * -1
   εμφάνισεγρ "A12 vaut:", A12
τέλοςαν

αν A12 > 0 τότε
   εμφάνισεγρ "Positif enfin:", A12
αλλιώς
   εμφάνισεγρ "Toujours négatif:", A12
τέλοςαν

A = A + (100 * B) + 20

B = sin(E) + A

εμφάνισεγρ "C:", C$.μέγεθος()

εμφάνισεγρ "Cos:", cos(10)

εμφάνισεγρ("Valeurs:", A + 100 , C$, D[10], U$[10])

αν A + 12 <> 11 ή B == 30 ή E == 101 τότε
   A = A + 1
   C$ = C$ + ": τότε"
   εμφάνισεγρ "τότε:", A, C$
αλλιώς
   B = B - A
   εμφάνισεγρ "Else:", B
τέλοςαν

A = 100

ενώ A > 10
   A = A - 1
   εμφάνισε A, ","
τέλοςενώ

εμφάνισεγρ()
για A = 0, A < 10 , A = A + 1 
   εμφάνισε "A=", A, " -> "
   για B = 0, B <= 10, B = B + 2
       εμφάνισεγρ "A + B=", A, B, (A + B), " "
   τέλοςγια
τέλοςγια

για A μέσα εύρος(1,10,1)
    εμφάνισεγρ "A in:", A
    εμφάνισεγρ "Nous bouclons"
τέλοςγια

εμφάνισεγρ()
εμφάνισεγρ "Ici:", appel(10,10)
εμφάνισεγρ "Là:", appel(10,10+40)

εμφάνισεγρ "Fact:", fact(9)

kdonnées = δεδομένα 10, 20, 30, 40, 50, "A", "B", "C" τέλοςδεδομένα
εμφάνισεγρ kdonnées, kdonnées[5], kdonnées.μέγεθος()

αν "ab" μέσα "abc" τότε
   εμφάνισεγρ "Oui"
αλλιώς
   εμφάνισεγρ "Non"
τέλοςαν

εμφάνισεγρ αριθμός("199") + 20.98

contenu = διαβάστεαρ(_current + "basicois")
εμφάνισεγρ μέγεθος(contenu.εξάγετε(0, "dans"))

REM Une lambda

A = [(x,y) αν x > y τότε x+y αλλιώς x*y τέλοςαν](10,20)
εμφάνισεγρ A

`
)

;(setq tree (abstract_tree code))

(println 'temps
   (elapse
      (setq lsp (transpile code))
   )
)

;lsp contains the transpiled code as a list
;(println (prettify lsp))
(eval lsp)


