;Date: 11/08/2022
;Author: Claude Roux
;Description: Example of Basicois code

(load (+ _current "transpiler.lisp"))
(load (+ _current "basicois.lisp")) 

(setq code 
   `
fonction appel(A, B)
   I = 20
   si A == B alors
      I = I + 100
   sinon
      I = I + A * B
      afficheln "Appel:", I
   finsi
   renvoie I
finfonction

fonction fact(a)
   si a <> 1 alors
      a * fact(a-1)
   sinon
      1
   finsi
finfonction

dim R[5,5,5]
dim R$[5,5,5]
dim D[100]
dim U$[100]

R = interval(0, 125,1)
R[1,1,1] = 100
afficheln "R[0,1]=", R[0,1]
afficheln "R[1,4,4]=", R[1,4,4]

e = 10 + 20 / 3
afficheln "e=",e
e = 10 + (20 / 3)
afficheln "e=",e

AZ = entiers(1, 2, 3)
AZ.empile(100, 200, 300*-1)
afficheln AZ[2], AZ.taille(), AZ, AZ[1].cos()

A12 = -9.987
A = 100
B = 30
E = 3
C$ = "Chaine de test"

U$[10] = "test de dim de strings"
D[10] = -1 + A + B + 10

A12 =  -1 * B

si A12 < 0 alors
   afficheln "Négatif:", A12
   A12 = A12 * -1
   afficheln "A12 vaut:", A12
FinSi

Si A12 > 0 alors
   println "Positif enfin:", A12
Sinon
   println "Toujours négatif:", A12
FinSi

A = A + (100 * B) + 20

B = sin(E) + A

afficheln "C:", C$.taille()

afficheln "Cos:", cos(10)

afficheln("Valeurs:", A + 100 , C$, D[10], U$[10])

Si A + 12 <> 11 ou B == 30 ou E == 101 alors
   A = A + 1
   C$ = C$ + ": Alors"
   afficheln "Alors:", A, C$
Sinon
   B = B - A
   afficheln "Else:", B
FinSi

A = 100

tant que A > 10
   A = A - 1
   affiche A, ","
FinTantQue

afficheln()
pour A = 0, A < 10 , A = A + 1 
   affiche "A=", A, " -> "
   pour B = 0, B <= 10, B = B + 2
       afficheln "A + B=", A, B, (A + B), " "
   FinPour
FinPour

Pour A dans interval(1,10,1)
    afficheln "A in:", A
    afficheln "Nous bouclons"
FinPour

afficheln()
afficheln "Ici:", appel(10,10)
afficheln "Là:", appel(10,10+40)

afficheln "Fact:", fact(9)

kdonnées = données 10, 20, 30, 40, 50, "A", "B", "C" findonnées
afficheln kdonnées, kdonnées[5], kdonnées.taille()

si "ab" dans "abc" alors
   afficheln "Oui"
sinon
   afficheln "Non"
finsi

afficheln réel("199") + 20.98

contenu = liref(_current + "basicois")
afficheln taille(contenu.extraire(0, "dans"))

REM Une lambda

A = [(x,y) si x > y alors x+y sinon x*y finsi](10,20)
afficheln A

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

