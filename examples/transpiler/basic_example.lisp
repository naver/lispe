;Date: 11/08/2022
;Author: 
;Description: 

(load (+ _current "transpiler.lisp"))

(setq code 
   `
Function appel(A, B)
   I = 20
   If A == B Then
      I = I + 100
   Else
      I = I + A * B
      println "Appel:", I
   EndIf
   return I
EndFunction


Function fact(A)
   If  A <> 1 Then
       A * fact(A - 1)
   Else
        1
   EndIf
EndFunction

DIM D[100]
DIM U$[100]

e = 10 + 20 / 3
println "e=",e
e = 10 + (20 / 3)
println "e=",e

AZ = integers(1, 2, 3)
AZ.push(100, 200, 300*-1)
println AZ[2], AZ.size(), AZ, AZ[1].cos()

A12 = -9.987
A = 100
B = 30
E = 3
C$ = "Chaine de test"

U$[10] = "test de dim de strings"
D[10] = -1 + A - B + 10

A12 =  -1 * B

If A12 < 0 Then
   println "Négatif:", A12
   A12 = A12 * -1
   println "A12 vaut:", A12
EndIf

If A12 > 0 Then
   println "Positif enfin:", A12
Else
   println "Toujours négatif:", A12
EndIf

A = A + (100 * B) + 20

B = sin(E) + A

println "C:", C$.size()

println "Cos:", cos(10)

println("Valeurs:", A + 100 , C$, D[10], U$[10])

If A + 12 <> 11 or B == 30 or E == 101 Then
   A = A + 1
   C$ = C$ + ": Alors"
   println "Then:", A, C$
Else
   B = B - A
   println "Else:", B
EndIf

A = 100

While A > 10
   A = A - 1
   print A, ","
EndWhile

println()
For A = 0, A < 10 , A = A + 1 
   print "A=", A, " -> "
   For B = 0, B <= 10, B = B + 2
       println "A + B=", A, B, (A + B), " "
   EndFor
EndFor

For A in range(1,10,1)
    println "A in:", A
    println "We loop"
EndFor

println()
println "Ici:", appel(10,10)
println "Là:", appel(10,10+40)

println "Fact:", fact(9)
`
)

(setq tree (basic_abstract_tree code))

(println 'temps
   (elapse
      (setq lsp (transpile code))
   )
)
;(println (prettify lsp))
(eval lsp)

