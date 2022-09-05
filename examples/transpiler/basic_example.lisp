;Date: 11/08/2022
;Author: 
;Description: 

(load (+ _current "transpiler.lisp"))

(setq code 
   `
function appel(A, B)
   I = 20
   if A == B then
      I = I + 100
   else
      I = I + A * B
      println "Appel:", I
   endIf
   return I
endfunction


function fact(a)
   if a <> 1 then
      a * fact(a-1)
   else
      1
   endif
endfunction

dim R[5,5,5]
dim R$[5,5,5]
dim D[100]
dim U$[100]

R = range(0, 125,1)
R[1,1,1] = 100
println "R[0,1]=", R[0,1]
println "R[1,4,4]=", R[1,4,4]

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

if A + 12 <> 11 or B == 30 or E == 101 then
   A = A + 1
   C$ = C$ + ": Alors"
   println "Then:", A, C$
else
   B = B - A
   println "Else:", B
endif

A = 100

While A > 10
   A = A - 1
   print A, ","
EndWhile

println()
for A = 0, A < 10 , A = A + 1 
   print "A=", A, " -> "
   for B = 0, B <= 10, B = B + 2
       println "A + B=", A, B, (A + B), " "
   endfor
endfor

for A in range(1,10,1)
    println "A in:", A
    println "We loop"
endfor

println()
println "Ici:", appel(10,10)
println "Là:", appel(10,10+40)

println "Fact:", fact(9)

kdata = data 10, 20, 30, 40, 50, "A", "B", "C" endData
println kdata

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







