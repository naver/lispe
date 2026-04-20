;Date: 20/04/2026
;Author: Claude Roux
;Description: Example of transpiling code

(load (+ _current "basic.lisp")) 
(load (+ _current "transpiler.lisp"))


; BasAIc
(setq code 
   «
REM first function definition

if not test
   println("ok")
endif

prompts = prompts.atob().json_parse()

c = u.test(10,20).json_parse().json()

b = join(r,":")

a = b.join(e)

println prompts[-1]["content"]

d = {"a": 2 + 10, "b":3, "c":24}

[e,r] = [10,20]

appel('test')

b = [1,2,3]

a = f"qs{json b}dkl"

a = """Ceci est un test....
Et un "autre" test
"""

identifiant = true
b["autre",12][-1] = 101

v =: a[:2][3][5][2,-1]

if identifiant == 10 and u == 100 and test == 100
  println "Ok"
endif 

l = list(100,2000,3000,"qhdqdhj")
l.push(10,302, "\n", "qskjdk","sdkjskj")
a = integers(10, 20, 30, 40, 60, 70, 50)
println a, a.type()

maplist( [lambda(x) x*2], l)

u = `(+ u 20)` 
b = dictionary()
u = float(20)
u = u & 0x1ff
u = u & 0b1010

idx = 0
function entry(prompts)
   prompts = json_parse(atob(prompts))
   prompts = prompts.atob().json_parse()
   d = split(theuserdata[idx], "\n")
   for s in d
        if "/" in s
            premier = s[0: "/"]
            second = s["/":0]
		prompts[-1,"content"] = premier
            callchat(prompts, 'entrypoint')
        else
            prompts[-1,"content"] = s
            callchat(prompts, 'entrypoint')
        endif
   endfor 
   idx += 1
   if idx < 3 then
        execute_when(20000, name("entry"), prompts.json())
   endif
endfunction

A = [λ (x,y) if x > y then x+y else x*y endif](10,20)
   B = [x**2 for x in [1,2,3,4] if x%2 == 0]
   switch(a)
   case "<>": 1
   case "==": 2
   case "ttt": 3
   endswitch
   test = [10,20,30,40,50,60,70,80,90]
   test[2:4] = 10
   try
   a = 2
   println a
   a = 23 + 2*19
   except(e)
   println e
   endtry
   »)

;-----------------------------------------------------------------------------
(println . compile code)
;-----------------------------------------------------------------------------

(println "-----------------------------------------------------------------------------")
(println "-----------------------------------------------------------------------------")
(println "-----------------------------------------------------------------------------")

;-----------------------------------------------------------------------------
; Python Code Example
;-----------------------------------------------------------------------------

(setq code `
clean_display()
idx = 0
results = []

def entry(prpts):
    prompts = []
    query = prpts[-1]["content"]
    prompts.append(prpts[0])
    prompts.append(prpts[-1])
    if query.lower() == "go":
        query = ""
    else:
      query += "\n"
   println("Query=", query)
   d = split(theuserdata[idx], "\n")
   for s in d:
    	if "/" in s:
            premier = s[0: "/"]
            second = s["/":0]
            prompts[-1,"content"] = query + premier
            callchat(prompts, 'entrypoint')
    	else:
            prompts[-1,"content"] = query + s
            callchat(prompts, 'entrypoint')
   idx += 1

println("Ici")
try:
   println("a")
catch(e):
   println(e)

`)

;-----------------------------------------------------------------------------
(println . compilepython code)
;-----------------------------------------------------------------------------

