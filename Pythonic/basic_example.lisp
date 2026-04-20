;Date: 11/08/2022
;Author: 
;Description: 

(load (+ _current "basic.lisp")) 
(load (+ _current "transpiler.lisp"))

; (json X () (json_parse X 20 (test X 10 20)))

; (json (json_parse (test u 10 20) 20))


(setq code 
   «
REM first function definition

if not test
   println("ok")
endif

prompts = prompts.atob().json_parse()

c = rt.size(209).json()[10]

c = rt.size()

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

v =: a[:][3][5][2:-1]
v =: a[:2][3][5][2,-1]

v =: a[2,3][5]

if not identifiant.test() == 1
   println "Ok"
endif

if identifiant == 10 and u == 100 and test == 100
  println "Ok"
endif 

l = list(100,2000,3000,"qhdqdhj")


l.push(10,302, "\n", "qskjdk","sdkjskj")

println(l, 10, "Test")
a = integers(10, 20, 30, 40, 60, 70, 50)
println a, a.type()

maplist( [lambda(x) x*2], l)

u = `(+ u 20)` 

u = 'r'
b = dictionary()

u = float(20)

u = u & 0x1ff
u = u & 0b1010

println(b, b["constant"], u, type(u), a[1:], a[2:-2], a[:], a[:-1], a[:"constant"], a["constant":])


if u == 10
    println("Ok")
else
    println("Non")
endif

function entrypoint (chat)
      chat = jsjson(chat)
      println "Phrase:", chat[-1,"content"] 
      a = getstruct(chat[-1,"content"], "[", "]")
      println a[0]
      save_session()
endfunction

clean_display()
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
   println A

   B = [x*2 for x in [1,2,3,4]]
   B = [x**2 for x in [1,2,3,4] if x%2 == 0]
   A = 12 + 34**4/12 + 21 - 5

   d[2:3] = 100
   if  a := 1 and i >= 10
   println("ok")
   endif
   if A < 3 and B and C < 10
   println "Ok"
   endif

   if e < 10
   println "Ok"
   E = 20
   elif e > 30
   println "Non"
   E = 100
   elif e == 10
   println "Ah!!"
   E = 2
   else
   println("Fin")
   endif

   switch(a)
   case "<>": 1
   case "==": 2
   case "ttt": 3
   endswitch
   test = [10,20,30,40,50,60,70,80,90]
   test[2:4] = 10

   r = [10,20,30]

   try
   a = 2
   println a
   a = 23 + 2*19
   except(e)
   println e
   endtry

   try
   println a
   b = 23 + 2*19
   except
   println "Erreur"
   endtry

if (a==1011)
  println("ok")
endif


rule execute_tool(chat, msg)
   ("confluence_search" in msg)
   arg = jsonextract(msg, "data")
   call_mcp("confluence", "confluence_search_pages", arg, 'affiche')
endrule

pattern test(prompt, "Chat 2", 'aa', [a, b $ c], e, {"a":"b" $ z})
    println a,b,c
    appel(chat,'abc')
endpattern

pattern onglet("Chat 0", prompts)
    msg = "Gives me the code to sort out a vector of strings."
    push_message(theprompts[1], "Chat 1")
    input_chat(msg, "Chat 1")
endpattern

patternjs onglet(prompts, "Chat 1", prompts)
    callchat(prompts, 'entrypoint')
endpatternjs

thepathname+"_"+code[:pp]

class Test

  def __init__(self, x, y, z)
        self.k = x
        self.v = y
  enddef

  def func(self, x)
	self.u = x
  enddef
endclass

   »)



(println . prettify . abstract_tree code)
(println . compile code)
;(setq tok (tokenize_rules parser_tok code))

(setq code `
clean_display()
idx = 0
results = []
defjs entrypoint (chat):
    println "Phrase:", chat[-2,"content"] 
    a = getstruct(chat[-1,"content"], "[", "]")
    results.push(a[0].json_parse())
    save_session()

defjs entry(prpts):
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

defjs entry(prpts):
    clean_display()
    cmd = prpts[-1]["content"]
    prompts = [prpts[0]]
    if "Go" in cmd:
        idx = integer(cmd[2:])
        if idx == 0:
            idx = 1
        elif idx >= theuserdata.size():
            idx = len(theuserdata) - 1
        println("IDX=",idx)
	    d = theuserdata[idx].trim().json_parse()
	    msg = "Here is a rejected example:"+ d["messages_rejected"].json()+"\n"
    	msg += "Here is the corrected example:"+d["messages_accepted"].json() + "\n"
    #else:
     #  msg = cmd + "\n"
     #prompts.append({"role":"user", "content":msg})
     #callchat(prompts, 'entrypoint')

class Test:
	def __init__(self, x, y, z):
		self.k = x
		self.v = y

	def func(self, x):
		self.u = x

`)

(injecte_labels code)











