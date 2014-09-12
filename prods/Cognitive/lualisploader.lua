print2(package.path)
package.path = ".\\lualisp\\?.lua;"..package.path

print2(getFunction("f1Pressed"))
function f1Pressed() luaCall(getBufferText()) end

do
require "Environment"
require "Parser"
require "LispInterpreter"

env = Lisp.getGlobalEnv()
end

ok,value = pcall(Lisp.evalExpr, env, "(+ 1 2)")
print2(Sexpr.prettyPrint(value))
28

5
clearError()


env["lisptest"] = Sexpr.newFun("lisptest", Lisp.prim_lisptest)

ok,value = pcall(Lisp.evalExpr, env, "(+ 23 (lisptest))")


print2(value)

.\lualisp\LispInterpreter.lua:93: The symbol 'lisptest' is not defined

function lisptest() return 5 end


print2(Sexpr.prettyPrint(value))

print2(getFunction("Lisp.prim_plus"))
function Lisp.prim_lisptest(env, args)
   return Sexpr.newAtom(5)
end


