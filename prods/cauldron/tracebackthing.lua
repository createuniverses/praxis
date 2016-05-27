setBufferName("tracebackthing.lua")

myfns = {}


do
 myfns[1] = function ()
   print2("myfns1")
   myfns[2]()
 end
 myfns[2] = function ()
   print2("myfns2")
   print2(debug.traceback())
 end
end

myfns[1]()
myfns1
myfns2
stack traceback:
	[string "do..."]:8: in function '?'
	[string "do..."]:4: in function '?'
	[string "myfns[1]()"]:1: in main chunk
	[C]: in function 'luaCall'
	[string "    local sCode = edGetLuaBlock()..."]:9: in function 'action'
	editor.lua:75: in function 'onKeyDown'
	[string "onKeyDown(13)"]:1: in main chunk









do
  closeBuffer()
  switchToBuffer("cps-factorial.lua")
end
