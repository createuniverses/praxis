setBufferName("tracebackthing.lua")

myfns = {}

do
 myfns[1] = function ()
   print2("myfns1")
   myfns[2]()
 end
 myfns[2] = function ()
   print2("myfns2")
   --print2(debug.traceback())
   --print2(inspect(debug.getinfo(2)))
   print2(getFunction(debug.getinfo(2).func))
 end
end

clearError()

myfns[1]()
myfns1
myfns2
 myfns[1] = function ()
   print2("myfns1")
   myfns[2]()
 end

function showcallingcode(fnname)
  -- change the definition of fn so it spits out calling code.
  -- after first run, sets itself back how it was.
end

------------------------------------

function wraptoshowcaller(fn)
  local wrappedfn = function (...)
    print2(getFunction(debug.getinfo(2).func))
    fn(...)
  end
  return wrappedfn
end

function wraptoshowcaller(fn, setfn)
  local wrappedfn = function (...)
    print2(getFunction(debug.getinfo(2).func))
    fn(...)
    setfn(fn)
  end
  setfn(wrappedfn)
end

wraptoshowcaller(edForceUpdate, function (fn) edForceUpdate = fn end)
print2(getFunction(edForceUpdate))
  local wrappedfn = function (...)
    print2(getFunction(debug.getinfo(2).func))
    fn(...)
    setfn(fn)
  end

do
  local tmpfn = function () edForceUpdate() end
  tmpfn()
end
  local tmpfn = function () edForceUpdate() end




  local wrappedfn = function (...)
    print2(getFunction(debug.getinfo(2).func))
    fn(...)
    setfn(fn)
  end

edForceUpdate()



do
 myfns = {}
 myfns[1] = function ()
   print2("myfns1")
   myfns[2]()
 end
 myfns[2] = function ()
   print2("myfns2")
 end
 --myfns[2] = wraptoshowcaller(myfns[2])
 wraptoshowcaller(myfns[2], function (fn) myfns[2] = fn end)
end

print2(getFunction(myfns[2]))
 myfns[2] = function ()
   print2("myfns2")
 end


  local wrappedfn = function (...)
    print2(getFunction(debug.getinfo(2).func))
    fn(...)
    setfn(fn)
  end



myfns[1]()
myfns1
 myfns[1] = function ()
   print2("myfns1")
   myfns[2]()
 end

myfns2

myfns1
myfns2

myfns1
 myfns[1] = function ()
   print2("myfns1")
   myfns[2]()
 end

myfns2

myfns1
myfns2

myfns1
myfns2

myfns1
 myfns[1] = function ()
   print2("myfns1")
   myfns[2]()
 end

myfns2



-------------------------------

myfns1
myfns2
{
  currentline = 4,
  func = nil --[[<function 1>]],
  istailcall = false,
  isvararg = false,
  lastlinedefined = 5,
  linedefined = 2,
  name = "?",
  namewhat = "field",
  nparams = 0,
  nups = 1,
  short_src = '[string "do..."]',
  source = 'do\n myfns[1] = function ()\n   print2("myfns1")\n   myfns[2]()\n end\n myfns[2] = function ()\n   print2("myfns2")\n   --print2(debug.traceback())\n   print2(inspect(debug.getinfo(2)))\n end\nend',
  what = "Lua"
}

myfns1
myfns2
{
  currentline = 9,
  func = nil --[[<function 1>]],
  istailcall = false,
  isvararg = false,
  lastlinedefined = 10,
  linedefined = 6,
  name = "?",
  namewhat = "field",
  nparams = 0,
  nups = 1,
  short_src = '[string "do..."]',
  source = 'do\n myfns[1] = function ()\n   print2("myfns1")\n   myfns[2]()\n end\n myfns[2] = function ()\n   print2("myfns2")\n   --print2(debug.traceback())\n   print2(inspect(debug.getinfo(1)))\n end\nend',
  what = "Lua"
}

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





