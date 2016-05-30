
function factorial (n)
  if n <= 1 then
    return 1
  else
    return n * factorial(n-1)
  end 
end

print2(factorial(10))
3628800

setBufferName("cps-factorial.lua")

diagfns = {}

function factorial (n)
 local fn = function ()
  if n <= 1 then
    return 1
  else
    return n * factorial(n-1)
  end
 end
 table.insert(diagfns, fn)
 return fn()
end

print2(factorial(10))
3628800

print2(#diagfns)
10

print2(diagfns[3]())
40320

720

5040


function factorial (n)
 local fn = function ()
  if n <= 1 then
    return 1
  else
    return n * factorial(n-1)
  end
 end
 coroutine.yield(coroutine.create(fn),n)
 --coroutine.yield(n)
 return fn()
end

function factorial (n)
 local fn = function ()
  if n <= 1 then
    return 1
  else
    return n * factorial(n-1)
  end
 end
 coroutine.yield(fn,fn())
 return fn()
end

co = coroutine.create(

print2(factorial(10)
co = coroutine.create(function () return factorial(10) end)


print2(coroutine.resume(co))

co1 = co
co = co1
clearTrace()
clearError()

do
  local b,r,n = coroutine.resume(co)
  print(n)
  if type(r) == "thread" then
    co = r
    print("coroutine continuing")
  elseif type(r) == "function" then
    co = coroutine.create(r)
    print("fresh coro")
  else
    print("result = " .. r)
  end
end
  
clearError()

print2

co1 = co

print2(factorial(10))
print2(type(co))
thread
clearError()

boolean
trying to make it all work at once

go slow and easy.
continuation passing style
read sicp
step 1: get lua one shot coroutine to work usably for factorial
step 2: continuation passing style implementation of factorial


thread
print2(coroutine.status(co))
clearError()

Step 1:

function factorial (n)
 local fn = function ()
  if n <= 1 then
    return 1
  else
    return n * factorial(n-1)
  end
 end
 coroutine.yield()
 return fn()
end

co = coroutine.create(function () return factorial(10) end)

do
  while coroutine.status(co) ~= "dead" do
    local d,r = coroutine.resume(co)
    if d then print("true") else print("false") end
    if r ~= nil then
      print(r)
    end
  end
end











do
uimainwidget.Widgets = {}
local w = uimainwidget.Widgets
w[1] = addButton(0,10,function(b) end)
w[2] = addButton(5,32,function(b) end)
--w[3] = redslider
w[3] = colorwheelgrp
w[4] = dome
end


