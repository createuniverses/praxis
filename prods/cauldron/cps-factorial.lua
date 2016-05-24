
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







do
uimainwidget.Widgets = {}
local w = uimainwidget.Widgets
w[1] = addButton(0,10,function(b) end)
w[2] = addButton(5,32,function(b) end)
--w[3] = redslider
w[3] = colorwheelgrp
w[4] = dome
end
