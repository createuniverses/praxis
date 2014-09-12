do
local s = "hello"
print2(#s)
end
5
print2(string.sub("hello", 3,-1))
llo


function wordwrap(s, n)
  return string.sub(s, 1,n) .. "\n" .. string.sub(s, n+1,n*2)
end
print2(getFunction("ps7"))
function ps7(c) print2(wordwrap(s7(c), 20)) end



function f3Pressed()
  local txt = getSExpr()
  print2("")
  ps7(txt)
end



