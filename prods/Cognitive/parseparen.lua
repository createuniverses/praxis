function f6Pressed()
  edParseParentheses(0)
end

-- to test this, I need to send praxis code.
-- In Windows I have the clipboard method. In Linux,
-- I need to implement the socket or pipe thing, or find out
-- how to do the clipboard method.

if string.sub(str,1,1) == "p" then print("its p") end

clearTrace()
clearError()

function parseOpenParen(pos,bias)
  local text = getBufferText()
  local stack = bias
  pos = pos + 1
  while stack ~= -1 and pos < #text do
    if string.sub(text,pos,pos) == "(" then stack = stack + 1 end
    if string.sub(text,pos,pos) == ")" then stack = stack - 1 end
    pos = pos + 1
  end
  
  if stack == -1 then
    return pos-1
  else
    return -1
  end
end

for i=0,6,1 do
  ppr = parseOpenParen(i,0)
  if ppr ~= nil then
    print(i .. ":" .. ppr)
  else
    print(i .. ": nil")
  end
end

do
  i = 2
  ppr = parseOpenParen(i,0)
  edSetPosition(i)
  edSetAnchor(ppr)
end

clearTrace()

function wordwrap(s, n)
  local s2 = ""
  local c  = n
  s2 = string.sub(s,1,n)
  while c < #s do
    s2 = s2 .. "\n" .. string.sub(s, c+1, c+n)
    c = c + n
  end
  return s2
end

function doLisp(c)
  print2(lisp(c))
end

function doLisp(c)
  print2(wordwrap(lisp(c), 60))
end

function f3Pressed()
  local txt = getSExpr()
  print2("")
  doLisp(txt)
end

function f4Pressed()
  local txt = getSelectedText()
  doLisp(txt)
end

--moveWindowRight()

newBuffer()
loadBuffer("wizards_game.scm")

--newBuffer()
--loadBuffer("infiniteLoopBreakTest.lua")

newBuffer()
loadBuffer("deriv.scm")

newBuffer()
loadBuffer("patternMatching.scm")

newBuffer()
loadBuffer("procedureAnnotate2.scm")












