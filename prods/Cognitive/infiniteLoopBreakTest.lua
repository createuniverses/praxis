-- Lua
do
 --newBuffer()
 local i = 10
 while i > 0 do
  --print2(i)
  --i = i - 1
 end
end

-- Lisp
print2(s7("(do ((i 0 (+ i 1))) ((= i -1)))"))

-- Forth
do
print2(forth(
[[: test 
    begin 0 until ;]]))
end

print2(forth("test"))

print2(forth("1 2 3 + . ."))

--continue()
--clearError()
--setBufferName("infiniteLoopBreakTest.lua")
