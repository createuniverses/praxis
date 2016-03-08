
do
  local corofn
  function corofn()
    for i=1,10,1 do
      print2(i)
      coroutine.yield()
    end  
  end
  
  coro = coroutine.create(corofn)
  
end


--print2(coroutine.status(coro))

--coroutine.resume(coro)

do
  closeBuffer()
  switchToBuffer("dome2.lua")
end
