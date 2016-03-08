print(debug.traceback())
clearTrace()

clearTrace_bk = clearTrace
function clearTrace()
  clearTrace_bk()
  print(debug.traceback())
end
clearTrace = clearTrace_bk
clearTrace()

setBufferName("fntracedemo.lua")

