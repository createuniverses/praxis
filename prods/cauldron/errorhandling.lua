
function f8Pressed()
  local name = getBufferName()
  newBuffer()
  setBufferName("error")
  print2(getErrorText())
  print2("")
  print2("")
  print2([[do]])
  print2([[  closeBuffer()]])
  print2([[  switchToBuffer("]] .. name .. [[")]])
  print2([[end]])
  edSetPosition(0)
  clearError()
end

function f9Pressed()
  newBuffer()
  print2(getTraceText())
  clearTrace()
end
