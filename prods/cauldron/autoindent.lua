print2(getFunction("returnPressed"))

-- auto indent...

do
 function returnPressed()
  edInsertNewline()
 end
end
setBufferName("autoindent.lua")

saveBuffer()





do
  closeBuffer()
  switchToBuffer("turtle.lua")
end
