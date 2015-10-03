print2(getFunction(returnPressed))
print2(getFunction(edRenderChar))

function edRenderChar(c,xp,yp)
  edStrokeCharacter(c,0,0)
end


function returnPressed()
  edInsertNewline()
end

setBufferName("editorplaying.lua")
