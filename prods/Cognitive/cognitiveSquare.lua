praxis:
hideEditor()
showEditor()

praxis:
setClipboardText(getFunction("render"))

praxis:
function render()
  WidgetLib.renderAll()
  
  fugue.render()
  
  SynthNode.renderInputs(sineConNode)
  SynthNode.renderInputs(sineConNode2)
  SynthNode.renderInputs(sineGenNode)
  SynthNode.renderInputs(lpfEffNode)
  SynthNode.renderInputs(sinkNode)
  
  SynthNode.render(lpfEffNode, vec2d(0,0), vec2d(100,30))
end


praxis:
workspace = {}
slipnet = {}
coderack = {}

workspace.items = {}
workspace.items[1] = { salience = 1,
                       label = "first side",
                       p1 = vec3d(0,0,0),
                       p2 = vec3d(100,0,100),
                       render = "renderLine" }

cognitiveLib = {}
cognitiveLib.renderLine = function (item)
  drawLine(item.p1.x,item.p1.y,item.p1.z,
           item.p2.x,item.p2.y,item.p2.z)
end

workspace.render = function ()
  for i=1,#workspace.items,1 do
    local item = workspace.items[i]
    cognitiveLib[item.render](item)
  end
end

-- praxis:STOP

