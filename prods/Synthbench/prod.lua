-- Name: prod.lua

inspect = require 'inspect'

function linearInterpolate(inmin, inmax, outmin, outmax, val)
  local proportion = (val - inmin) / (inmax - inmin)
  local out = proportion * (outmax - outmin) + outmin
  return out
end

dofile("unpack2.lua")
dofile("reflect.lua")
dofile("geometry.lua")
dofile("drawing.lua")
dofile("queue.lua")
dofile("widgets.lua")
dofile("slider.lua")
dofile("synth.lua")
dofile("fugue.lua")
dofile("cognitive.lua")

g_updateCount = 0

function update()
  WidgetLib.callAll("update")
  
  fugue.update()
  
  updateSlipnet()
  updateCoderack()
  

  local c = (170 * 0.5) + 30
  --orbitCamPP(c,10,c,0.01,0)

  SynthNode.updateSynthNode(sineConNode)
  SynthNode.updateSynthNode(sineConNode2)
  SynthNode.updateSynthNode(sineGenNode)
  SynthNode.updateSynthNode(lpfEffNode)
  SynthNode.updateSynthNode(sinkNode)
  
  -- local rpos,wpos,rtot,wtot = getSampleMarkers()
  
  -- while wtot < rtot + g_samplesPerRequest do
    -- rpos,wpos,rtot,wtot = getSampleMarkers()
    -- --local sample = SynthNode.getSample(sineGenNode)
    -- local sample = SynthNode.getSample(lpfEffNode)
    -- writeSample(sample)
  -- end
  
  g_updateCount = g_updateCount + 1
end

ttest = transform.new()
transform.setTranslation(ttest, 20,10,30)

function render()
  WidgetLib.callAll("render")
  
  fugue.render()
  
  renderSlipnet()
  renderWorkspace()
  --testGLColorFunc()
  
  colorGL(255,255,0,255)
  
  local rpos,wpos,rtot,wtot = getSampleMarkers()
  
  drawText2D("" .. rtot, 10,15)
  drawText2D("" .. wtot, 10,10)
  
  drawText2D("" .. lpfEffNode.samplesLastFrame, 10,5)
  
  SynthNode.renderInputs(sineConNode)
  SynthNode.renderInputs(sineConNode2)
  SynthNode.renderInputs(sineGenNode)
  SynthNode.renderInputs(lpfEffNode)
  SynthNode.renderInputs(sinkNode)
  
  --SynthNode.render(lpfEffNode, vec2d(0,0), vec2d(100,30))
  
  --drawLine(0,0,0,0,50,0)
  
  transform.setTranslation(ttest, 50 + math.sin(g_updateCount * 3 * (math.pi / 180)) * 100,60,-100)
  transform.lookAt(ttest, 100,20,100)
  --transform.setScale(ttest, math.sin(g_updateCount * 6 * (math.pi / 180)) * 2, 1,1)
  
  drawLine(100,20,100,100,50,100)
  drawLine(100,20,100, transform.getTranslation(ttest))
  --lookAt(transform.getTranslation(ttest))
  --setCamPos(100,60,100)
  
  --local px,py,pz = transform.localToGlobal(ttest,   0,  0,  0)
  --drawText2D("" .. px .. "," .. py .. "," .. pz, 50, 10)
  
  local pts = {}
  for i = 0,100,10 do
    for j = 0,100,10 do
      local pt  = vec3d(transform.localToGlobal(ttest, i,0,j))
      local pt2 = vec3d(transform.globalToLocal(ttest, pt.x,pt.y,pt.z))
      table.insert(pts, pt)
      table.insert(pts, pt2)
    end
  end
  beginLinGL()
    for i=1,#pts,1 do
      vectorGL(pts[i].x,pts[i].y,pts[i].z)
      vectorGL(pts[i].x,pts[i].y+10,pts[i].z)
    end
  endGL()
end

function testGLColorFunc()
  colorGL(255,0,0,255)
  local r,g,b,a = getColorGL()
  local str = r .. "," .. g .. "," .. b .. "," .. a
  drawText3DStroked(str, 10,0,0)
end

function f4Pressed()
  if sl2 == nil then
    sl2 = Slider.new(vec3d(0,0,0), 0.1, 10)
  end

  function sl2.update(slider)
    sineConNode2.freq = slider.pos
  end

  transform.setTranslation(sl2.lspace, Vector3D.getArgs(vec3d(getMouseCursorPos()) + vec3d(0,10,0)))
  transform.rotate(sl2.lspace, math.pi * 0.5, 0)
  --transform.lookAt(sl2.lspace, -100,10,-100)
end

function f5Pressed()
  clearWorkspace()
  activateConcept(slipnet.items["red"])
  activateConcept(slipnet.items["blue"])
end

function f6Pressed()
  playSound()
end

function f7Pressed()
  stopSound()
end

function f11Pressed()
  showEditor()
end

function f12Pressed()
  hideEditor()
end

clearTrace()

function printToBuffer(...)
  local s = ""
  local args = {...}
  for i=1,#args,1 do
    s = s .. "\n" .. args[i]
  end
  setBufferText(getBufferText() .. "\n" .. s)
  gotoBufferEnd()
end

function OnMouseMove(dx,dy,x,y)
  WidgetLib.callAllInRange("mousemove")
  --print(dx,dy,x,y)
end

function LMBDown(x,y)
  WidgetLib.callAllInRange("lmbdown")
end

function LMBUp(x,y)
  WidgetLib.callAllInRange("lmbup")
end

function RMBDown(x,y)
  WidgetLib.callAllInRange("rmbdown")
end

function RMBUp(x,y)
  WidgetLib.callAllInRange("rmbup")
end

setMaxFramerate(50)

--windowedMode(-900,10,800,450)
--windowedMode(-1000,100)
windowedMode()

-- setBufferText("dofile(\"prod.lua\")")
setCamPos(0,90,100)

playSound()
stopSound()

midiStart()

-- update, render and all input callbacks should all call those respective functions
-- for all registered objects.
