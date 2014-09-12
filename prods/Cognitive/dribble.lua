praxis:

clearError()

setClipboardText(inspect(debug.getinfo(getFunction2)))

{
  currentline = -1,
  func = <function 1>,
  istailcall = false,
  isvararg = false,
  lastlinedefined = 18,
  linedefined = 3,
  namewhat = "",
  nparams = 1,
  nups = 1,
  short_src = '[string "\r..."]',
  source = '\r\n\r\nfunction getFunction2(fn)\r\n  local dt = debug.getinfo(fn)\r\n  if string.sub(dt.source, 1,1) == "@" then\r\n    --print(dt.short_src)\r\n    --print(dt.linedefined, dt.lastlinedefined)\r\n    local filetext = readFile(dt.short_src)\r\n    -- this perhaps needs to be written in lua\r\n    local fntext = selectLines(filetext, dt.linedefined, dt.lastlinedefined)\r\n    --print(fntext)\r\n    return fntext,dt\r\n  else\r\n    local fntext = selectLines(dt.source, dt.linedefined, dt.lastlinedefined)\r\n    --print(fntext)\r\n    return fntext,dt\r\n  end\r\nend\r\n\r\nsetClipboardText(getFunction2(render))\r\n',
  what = "Lua"
}

{
  currentline = -1,
  func = <function 1>,
  istailcall = false,
  isvararg = false,
  lastlinedefined = 90,
  linedefined = 58,
  namewhat = "",
  nparams = 0,
  nups = 1,
  short_src = "main.lua",
  source = "@main.lua",
  what = "Lua"
}


function getFunction2(fn)
  local dt = debug.getinfo(fn)
  if string.sub(dt.source, 1,1) == "@" then
    --print(dt.short_src)
    --print(dt.linedefined, dt.lastlinedefined)
    local filetext = readFile(dt.short_src)
    -- this perhaps needs to be written in lua
    local fntext = selectLines(filetext, dt.linedefined, dt.lastlinedefined)
    --print(fntext)
    return fntext,dt
  else
    local fntext = selectLines(dt.source, dt.linedefined, dt.lastlinedefined)
    --print(fntext)
    return fntext,dt
  end
end

setClipboardText(getFunction2(render))

function render()
  WidgetLib.renderAll()
  --WidgetLib.callAll("render")
  
  renderGears()
  renderGearBots()
  fugue.render()
  
  --renderSlipnet()
  --renderWorkspace()
  
  --testGLColorFunc()
  
  -- colorGL(255,255,0,255)
  
  -- local rpos,wpos,rtot,wtot = getSampleMarkers()
  
  -- drawText2D("" .. rtot, 10,15)
  -- drawText2D("" .. wtot, 10,10)
  
  -- drawText2D("" .. lpfEffNode.samplesLastFrame, 10,5)
  
  SynthNode.renderInputs(sineConNode)
  SynthNode.renderInputs(sineConNode2)
  SynthNode.renderInputs(sineGenNode)
  SynthNode.renderInputs(lpfEffNode)
  SynthNode.renderInputs(sinkNode)
  
  SynthNode.render(lpfEffNode, vec2d(0,0), vec2d(100,30))
  
  --ttestrender()
  
end


local dt = debug.getinfo(render)

setClipboardText(dt.source)

@main.lua

setClipboardText(getFunction("getFunction"))

function getFunction(fnname)
  local dt = debug.getinfo(
    load("return " .. string.gsub(fnname, ":", "."))())
  if string.sub(dt.source, 1,1) == "@" then
    --print(dt.short_src)
    --print(dt.linedefined, dt.lastlinedefined)
    local filetext = readFile(dt.short_src)
    -- this perhaps needs to be written in lua
    local fntext = selectLines(filetext, dt.linedefined, dt.lastlinedefined)
    --print(fntext)
    return fntext,dt
  else
    local fntext = selectLines(dt.source, dt.linedefined, dt.lastlinedefined)
    --print(fntext)
    return fntext,dt
  end
end
