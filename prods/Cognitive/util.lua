function mkdoifchanger(fn, varfn)
  local v = { v = varfn() }
  local r = function ()
    local nv = varfn()
    if nv ~= v.v then
      v.v = nv
      fn()
    end
  end
  
  local get = function ()
    return v.v
  end
  
  return { v = v, doit = r, get = get }
end


clearError()


do
  lnpr = mkdoifchanger(
    function ()
      local p = getEditorLineStart()
      print(p)
      print(edGetAt(p, 10))
      edSetPosition(p)
      if edGetAt(p, 1)==" " then
       edDelete()
      end
   end,
   getEditorLineStart) 
end

print2(lnpr.v)
627

627




print2(getErrorText())
[string "print2(getFunction(edRenderChar())"]:1: ')' expected near '<eof>'
[string "function mkdoifchanger(fn, varfn)..."]:15: unexpected symbol near 'do'


[string "lnpr = mkdoifchanger(function () print(getE..."]:1: attempt to call global 'mkdoifchanger' (a nil value)
stack traceback:
  [string "function onerror(s) endGL() glResetStencil(..."]:1: in function 'mkdoifchanger'
  [string "lnpr = mkdoifchanger(function () print(getE..."]:1: in main chunk

lnpr()
clearTrace()

print2(getFunction(edRenderChar))
function edRenderChar(c,n,xp,yp)
  local s = 3000
  if n == lnpr.v.v then --lnpr.get() then
    drawLine(0,0,0, s, 0, 0)
  end
  edStrokeCharacter(c,0,0)
end



print2(getFunction(render))
function render()
  WidgetLib.renderAll()
  --WidgetLib.callAll("render")

  lnpr.doit()
  
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



