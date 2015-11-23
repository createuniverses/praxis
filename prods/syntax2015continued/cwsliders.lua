do
redslider = nil
greenslider = nil
blueslider = nil
end

do
redslider = redslider or Slider.new(vec3d(0,0,-50), 0, 255)
greenslider = greenslider or Slider.new(vec3d(0,0,-50), 0, 255)
blueslider = blueslider or Slider.new(vec3d(0,0,-50), 0, 255)
end


do
redslider.depth = 100
redslider.width = 10
greenslider.depth = 100
greenslider.width = 10
blueslider.depth = 100
blueslider.width = 10

--makePositionSaver("redslider")
--makePositionSaver("blueslider")
--makePositionSaver("greenslider")

transform.setTranslation(blueslider.lspace, 290,-1,27)
transform.setTranslation(greenslider.lspace, 270,-1,26)
transform.setTranslation(redslider.lspace, 252,-1,27)

transform.setTranslation(redslider.lspace, 20,0,0)
transform.setTranslation(greenslider.lspace, 40,0,0)
transform.setTranslation(blueslider.lspace, 60,0,0)
end
clearError()

print2(getErrorText())
[string "do..."]:13: bad argument #1 to 'setTranslation' (LiveCode.transform expected, got number)
stack traceback:
	[string "function onerror(s) endGL() glResetStencil(..."]:1: in function <[string "function onerror(s) endGL() glResetStencil(..."]:1>
	[C]: in function 'setTranslation'
	[string "do..."]:13: in main chunk
	[C]: in function 'luaCall'
	[string "    local sCode = edGetLuaBlock()..."]:9: in function 'action'
	editor.lua:75: in function 'onKeyDown'
	[string "onKeyDown(36)"]:1: in main chunk


clearError()








do
  closeBuffer()
  switchToBuffer("colorwheelwidget.lua")
end
