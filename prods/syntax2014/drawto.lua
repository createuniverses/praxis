print2(getFunction("render"))
function render()
  WidgetLib.renderAll()
end
clearError()

do
  cube = WidgetLib.addRender(
  function (o)
    drawLine(0,0,0,100,0,100)
  end)
end

drawTo_data = {x = 0, y = 0, z = 0}

function movePen(x,y,z)
  drawTo_data = {x = x, y = y, z = z}
end

function drawTo(x,y,z)
  drawLine(drawTo_data.x,
           drawTo_data.y,
           drawTo_data.z,
           x,y,z)
  drawTo_data = {x = x, y = y, z = z}
end

do
  cube.s = 20
  cube.render = function (o)
    movePen(0,1,0)
    drawTo(o.s,1,0)
    drawTo(o.s,1,o.s)
    drawTo(0,1,o.s)
    drawTo(0,1,0)
  end
end

transform.copy(cube.lspace, transform.identity())

transform.applyTranslation(cube.lspace, 0,0,0)
transform.rotate(cube.lspace, math.pi * 0.01, 0)
transform.rotate(cube.lspace, 0,math.pi * -0.01)

clearError()
continue()
