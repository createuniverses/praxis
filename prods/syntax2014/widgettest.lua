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
