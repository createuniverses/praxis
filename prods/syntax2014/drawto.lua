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
