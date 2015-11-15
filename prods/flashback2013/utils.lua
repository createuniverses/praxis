-- Name: utils.lua

function searchWorkspace(description)
  matches = {}
  for i=1,#workspace.items,1 do
    if workspace.items[i].description == description then
      table.insert(matches,workspace.items[i])
    end
  end
  return matches
end

function copyWorkspace()
  copy = {}
  for i=1,#workspace.items,1 do
    table.insert(copy,workspace.items[i])
  end
  return copy
end

function workspaceDistanceSort(t,pos)
  table.sort(t,
    function (item1,item2)
      distance1 = Vector3D.magnitude(item1 - pos)
      distance2 = Vector3D.magnitude(item2 - pos)
      return distance1 < distance2
    end)
end

function printTable(t)
  for i=1,#t,1 do
    print(t[i].description .. " " .. t[i].x .. "," .. t[i].y)
  end
end

function rouletteSelection(t,f)
  total = 0
  for i=1,#t,1 do
    total = total + (t[i][f] * t[i][f])
  end
  if total <= 0 then return { i = 0, v = nil } end
  ball = math.random(total)
  cumtotal = 0
  for i=1,#t,1 do
    cumtotal = cumtotal + (t[i][f] * t[i][f])
    if cumtotal >= ball then
      return { i = i, v = t[i] }
    end
  end
  return { i = 0, v = nil }
end

function printEdDetails()

  drawText2D("tpos: " .. edGetTopPosition() .. "\n" ..
             "bpos: " .. edGetBottomPosition() .. "\n" ..
             "pos: "  .. edGetPosition() .. "\n" ..
             "lpos: " .. edGetLeftPosition(), 20,98)

end

function lookDown()
  pos = { getCamPos() }
  pos[1] = pos[1] + 10
  lookAt(table.unpack(pos))
  rotateCam(0, math.pi * -0.5)
end
