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

praxis:
function rouletteSelection(t,f,tr)
  total = 0
  for i=1,#t,1 do
    --total = total + (t[i][f] * t[i][f])
    total = total + tr(t[i][f])
  end
  if total <= 0 then return { i = 0, v = nil } end
  ball = math.random(total)
  cumtotal = 0
  for i=1,#t,1 do
    cumtotal = cumtotal + tr(t[i][f])
    if cumtotal >= ball then
      return { i = i, v = t[i] }
    end
  end
  return { i = 0, v = nil }
end

function roulTest()
  local slices = { 1,2,3,4,5 }
  local t = {}
  for i = 1,#slices,1 do
    t[i] = {}
    t[i].slice = slices[i]
    t[i].cnt = 0
  end
  for i = 1,1000,1 do
    local r = rouletteSelection(t, "slice", function (x) return x end)
    if r.v ~= nil then
      r.v.cnt = r.v.cnt + 1 end
  end
  setClipboardText(inspect(t))
end

roulTest()
praxis:
roulTest()


{ {
    cnt = 64,
    slice = 1
  }, {
    cnt = 125,
    slice = 2
  }, {
    cnt = 214,
    slice = 3
  }, {
    cnt = 263,
    slice = 4
  }, {
    cnt = 334,
    slice = 5
  } }

{ {
    cnt = 69,
    slice = 1
  }, {
    cnt = 127,
    slice = 2
  }, {
    cnt = 190,
    slice = 3
  }, {
    cnt = 267,
    slice = 4
  }, {
    cnt = 347,
    slice = 5
  } }

{ {
    cnt = 75,
    slice = 1
  }, {
    cnt = 266,
    slice = 2
  }, {
    cnt = 587,
    slice = 3
  }, {
    cnt = 72,
    slice = 4
  }, {
    cnt = 0,
    slice = 5
  } }

{ {
    cnt = 20,
    slice = 1
  }, {
    cnt = 67,
    slice = 2
  }, {
    cnt = 187,
    slice = 3
  }, {
    cnt = 298,
    slice = 4
  }, {
    cnt = 428,
    slice = 5
  } }
  { {
    cnt = 26,
    slice = 1
  }, {
    cnt = 96,
    slice = 2
  }, {
    cnt = 275,
    slice = 3.4
  }, {
    cnt = 587,
    slice = 5
  } }
  nil

praxis:
clearError()



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
