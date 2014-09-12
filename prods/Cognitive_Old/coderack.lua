-- Name: coderack.lua

coderack = {}
selItems = {}

function updateCoderack()
  selItems = {}
  
  -- select a codelet
  local selection = rouletteSelection(coderack, "importance")
  
  -- if no codelet was selected, don't continue
  if selection.v == nil then return end

  -- run it
  local r1,r2 = coroutine.resume(selection.v.process)
  if not r1 then
    print(r2)
    print(debug.traceback(selection.v.process))
  end
  
  -- remove it if finished
  if coroutine.status(selection.v.process) == "dead" then
    table.remove(coderack, selection.i)
  else
    selection.v.numruns = selection.v.numruns+1
  end
end

function renderCoderack()
  setColorGL(0,255,0,255)
  if selItems[1] ~= nil then
    drawPolygon({x = selItems[1].pos.x,
                 y = selItems[1].pos.y,
                 z = selItems[1].pos.z},
                7,4,math.pi/4,3)
  end
  setColorGL(0,255,255,255)
  if selItems[2] ~= nil then
    drawPolygon({x = selItems[2].pos.x,
                 y = selItems[2].pos.y,
                 z = selItems[2].pos.z},
                7,4,math.pi/4,3)
  end
  
  codeletText = ""
  for i=1,#coderack,1 do
    codeletText = codeletText .. coderack[i].name .. "," ..
                  coroutine.status(coderack[i].process) .. "," ..
                  coderack[i].numruns .. "\n"
  end
  --drawText(codeletText, -10, 10, -10)
end

function newCodelet(name, funct)
  local codelet =
  {
    name         = name,
    funct        = funct,
    importance   = 1,
    numruns      = 0
  }
  codelet.process = coroutine.create(function () codelet:funct() end)
  
  return codelet
end

function generateBottomUpCodelets()
  -- perhaps this should also decide on a selected object to work on
  -- and pass it to the codelet as a parameter.
  
  --addMarkerAdderCodelet(addCircleAdderCodelet)
  --addMarkerAdderCodelet(addSquareAdderCodelet)
  --addMarkerAdderCodelet(addTriangleAdderCodelet)

  --addCircleAdderCodelet(vec2d(-20, -40))
  --addSquareAdderCodelet(vec2d(  0, -40))
  --addTriangleAdderCodelet(vec2d(20, -40))
  
  --addFindMatchingPairCodelet()

  
  for i=1,3,1 do
    --addArrowAdderCodelet()
  end
  
  for i=1,2,1 do
    --addBreakerCodelet()
  end
  
  for i=1,3,1 do
    --addConnectorCodelet()
  end
end
