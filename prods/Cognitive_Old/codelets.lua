-- Name: codelets.lua

function addCodelet(name, funct)
  local codelet = newCodelet(name, funct)
  table.insert(coderack, codelet)
  return codelet
end

function addShapeAdderCodelet(shapeAdderFn, marker)
  addCodelet("shapeAdder",
    function (self)
      shapeAdderFn(workspace, marker)
    end)
end

function addCircleAdderCodelet(marker)
  addShapeAdderCodelet(workspace.addCircle, marker)
end

function addSquareAdderCodelet(marker)
  addShapeAdderCodelet(workspace.addSquare, marker)
end

function addTriangleAdderCodelet(marker)
  addShapeAdderCodelet(workspace.addTriangle, marker)
end

function addArrowAdderCodelet()
  addCodelet("arrowAdder",
    function (self)
      selItems[1] = rouletteSelection(workspace.items, "salience").v
      selItems[2] = rouletteSelection(workspace.items, "salience").v
      if selItems[1] == nil or selItems[2] == nil or selItems[1] == selItems[2] then
        selItems = {}
        return
      end
      workspace:addArrow(selItems[1], selItems[2])
    end)
end

lastSpotPos = vec2d(0,0)

function addMarkerAdderCodelet(followUpCodeletAdderFn)
  -- local spotPos = vec2d(math.random(10)*10, math.random(10)*10)
  -- place the markers in a regular grid.
  local spotPos = vec2d(lastSpotPos.x, lastSpotPos.y)
  spotPos.x = spotPos.x + 10
  if spotPos.x > 100 then
    spotPos.y = spotPos.y + 10
    spotPos.x = 0
  end
  lastSpotPos = vec2d(spotPos.x, spotPos.y)
  addCodelet("markerAdder",
    function (self)
      if #workspace.items > 30 then return end
      local spot = workspace:addSpot(spotPos)
      coroutine.yield()
      followUpCodeletAdderFn(spot)
    end)
end

function addBreakerCodelet()
  addCodelet("breaker",
    function (self)
      local selection = rouletteSelection(workspace.items, "salience")
      if selection.v == nil then return end
      selItems[1] = selection.v
      selection.v.removed = true
      table.remove(workspace.items, selection.i)
    end)
end

function addFindMatchingPairCodelet()
  addCodelet("pairfinder",
    function (self)
      local lselItems = {}
      lselItems[1] = rouletteSelection(workspace.items, "salience").v
      -- the second item should be an item near this selected item
      lselItems[2] = rouletteSelection(workspace.items, "salience").v
      selItems[1] = lselItems[1]
      selItems[2] = lselItems[2]
      --coroutine.yield()
      if lselItems[1] == nil or lselItems[2] == nil or lselItems[1] == lselItems[2] then
        selItems = {}
        return
      end
      --selItems[1] = lselItems[1]
      --selItems[2] = lselItems[2]
      --coroutine.yield()
      --selItems[1].salience = selItems[1].salience + 1
      --selItems[2].salience = selItems[2].salience + 1
      if lselItems[1].description == lselItems[2].description then
        print("Found a pair of " .. lselItems[1].description)
        workspace:addArrow(lselItems[1], lselItems[2])
        -- add someting to indicate a bond has been forged.
      end
      selItems[1] = lselItems[1]
      selItems[2] = lselItems[2]
      coroutine.yield()
      selItems[1] = lselItems[1]
      selItems[2] = lselItems[2]
    end)
end

-- 1. Scout codelet
-- 2. Strength tester codelet
-- 3. Structure builder codelet

-- these are the sorts of things that are built.
-- a vocabulary, a reportoire of structures
-- structures built on top of structures
-- a structure is proposed
-- why is it proposed?
-- a scout. A scout looks to build a specific sort of structure.
-- top down scout

function topDownSquareScout()
end

function bottomUpBondScout()
end
