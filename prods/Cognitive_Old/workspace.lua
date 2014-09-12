-- Name: workspace.lua

workspace = {}
workspace.items = {}

function workspace:addCircle(pos)
  circle = {
    description = "circle",
    pos = pos,
    renderfn = 
      function (self)
        drawCircle(self.pos,5,5)
      end,
    salience = 1 }
  
  table.insert(self.items, circle)
  return circle
end

function workspace:addSquare(pos)
  square = {
    description = "square",
    pos = pos,
    renderfn =
      function (self)
        drawSquare(self.pos,5,5)
      end,
    salience = 1 }
  
  table.insert(self.items,square)
  return square
end

function workspace:addTriangle(pos)
  triangle = {
    description = "triangle",
    pos = pos,
    renderfn = 
      function (self)
        drawTriangle(self.pos,5,5)
      end,
    salience = 1 }
  
  table.insert(self.items,triangle)
  return triangle
end

function workspace:addSpot(pos)
  local spot = {
    description = "marker",
    pos = pos,
    renderfn =
      function (self)
        drawTriangle(self.pos,2,5)
      end,
    salience = 1 }
  
  table.insert(self.items, spot)
  return spot
end

-- child items
-- parent items

function workspace:addBond(item1, item2)
  local bond =
  { description = "bond",
    pos = (item1.pos + item2.pos) * 0.5,
    item1 = item1, item2 = item2,
    renderfn =
      function (self)
        setColorGL(250,250,0,255)
        drawLine(self.item1.pos.x, 0, self.item1.pos.z,
                 self.item1.pos.x, 5, self.item1.pos.z)
        drawLine(self.item1.pos.x, 5, self.item1.pos.z,
                 self.item2.pos.x, 5, self.item2.pos.z)
        drawLine(self.item2.pos.x, 5, self.item2.pos.z,
                 self.item2.pos.x, 0, self.item2.pos.z)
        drawEmptyCircle(self.pos,1)
      end,
    salience = 1 }
    
  table.insert(self.items, bond)
  
  if item1.bonds == nil then item1.bonds = {} end
  if item2.bonds == nil then item2.bonds = {} end
  table.insert(item1.bonds, bond)
  table.insert(item2.bonds, bond)
  
  return bond
end

--workspace:addArrow(workspace.items[1], workspace.items[2])

function workspace:addArrow(item1, item2)
  if item1.arrows ~= nil then
    for i=1,#item1.arrows,1 do
      if item1.arrows[i].item1 == item1 then
        print("arrow already there")
        return
      end
      if item1.arrows[i].item2 == item1 then
        print("arrow already there")
        return
      end
    end
  end
  
  if item2.arrows ~= nil then
    for i=1,#item2.arrows,1 do
      if item2.arrows[i].item1 == item2 then
        print("arrow already there")
        return
      end
      if item2.arrows[i].item2 == item2 then
        print("arrow already there")
        return
      end
    end
  end
  
  local arrow = {
    description = "arrow",
    pos = (item1.pos + item2.pos) * 0.5,
    item1 = item1, item2 = item2,
    renderfn = 
      function (self)
        setColorGL(250,250,0,255)
        
        drawArrow(item1.pos, item2.pos, 8)
        
        --drawArrow(item2, self.markerPos)
        --
        --if item1.removed == true then
        --  setColorGL(0,150,150,255)
        --  item1:renderfn()
        --end
        --if item2.removed == true then
        --  setColorGL(0,150,150,255)
        --  item2:renderfn()
        --end
      end,
    salience = 1 }
    --markerPos =
    --  { x = item2.x + (item2.x - item1.x) * 0.3,
    --    y = item2.y + (item2.y - item1.y) * 0.3,
    --    z = item2.z } }
    
  print("arrow added 2.")
  table.insert(self.items, arrow)
  
  if item1.arrows == nil then item1.arrows = {} end
  if item2.arrows == nil then item2.arrows = {} end
  table.insert(item1.arrows, arrow)
  table.insert(item2.arrows, arrow)
  

  return arrow
end

function makeworkspace()
  for i=10,130,20 do
    for j=10,130,20 do
      local dice = math.random(3)
      if dice == 1 then
        workspace:addCircle(vec2d(i,j))
      elseif dice == 2 then
        workspace:addTriangle(vec2d(i,j))
      else
        workspace:addSquare(vec2d(i,j))
      end
    end
  end
end

makeworkspace()

--workspace:addCircle(vec2d(10,10))
--workspace:addTriangle(vec2d(20,10))
--workspace:addSquare(vec2d(30,10))

--workspace:addCircle(vec2d(10,30))
--workspace:addTriangle(vec2d(20,30))
--workspace:addSquare(vec2d(30,30))

--workspace:addSpot(vec2d(40,10))

--workspace:addBond(workspace.items[1], workspace.items[2])

--workspace:addArrow(workspace.items[1], workspace.items[2])

function updateWorkspace()
  -- codelets can do the selecting
  -- i,workspace.selItem = rouletteSelection(workspace.items, "salience")
end

function renderWorkspace()
  centerheight = 2
  for i=1,#workspace.items,1 do
    setColorGL(250,50,150,255)
    workspace.items[i]:renderfn()
  end
end

-- add "importance" and "unhappiness"
-- "salience" is a function of these 2.
-- what about "strength"? Is there such a thing, or is that for slipnet links?
-- importance is how connected it is
-- unhappiness is how unconnected it is

-- copycat separates bonds from other workspace objects
-- bonds have a strength.

-- ok, don't get too caught up in the details.
-- implement one thing at a time.
