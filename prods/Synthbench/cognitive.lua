-- cognitive.lua

workspace = {}

function clearWorkspace()
  workspace.items = {}
  do
    for i=1,300,1 do
      workspace.items[i] = { red = 100, green = 100, blue = 150 }
    end
  end

  workspace.cursor = 1
end

clearWorkspace()

function workspace:setColorAtCursor(r,g,b)
  self.items[self.cursor] = { red = r, green = g, blue = b }
  self.cursor = self.cursor + 1
  if self.cursor > #workspace.items then self.cursor = 1 end
end

function updateWorkspace()
end

function renderWorkspace()
  -- render the grid of colors
  --drawLine(0,0,0,100,100,100)
  beginQuadGL()
  local i = 1
  for j=1,17,1 do
    for k=1,17,1 do
      local item = workspace.items[i]
      colorGL(item.red,item.green,item.blue,255)
      local offset = 20
      vectorGL( offset +  j   *10, 5, offset +  k    *10)
      vectorGL( offset + (j+1)*10, 5, offset +  k    *10)
      vectorGL( offset + (j+1)*10, 5, offset + (k+1) *10)
      vectorGL( offset +  j   *10, 5, offset + (k+1) *10)
      i = i + 1
    end
  end
  endGL()
end

coderack = {}

function updateCoderack()
  if #coderack > 0 then
    local i = math.random(#coderack)
    coderack[i]()
    table.remove(coderack,i)
  end
end

codelets = {}

function codelets.makeRed()
  workspace:setColorAtCursor(255,0,0)
end

function codelets.makeGreen()
  workspace:setColorAtCursor(0,255,0)
end

function codelets.makeBlue()
  workspace:setColorAtCursor(0,0,255)
end

function codelets.makePurple()
  workspace:setColorAtCursor(255,0,255)
end

-- codelets need to make higher order structures.

slipnet = {}
slipnet.items = {}
slipnet.items["red"]   =  { name = "red",    codelet = "makeRed"    }
slipnet.items["green"] =  { name = "green",  codelet = "makeGreen"  }
slipnet.items["blue"]  =  { name = "blue",   codelet = "makeBlue"   }
slipnet.items["purple"] = { name = "purple", codelet = "makePurple" }
slipnet.links = {}
slipnet.linkslist = {}
--slipnet.links[1] = { a = "red",    b = "green",  concept = "purple" }
--slipnet.links[2] = { a = "green",  b = "blue",   concept = "red"    }
--slipnet.links[3] = { a = "blue",   b = "purple", concept = "green"  }
--slipnet.links[4] = { a = "purple", b = "red",    concept = "blue"   }
--slipnet.focus = "red"

-- have this running and rendering as I change it!!
-- that is the whole point of live coding after all.

for k,v in pairs(slipnet.items) do
  v.activation = 0
end

function addSliplink(c1,c2,c3)
  if slipnet.links[c1] == nil then slipnet.links[c1] = {} end
  if slipnet.links[c2] == nil then slipnet.links[c2] = {} end
  local link = { a       = slipnet.items[c1],
                 b       = slipnet.items[c2],
                 concept = slipnet.items[c3] }
  slipnet.links[c1][c2] = link
  slipnet.links[c2][c1] = link
  table.insert(slipnet.linkslist, link)
end

function getSliplink(c1,c2)
  if slipnet.links[c1] == nil then return nil end
  return slipnet.links[c1][c2]
end

addSliplink("red",     "green",   "purple")
addSliplink("green",   "blue",    "red")
addSliplink("blue",    "purple",  "green")
addSliplink("purple",  "red",     "blue")

function getAdjacentConcepts(c)
  local adj = {}
  for k,v in pairs(slipnet.links[c]) do
    table.insert(adj, { node       = k,
                        controller = v.concept } )
  end
  return adj
end

print(inspect(getAdjacentConcepts("red")))
print(inspect(slipnet.links.red))

function changeAndClamp(k,c,d)
  c[k] = c[k] + d
  if c[k] < 0 then c[k] = 0 end
  if c[k] > 100 then c[k] = 100 end
end

-- the events can be readily logged and rendered.

-- The trouble is this seems a little pointless.
-- I'm modeling a molecule of thought.
-- I saw the behaviour of "copycat" and I want to capture a slice of that interesting looking behaviour.

function updateSlipnet()
  -- apply decay
  for k,v in pairs(slipnet.items) do
    if math.random(110) < v.activation then
      changeAndClamp("activation",v,-1)
    end
  end
  
  -- spawn codelets
  for k,v in pairs(slipnet.items) do
    if math.random(100) < v.activation then
      -- spawn codelet.
      table.insert(coderack, codelets[v.codelet])
    end
  end
  
  -- transfer activations for active links
  for k,v in pairs(slipnet.linkslist) do
    if math.random(100) < v.concept.activation then
      -- the gate is open
      -- try a -> b
      if math.random(100) < v.a.activation then
        changeAndClamp("activation",v.a,-1)
        changeAndClamp("activation",v.b, 1)
      end
      -- try b -> a
      if math.random(100) < v.b.activation then
        changeAndClamp("activation",v.a, 1)
        changeAndClamp("activation",v.b,-1)
      end
    end
  end
end

function activateConcept(c)
  c.activation = 100
end

slipnet.items["red"].pos       = vec3d(0,0,0)
slipnet.items["green"].pos     = vec3d(20,0,0)
slipnet.items["blue"].pos      = vec3d(20,0,20)
slipnet.items["purple"].pos    = vec3d(0,0,20)

slipnet.items["red"].color     = { r = 255, g = 0,   b = 0,   a = 255 }
slipnet.items["green"].color   = { r = 0,   g = 255, b = 0,   a = 255 }
slipnet.items["blue"].color    = { r = 0,   g = 0,   b = 255, a = 255 }
slipnet.items["purple"].color  = { r = 255, g = 0,   b = 255, a = 255 }

function renderSlipnet()
  for k,v in pairs(slipnet.items) do
    colorGL(v.color.r, v.color.g, v.color.b, v.color.a)
    drawCircle(v.pos, 5,5+v.activation)
  end
end

-- 1) Noticing an object
-- 2) Applying a descriptor to it (which descriptor?)
-- 3) 

-- How about a constructing thing?
-- Not something that reads something, then responds - 
-- that is way too arbitrary
--
-- How about - make a circle? Make a square?
-- You know, like those genetic algorithm tests where
-- the solution was a single number, and the fitness
-- function was the distance away from the number?
-- Nice and simple to imagine and to code.
-- 
-- gap finder
-- its a mechanical process. Trying to make it non-mechanical.

-- In Copycat, a situation is represented by descriptions (the attributes of a given object) and by bonds (the relations between objects). 

-- A turtle moves around the circle in steps
-- At each step, the turtle finds the nearest twig, and tells it to move its end point closer to the current one
-- it'll just be fun to watch this.

