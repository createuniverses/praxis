-- gears.lua

function f4Pressed()
  dofile("gears.lua")
end

gears = {}

function addGear(x,y)
  local gear = {}
  gear.pos = vec2d(x,y)
  gear.angle = 0
  gear.height = 0
  
  if #gears >= 1 then
    --local lastGear = findNearestGear(gear.pos)
    --if lastGear == nil then
      lastGear = gears[#gears]
    --end
    p1 = cvec3d(lastGear.pos)
    p2 = cvec3d(gear.pos)
    p1.y = 0
    p2.y = 0
    local dist = Vector3D.magnitude(p2 - p1)
    gear.radius = dist - lastGear.radius
    gear.speed = lastGear.speed * -1 * (lastGear.radius / gear.radius)
    --gear.height = lastGear.height + 10
  else
    gear.radius = 50
    gear.speed = 0.1
  end
  
  table.insert(gears, gear)
  
  if #gears > 10 then
    table.remove(gears,1)
  end
end

function findNearestGear(pos)
  local chosen = nil
  local bestdist = 9999
  for i=1,#gears,1 do
    local dist = Vector3D.magnitude(gears[i].pos - pos)
    if dist > gears[i].radius and dist < bestdist then
      chosen = gears[i]
      bestdist = dist
    end
  end
  return chosen
end

function updateGears()
  for i=1,#gears,1 do
    gears[i].angle = gears[i].angle + gears[i].speed
  end
end

function f4Pressed()
  dofile("gears.lua")
end

function renderGears()
  for i=#gears,#gears - 10, -1 do
    if i > 0 then
      colorGL(255, 255, 255, 255)
      drawPolygon(gears[i].pos, gears[i].radius, 16, gears[i].angle, 15)
    end
  end
  
  renderSelGear()
end

selGear = nil

function renderSelGear()
  if selGear == nil then return end
  
  colorGL(255, 255, 0, 155)
  local pos = cvec3d(selGear.pos)
  --pos.y = i * 10
  drawPolygon(pos, selGear.radius, 16, selGear.angle, 25)
end

gearBots = {}

function addGearBot(x,y,angle)
  local bot = {}
  bot.pos = vec2d(x,y)
  bot.angle = angle
  bot.ticks = 1
  table.insert(gearBots, bot)
end

function updateGearBots()
  for i=1,#gearBots,1 do
    local bot = gearBots[i]
    local spd = 3
    local delta = vec2d(
                    math.sin(bot.angle) * spd,
                    math.cos(bot.angle) * spd)
    bot.pos = bot.pos + delta
    bot.angle = bot.angle + math.pi * 0.005
    
    if bot.ticks % 40 == 0 then
      addGear(bot.pos.x, bot.pos.z)
    end
    bot.ticks = bot.ticks + 1
  end
end

function renderGearBots()
  for i=1,#gearBots,1 do
    local bot = gearBots[i]
    colorGL(255, 55, 55, 255)
    if bot.ticks % 40 < 10 then
      colorGL(255, 255, 55, 255)
    end
    drawSquare(bot.pos, 10, 10)
  end
end

-- a gear placing robot that moves in a circle or a spiral

-- gear placing functions that use an arbitrary gear instead of the last one
-- as the adjacent gear

-- gear placing function that places a gear on the same axle above an existing
-- gear

--addGearBot(0,0,0)
--addGearBot(100,0,math.pi * 0.5)

-- smooth falling after the first 10 gears placed
-- put the height info in the gear

-- if the bot is inside a gear, make a co-axial gear
-- make the gears move epicyclically every so often
