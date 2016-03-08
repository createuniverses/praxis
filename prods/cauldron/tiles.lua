tiles = WidgetLib2.newSimple("tiles")
Widgets["tiles"] = tiles

transform.setTranslation(tiles.lspace, 0,10,0)
airplane.followcam = true

do
tiles.map = {}
tiles.step = 25
tiles.xm = 500
tiles.ym = 500
end

function airplane.getpos()
  local v = vec3d(transform.getTranslation(airplane.lspace))
  v.y = v.z
  return v
end

--continue()

function tiles.etch(p)
  local x = math.floor(p.x / tiles.step) * tiles.step
  local y = math.floor(p.y / tiles.step) * tiles.step
  if tiles.map[x] == nil then
    tiles.map[x] = {}
  end
  tiles.map[x][y] = 1
end

function tiles.getcol(x,y)
  if tiles.map[x] == nil then
    return 0
  end
  if tiles.map[x][y] == nil then
    return 0
  end
  return tiles.map[x][y]
end

do tiles.update = function (o)
  o.etch(airplane.getpos())
end end


do tiles.render = function (o)
  local t = 0
  local step = o.step
  local xm = o.xm
  local ym = o.ym
  glBeginQuads()
  for x=-xm,xm,step do
    for y=-ym,ym,step do
      --if t == 0 then
      if o.getcol(x,y) == 1 then
        glColor(0,0,0,255)
        --glColor(255-y,255 - x,255,255)
      else
        glColor(255,255,255,255)
        glColor(0,0,0,255)
        glColor(255-y,255 - x,255,255)
      end
      vectorGL(x,0,y)
      vectorGL(x+step,0,y)
      vectorGL(x+step,0,y+step)
      vectorGL(x,0,y+step)
      t = (t+1) % 2
    end
  end
  glEnd()
end end


