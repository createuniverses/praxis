-- add lua implementations of the intersection functions

l1 = { a = vec3d(0,0,0), b = vec3d(100,0,100) }
l2 = { a = vec3d(70,0,0), b = vec3d(0,0,100) }
c1 = { c = vec3d(10,0,40), r = 50 }
c2 = { c = vec3d(10,0,-40), r = 50 }

function renderLine(l)
  drawLine(l.a.x, l.a.y, l.a.z,
           l.b.x, l.b.y, l.b.z)
end

function renderCircle(c)
  movePen(c.c.x + c.r * math.sin(0), 0, c.c.z + c.r * math.cos(0))
  for i=0,math.pi * 2,math.pi * 0.05 do
    drawTo(c.c.x + c.r * math.sin(i), 0, c.c.z + c.r * math.cos(i))
  end
end

function renderPoint(p)
  drawLine(p.x, p.y-20, p.z,
           p.x, p.y+20, p.z)
end

function render()
  renderLine(l1)
  renderLine(l2)
  renderCircle(c1)
  renderCircle(c2)
  local r = lineline(l1, l2)
  renderPoint(r.p1)
  r = linecircle(l1,c1)
  renderPoint(r.p1)
  
  r = circlecircle(c1,c2)
  renderPoint(r.p1)
  renderPoint(r.p2)
  --print(r.status)
end

clearTrace()
