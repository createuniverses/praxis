-- add lua implementations of the intersection functions

l1 = { a = vec3d(0,0,0), b = vec3d(100,0,100) }
l2 = { a = vec3d(70,0,0), b = vec3d(0,0,100) }
l3 = { a = vec3d(70,0,0), b = vec3d(0,0,100) }
c1 = { c = vec3d(0,0,0), r = 80 }
c2 = { c = vec3d(10,0,-40), r = 140 }
c3 = { c = vec3d(150,0,0), r = 100 }

mechangle = 0

mechinits = 0

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

function mech.init()
  setCamPos(50 + math.random(100), 50 + math.random(100), 50 + math.random(100))
  lookAt(75,0,0)
  mechinits = mechinits + 1
end

function mech.render()
  renderLine(l1)
  renderLine(l2)
  renderLine(l3)
  renderCircle(c1)
  renderCircle(c2)
  renderCircle(c3)
end

function mech.update()
  l1.b.x = c1.r * math.sin(mechangle)
  l1.b.z = c1.r * math.cos(mechangle)
  mechangle = mechangle + math.pi * 0.02
  
  l2.a.x = l1.b.x
  l2.a.z = l1.b.z
  
  c2.c.x = l2.a.x
  c2.c.z = l2.a.z
  
  local r = circlecircle(c2,c3)
  
  l2.b.x = r.p1.x
  l2.b.z = r.p1.z
  
  l3.a.x = l2.b.x
  l3.a.z = l2.b.z
  
  l3.b.x = c3.c.x
  l3.b.z = c3.c.z
  
  if mechinits % 2 == 0 then
    setCamPos(l1.b.x, 50, l1.b.z)
    if mechinits % 4 == 0 then
      lookAt(l3.a.x, l3.a.y, l3.a.z)
    else
      lookAt(l3.b.x, l3.b.y, l3.b.z)
    end
  end
end
