-- add lua implementations of the intersection functions

l1 = { a = vec3d(0,0,0), b = vec3d(100,0,100) }
l2 = { a = vec3d(70,0,0), b = vec3d(0,0,100) }
l3 = { a = vec3d(70,0,0), b = vec3d(0,0,100) }
c1 = { c = vec3d(0,0,0), r = 80 }
c2 = { c = vec3d(10,0,-40), r = 140 }
c3 = { c = vec3d(150,0,0), r = 100 }

mechangle = 0

mechinits = 0

locus = {}

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

function renderLocus(l)
  if #locus < 2 then
    return
  end
  
  movePen(locus[1].x, locus[1].y, locus[1].z)
  for i=2,#locus,1 do
    drawTo(locus[i].x, locus[i].y, locus[i].z)
  end
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
  
  do --if mechinits % 2 == 1 then
    renderLocus(l)
    renderParticles()
  end
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
  
  -- now, locus
  local cl1 = { c = cvec3d(l2.a), r = 80 }
  local cl2 = { c = cvec3d(l2.b), r = 80 }
  
  r = circlecircle(cl1, cl2)
  
  table.insert(locus, r.p1)
  
  updateParticles()
  
  if mechinits % 2 == 0 then
    setCamPos(l1.b.x, 50, l1.b.z)
    if mechinits % 4 == 0 then
      lookAt(l3.a.x, l3.a.y, l3.a.z)
    else
      lookAt(l3.b.x, l3.b.y, l3.b.z)
    end
  else
    lookAt(locus[#locus].x, locus[#locus].y, locus[#locus].z)
  end
  
end

particles = {}

function initParticles()
  for i = 1, 100, 1 do
    particles[i] = {}
    local p = particles[i]
    p.pos = vec3d(0,0,0)
    p.vel = vec3d(0,0,0)
    p.active = false
  end
end

initParticles()

function updateParticles()
  local activationCountdown = 10
  for i = 1,100,1 do
    local p = particles[i]
    if p.active == true then
      p.pos = p.pos + p.vel * 0.3
      p.vel = p.vel + vec3d(0,-1,0) * 0.2
      if p.pos.y < 0 then
        p.active = false
      end
    else
      if activationCountdown > 0 and g_wrapItUp ~= 0 then
        -- prime the particle
        p.pos = cvec3d(locus[#locus])
        --p.pos.y = 60
        p.vel = vec3d(math.random(20) - 10, math.random(6)-3, math.random(6)-3)
        p.active = true
        activationCountdown = activationCountdown - 1
      end
    end
  end
end

function renderParticles()
  colorGL(200,180, 90)
  for i = 1,100,1 do
    local p = particles[i]
	if p.active == true then
		drawLine(p.pos.x,
				 p.pos.y,
				 p.pos.z,
				 p.pos.x + p.vel.x,
				 p.pos.y + p.vel.y,
				 p.pos.z + p.vel.z)
    end
  end
end
