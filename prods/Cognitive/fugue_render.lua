
fugue.particles = {}

function fugue.initParticles()
  for i = 1, 100, 1 do
    fugue.particles[i] = {}
    local p = fugue.particles[i]
    p.pos = vec3d(0,0,0)
    p.vel = vec3d(0,0,0)
    p.active = false
  end
end

fugue.initParticles()

function fugue.updateParticles()
  local activationCountdown = 10
  for i = 1,100,1 do
    local p = fugue.particles[i]
    if p.active == true then
      p.pos = p.pos + p.vel * 0.3
      p.vel = p.vel + vec3d(0,-1,0) * 0.2
      if p.pos.y < 0 then
        p.active = false
      end
    else
      if activationCountdown > 0 and g_wrapItUp ~= 0 then
        -- prime the particle
        p.pos = cvec3d(fugue.demiurge[1])
        p.pos = fugue.demiurge[math.random(2)] + vec3d(50,0,0)
        --p.pos.y = 60
        p.vel = vec3d(math.random(20) - 10, math.random(6)-3, math.random(6)-3)
        p.active = true
        activationCountdown = activationCountdown - 1
      end
    end
  end
end

function fugue.renderParticles()
  colorGL(200,180, 90)
  for i = 1,100,1 do
    local p = fugue.particles[i]
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

function fugue.renderDemiurge()
  fugue.updateParticles()
  fugue.updateParticles()
  fugue.updateParticles()
  fugue.renderParticles()
end

function fugue.renderSheet(nTotal, nAhead)
  colorGL(50, 50, 150 + math.random(20))
  beginQuadGL()
  for i = 1, nTotal, 1 do
    local nindex = fugue.currnote - i + nAhead
    if nindex <= #fugue.lines[1] and nindex > 0 then
      local note = fugue.lines[1][nindex]
      if note.isRest == nil then
        local pos = note.degree * 5 + 
                    (note.octave + 2) * 35
        vectorGL(i *     10, pos, 200)
        vectorGL((i+1) * 10, pos, 200)
        vectorGL((i+1) * 10, pos - 5, 200)
        vectorGL(i *     10, pos - 5, 200)
      end
    end
  end
  endGL()
  
  colorGL(150 + math.random(20),50,50)
  beginQuadGL()
  for i = 1, nTotal, 1 do
    local nindex = fugue.currnote - i + nAhead
    if nindex <= #fugue.lines[2] and nindex > 0 then
      local note = fugue.lines[2][nindex]
      if note.isRest == nil then
        local pos = note.degree * 5 + 
                    (note.octave + 2) * 35
        vectorGL(i *     10, pos, 200)
        vectorGL((i+1) * 10, pos, 200)
        vectorGL((i+1) * 10, pos - 5, 200)
        vectorGL(i *     10, pos - 5, 200)
      end
    end
  end
  endGL()
end

-- function fugue.render()
  
  -- renderGear(50,50,50, 20, fugue.gpos)
  -- renderGear(50,50,92, 20, -fugue.gpos+math.pi*0.05)
  -- renderGear(92,50,50, 20, -fugue.gpos+math.pi*0.05)
  
  -- fugue.gpos = fugue.gpos + math.pi*0.01
-- end

fugue.demiurge = {}
fugue.demiurge[1] = vec3d(0,0,0)
fugue.demiurge[2] = vec3d(0,0,0)

function fugue.renderGear(x, y, z, radius, trackNum, noteColor, nTotal, nAhead)
  local nnotes   = nTotal
  local angstep  = (math.pi * 2)/nnotes
  --local angbegin = fugue.currnote * angstep * 1.0 * 0
  local angbegin = nAhead * angstep * -1.0
  if trackNum % 2 == 1 then
    angbegin = angbegin * -1
    angstep = angstep * -1
    angbegin = angbegin + angstep * 0.5 + math.pi
  end
  local c        = vec3d(x,y,z)
  
  colorGL(noteColor.red, noteColor.green, noteColor.blue)
  
  local ang = angbegin

  beginQuadGL()
  for i = 1, nnotes, 1 do
    if i == nAhead then
      colorGL(200, 200, 0)
    else
      colorGL(noteColor.red, noteColor.green, noteColor.blue)
    end
        
    local nindex = fugue.currnote - i + nAhead
    if  nindex <= #fugue.lines[trackNum] and nindex > 0 then
      local note = fugue.lines[trackNum][nindex]
      local npos = note.degree * 2 + 
                   note.octave * 14 + 30
      local p1 = rvec3d(radius-2, ang) + c
      local p2 = rvec3d(radius-2, ang+angstep) + c
      if i == nAhead then
        fugue.demiurge[trackNum] = vec3d(c.x - 50, p1.y + npos - 1.5, p1.z)
        vectorGL(c.x - 50, p1.y + npos - 2, p1.z)
        vectorGL(c.x + 50, p2.y + npos - 2, p2.z)
        vectorGL(c.x + 50, p2.y + npos, p2.z)
        vectorGL(c.x - 50, p1.y + npos, p1.z)
      else
        vectorGL(p1.x, p1.y + npos - 2, p1.z)
        vectorGL(p2.x, p2.y + npos - 2, p2.z)
        vectorGL(p2.x, p2.y + npos, p2.z)
        vectorGL(p1.x, p1.y + npos, p1.z)
      end
    end
    
    ang = ang + angstep
  end
  endGL()
end

