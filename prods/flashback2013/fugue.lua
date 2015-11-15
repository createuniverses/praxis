-- fugue.lua

fugue = {}

fugue.minscale = { 0, 2, 3, 5, 7, 8, 11, 12 }
fugue.majscale = { 0, 2, 4, 5, 7, 9, 11, 12 }

fugue.currentKey = 60 -- C

fugue.lastNoteTime = 0
--fugue.timeBetweenNotes = 0.5
fugue.timeBetweenNotes = 0.15
fugue.currnote = 1

fugue.improvisation = {}
fugue.improvisation.lines = { {}, {}, {} } -- 2, no 3 parts

g_wrapItUp = 2

function fugue.makeNote(degree, octave)
  local note = { degree = degree, octave = octave }
  return note
end

function fugue.stepNote(note, numSteps)
  -- note is a degree of the scale and an octave
  local newNote = fugue.makeNote(note.degree, note.octave)
  newNote.degree = newNote.degree + numSteps
  
  while newNote.degree < 1 do
    newNote.degree = newNote.degree + 7
    newNote.octave = newNote.octave - 1
  end
  
  while newNote.degree > 7 do
    newNote.degree = newNote.degree - 7
    newNote.octave = newNote.octave + 1
  end
  
  return newNote
end

function fugue.makeMidiNote(note)
  -- note is a degree of the scale and an octave
  local midiNote = fugue.currentKey +
                   note.octave * 12 +
                   fugue.minscale[note.degree]
  
  
  return midiNote
end

fugue.baseDegree = 1
fugue.baseOctave = -1

--fugue.phrased = { 0, -1, -1, -1, -1, -1, -1, -1}

--fugue.bases = {
--  fugue.makeNote(5,0),
--  fugue.makeNote(7,0),
--  fugue.makeNote(2,1) }

-- Different constraints
-- phrase entries are:
-- up to a base note
-- down to a base note
-- up to any note
-- down to any note

fugue.tobase = 1
fugue.toany = 2

function newPhraseNote(notetype, jump)
  return { notetype = notetype, jump = jump }
end

function useTonicBases()
  fugue.bases = {
    fugue.makeNote(1,0),
    fugue.makeNote(3,0),
    fugue.makeNote(5,0) }
  fugue.perturbs = 7
  fugue.phrase = {
    newPhraseNote(fugue.tobase, 0), 
    newPhraseNote(fugue.toany, 2), 
    newPhraseNote(fugue.tobase, 2), 
    newPhraseNote(fugue.toany, -2), 
    newPhraseNote(fugue.tobase, -2), 
    newPhraseNote(fugue.toany, 2), 
    newPhraseNote(fugue.tobase, 2), 
    newPhraseNote(fugue.toany, -2) } 
    
  fugue.rests1 = { 1,1,0,1,0,1,1,1 }
  fugue.rests2 = { 1,0,1,0,1,0,0,1 }
  fugue.rests1 = { 1,1,1,1,1,1,1,1 }
  fugue.rests2 = { 1,1,1,1,1,1,1,1 }
end

function useSupertonicBases()
  fugue.bases = {
    fugue.makeNote(2,0),
    fugue.makeNote(4,0),
    fugue.makeNote(6,1) }
  fugue.perturbs = 2
  fugue.phrase = {
    newPhraseNote(fugue.toany, 0), 
    newPhraseNote(fugue.toany, 1), 
    newPhraseNote(fugue.toany, 1), 
    newPhraseNote(fugue.toany, 2), 
    newPhraseNote(fugue.toany, -1), 
    newPhraseNote(fugue.toany, -1), 
    newPhraseNote(fugue.toany, 1), 
    newPhraseNote(fugue.toany, 1) } 
  --fugue.phrased = { 0, -1, -1, -1, -1, -1, -1, -1}
  
  fugue.rests1 = { 1,1,1,1,1,1,1,1 }
  fugue.rests2 = { 1,1,1,1,1,1,1,1 }
end

function useDominantBases()
  fugue.bases = {
    fugue.makeNote(5,0),
    fugue.makeNote(7,0),
    fugue.makeNote(2,1) }
  fugue.perturbs = 2
  fugue.phrase = {
    newPhraseNote(fugue.toany, 0), 
    newPhraseNote(fugue.toany, -1), 
    newPhraseNote(fugue.toany, -1), 
    newPhraseNote(fugue.toany, -1), 
    newPhraseNote(fugue.toany, -1), 
    newPhraseNote(fugue.toany, -1), 
    newPhraseNote(fugue.toany, -1), 
    newPhraseNote(fugue.toany, -1) } 
  --fugue.phrased = { 0, -1, -1, -1, -1, -1, -1, -1}
  
  fugue.rests1 = { 1,1,1,1,1,1,1,1 }
  fugue.rests2 = { 1,1,1,1,1,1,1,1 }
end

function useSubdominantBases()
  fugue.bases = {
    fugue.makeNote(4,0),
    fugue.makeNote(6,0),
    fugue.makeNote(1,1) }
  
  fugue.perturbs = 7
  fugue.phrase = {
    newPhraseNote(fugue.tobase, 0), 
    newPhraseNote(fugue.toany, 2), 
    newPhraseNote(fugue.tobase, 2), 
    newPhraseNote(fugue.toany, -2), 
    newPhraseNote(fugue.tobase, -2), 
    newPhraseNote(fugue.toany, 2), 
    newPhraseNote(fugue.tobase, 2), 
    newPhraseNote(fugue.toany, -2) } 
    
  fugue.rests1 = { 1,0,0,1,1,1,1,1 }
  fugue.rests2 = { 1,0,1,0,1,0,0,1 }
  fugue.rests1 = { 1,1,1,1,1,1,1,1 }
  fugue.rests2 = { 1,1,1,1,1,1,1,1 }
end

  fugue.perturbs = 2
  fugue.phrase = {
    newPhraseNote(fugue.toany, 0), 
    newPhraseNote(fugue.tobase, -1), 
    newPhraseNote(fugue.toany, -1), 
    newPhraseNote(fugue.tobase, 1), 
    newPhraseNote(fugue.toany, 2), 
    newPhraseNote(fugue.toany, 1), 
    newPhraseNote(fugue.tobase, -1), 
    newPhraseNote(fugue.toany, -1) } 
    
-- consider multiple passes instead of single pass
-- need an introspective part that looks at the phrase generated
-- decides on the harmonic direction based on is
-- those "any" notes will determine what the next harmony should be
-- predominantly 
function isaBase(note, bases)
  for i=1,#bases,1 do
    if note.degree == bases[i].degree then
      return true
    end
  end
  return false
end

function findNearestBaseMatch(note, bases, direction)
  newNote = fugue.makeNote(note.degree, note.octave)
  for i=1,8,1 do
    newNote = fugue.stepNote(newNote, direction)
    if isaBase(newNote, bases) then
      return newNote
    end
  end
  return note
end

function fugue.moveBaseDegree()
  fugue.baseDegree = fugue.baseDegree + 1
  if fugue.baseDegree > #fugue.bases then
    fugue.baseDegree = 1
    fugue.baseOctave = fugue.baseOctave + 1
    if fugue.baseOctave > 1 then
      fugue.baseOctave = -1
    end
  end
end

  fugue.tonicbases = {
    fugue.makeNote(1,0),
    fugue.makeNote(3,0),
    fugue.makeNote(5,1) }
  fugue.dominantbases = {
    fugue.makeNote(5,0),
    fugue.makeNote(7,0),
    fugue.makeNote(2,1) }
  fugue.subdominantbases = {
    fugue.makeNote(4,0),
    fugue.makeNote(6,0),
    fugue.makeNote(1,1) }
  fugue.supertonicbases = {
    fugue.makeNote(2,0),
    fugue.makeNote(4,0),
    fugue.makeNote(6,0) }

function fugue.improvise(part)

  local scores = {}
  -- red - tonic
  -- green - dominant
  -- blue - subdominant
  -- purple - supertonic
  table.insert(scores, { bases = fugue.tonicbases,       score = 0, concept = "red"    } )
  table.insert(scores, { bases = fugue.dominantbases,    score = 0, concept = "green"  } )
  table.insert(scores, { bases = fugue.subdominantbases, score = 0, concept = "blue"   } )
  table.insert(scores, { bases = fugue.supertonicbases,  score = 0, concept = "purple" } )

  local baseDegree = fugue.bases[fugue.baseDegree].degree
  local baseOctave = fugue.bases[fugue.baseDegree].octave + fugue.baseOctave
  
  local perturbs = math.random(fugue.perturbs + 1) - 1
  
  local note = fugue.makeNote(baseDegree, baseOctave)
  for i = 1, #fugue.phrase, 1 do
    local jump = fugue.phrase[i].jump
    if perturbs > 0 then
      if math.random(2) == 1 then
        jump = jump * -1
        perturbs = perturbs - 1
      end
    end
    
    local propnote = fugue.stepNote(note, jump)
    
    if fugue.phrase[i].notetype == fugue.tobase then
      if isaBase(propnote, fugue.bases) == false then
        propnote = fugue.makeNote(note.degree, note.octave)
        local direction = 1
        if jump < 0 then direction = -1 end
        propnote = findNearestBaseMatch(propnote, fugue.bases, direction)
      end
    end
    
    note = fugue.makeNote(propnote.degree, propnote.octave)
    
    for ibases = 1,#scores,1 do
      if isaBase(note, scores[ibases].bases) then
        scores[ibases].score = scores[ibases].score + 1
      end
    end
    
    table.insert(part, note)
  end
  
  -- look at the phrase just added to the improvisation, 
  -- and call one of the bases generating functions based on that analysis
  
  local totalscore = 0
  for ibases = 1,#scores,1 do
    totalscore = totalscore + scores[ibases].score
  end
  
  local rouletteball = math.random(totalscore)
  local rouletteslot = 0
  local cumulativescore = 0
  for ibases = 1,#scores,1 do
    cumulativescore = cumulativescore + scores[ibases].score
    if cumulativescore >= rouletteball and rouletteslot == 0 then
      rouletteslot = ibases
    end
  end
  
  -- print("totalscore: " .. totalscore)
  -- print("rouletteball: " .. rouletteball)
  -- print("rouletteslot: " .. rouletteslot)
  bumpActivation(slipnet.items[scores[rouletteslot].concept], g_slipnetBumpRate)
  
end

g_slipnetDecayRate = 3
g_slipnetBumpRate = 25

useTonicBases()

--fugue.perturbs = 2
fugue.inst1 = 0
fugue.inst2 = 10

fugue.pfreq1 = 45
fugue.pfreq2 = 30

-- fugue.rests1 = { 1,0,0,1,1,1,1,1 }
-- fugue.rests2 = { 1,0,1,0,1,0,0,1 }

fugue.p1On = true
fugue.p2On = true

function fugue.playForwardAndImprovise()
  -- see how much time has elapsed
  -- play the next note.
  if fugue.currnote <= #fugue.improvisation.lines[1] then
    --print("Playing " .. midiNote)
    -- a cheap way of getting rhythms is to randomly drop notes
    
    --if math.random(60) < fugue.pfreq1 then
    
    --if fugue.p1On == true and fugue.rests1[fugue.currnote % 8 + 1] == 1 then
    if math.random(16) < 15 then
      local midiNote1 = fugue.makeMidiNote(fugue.improvisation.lines[1][fugue.currnote])
      midiSelectInstrument(fugue.inst1)
      midiNoteOn(midiNote1)
    end
    
    --if math.random(60) < fugue.pfreq2 then
    --if fugue.p2On == true and fugue.rests2[fugue.currnote % 8 + 1] == 1 then
    if math.random(16) < 15 then
      local midiNote2 = fugue.makeMidiNote(fugue.improvisation.lines[2][fugue.currnote])
      midiSelectInstrument(fugue.inst2)
      midiNoteOn(midiNote2)
    end
    --local midiNote3 = fugue.makeMidiNote(fugue.improvisation.lines[2][fugue.currnote])
    --midiNoteOn(midiNote3)
    --local midiNote4 = fugue.makeMidiNote(fugue.improvisation.lines[2][fugue.currnote])
    --midiNoteOn(midiNote4)
    
    fugue.currnote = fugue.currnote + 1
  end
  
  if fugue.currnote > #fugue.improvisation.lines[1] then
    -- do some composing
    fugue.improvise(fugue.improvisation.lines[1])
    fugue.moveBaseDegree()
    fugue.moveBaseDegree()
    fugue.moveBaseDegree()
    fugue.improvise(fugue.improvisation.lines[2])
    fugue.moveBaseDegree()
    --fugue.improvise(fugue.improvisation.lines[3])
    --fugue.improvise(fugue.improvisation.lines[4])
    
    -- local nextThing = math.random(4)
    
    -- if nextThing == 1 then
      -- print("I")
      -- useTonicBases()
    -- elseif nextThing == 2 then
      -- print("V")
      -- useDominantBases()
    -- elseif nextThing == 3 then
      -- print("IV")
      -- useSubdominantBases()
    -- else
      -- print("II")
      -- useSupertonicBases()
    -- end
    
    if g_wrapItUp < 2 then
      print("Wrapping it up, using tonic")
      useTonicBases()
    else
      --updateSlipnet()
      updateCoderack()
    end
    
    for i = 1, 4, 1 do
      local jopt = math.random(2)
      if jopt == 1 then
        fugue.phrase[i].jump = 1
      else
        fugue.phrase[i].jump = -1
      end
      fugue.phrase[i].notetype = fugue.tobase
    end
    
    for i = 5, 8, 1 do
      local jopt = math.random(2)
      if jopt == 1 then
        fugue.phrase[i].jump = 1
      else
        fugue.phrase[i].jump = -1
      end    
      fugue.phrase[i].notetype = fugue.toany
    end
    
    if math.random(2) == 1 then
      --fugue.p2On = true
    else
      --fugue.p2On = false
    end
  end
end


function fugue.playBackward()
  if fugue.currnote > 0 then
    local midiNote = fugue.makeMidiNote(fugue.improvisation[fugue.currnote])
    --print("Playing " .. midiNote)
    midiNoteOn(midiNote)
    
    fugue.currnote = fugue.currnote - 1
  else
    fugue.currnote = #fugue.improvisation
  end
end

function fugue.update()
  if g_wrapItUp == 0 then
    if os.clock() - fugue.lastNoteTime > 5 then
      os.exit()
    end
  else
    if os.clock() - fugue.lastNoteTime > fugue.timeBetweenNotes then
      fugue.noteTick()
      fugue.lastNoteTime = os.clock()
    end
  end
end

fugue.noteNum = 1

function fugue.noteTick()
  
  if fugue.noteNum < 450 then
    fugue.playForwardAndImprovise()
    --fugue.playBackward()
    updateSlipnet()
    
    fugue.noteNum = fugue.noteNum + 1
  else
    print("Wrap it up being set to 0")
    g_wrapItUp = 0
  end
  
  if g_wrapItUp == 2 and fugue.noteNum > 400 then
    g_wrapItUp = 1
  end
end

  -- render the improvisation
  -- as a spiral moving outward, perturbed by the note selection
  -- as a cylinder of notes, with only the most recent n notes displayed
  -- a helix
  -- gears
  -- meshing, interlocking gears as the harmonising notes
  
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

  function fugue.render()
  
  colorGL(50, 50, 150 + math.random(20))
  beginQuadGL()
  for i = 1, 50, 1 do
    local nindex = fugue.currnote - i
    if nindex <= #fugue.improvisation.lines[1] and nindex > 0 then
      local pos = fugue.improvisation.lines[1][nindex].degree * 5 + 
                  fugue.improvisation.lines[1][nindex].octave * 50
      vectorGL(i *     10, pos, 200)
      vectorGL((i+1) * 10, pos, 200)
      vectorGL((i+1) * 10, pos - 10, 200)
      vectorGL(i *     10, pos - 10, 200)
    end
  end
  endGL()
  
  colorGL(150 + math.random(20),50,50)
  local nnotes = #fugue.improvisation.lines[1]
  beginQuadGL()
  for i = 1, 50, 1 do
    local nindex = fugue.currnote - i
    if nindex <= #fugue.improvisation.lines[2] and nindex > 0 then
      local pos = fugue.improvisation.lines[2][nindex].degree * 5 + 
                  fugue.improvisation.lines[2][nindex].octave * 50
      vectorGL(i *     10, pos, 200)
      vectorGL((i+1) * 10, pos, 200)
      vectorGL((i+1) * 10, pos - 10, 200)
      vectorGL(i *     10, pos - 10, 200)
    end
  end
  endGL()

  renderGear(80,50,-160, 50, 1)
  renderGear(80,50,-262, 50, 2)
  
  fugue.renderDemiurge()
  
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

function renderGear(x, y, z, radius, trackNum)
  local nnotes   = 50
  local angstep  = (math.pi * 2)/nnotes
  local angbegin = fugue.currnote * angstep * 1.0 * 0
  if trackNum % 2 == 1 then
    angbegin = angbegin * -1
    angstep = angstep * -1
    angbegin = angbegin + angstep * 0.5 + math.pi
  end
  local c        = vec3d(x,y,z)
    
  if trackNum % 2 == 1 then
    colorGL(50, 50, 150 + math.random(20))
  else
    colorGL(150 + math.random(20),50,50)
  end
  local ang = angbegin

  beginQuadGL()
  for i = 1, nnotes, 1 do
    if i == 1 then
      colorGL(200, 200, 0)
    else
      if trackNum % 2 == 1 then
        colorGL(50, 50, 150 + math.random(20))
      else
        colorGL(150 + math.random(20),50,50)
      end
    end
        
    local nindex = fugue.currnote - i
    if  nindex <= #fugue.improvisation.lines[trackNum] and nindex > 0 then
      local npos = fugue.improvisation.lines[trackNum][nindex].degree * 2 + 
                   fugue.improvisation.lines[trackNum][nindex].octave * 20 + 30
      local p1 = rvec3d(radius-2, ang) + c
      local p2 = rvec3d(radius-2, ang+angstep) + c
      if i == 1 then
        fugue.demiurge[trackNum] = vec3d(c.x - 50, p1.y + npos - 1.5, p1.z)
        vectorGL(c.x - 50, p1.y + npos - 3, p1.z)
        vectorGL(c.x + 50, p2.y + npos - 3, p2.z)
        vectorGL(c.x + 50, p2.y + npos, p2.z)
        vectorGL(c.x - 50, p1.y + npos, p1.z)
      else
        vectorGL(p1.x, p1.y + npos - 3, p1.z)
        vectorGL(p2.x, p2.y + npos - 3, p2.z)
        vectorGL(p2.x, p2.y + npos, p2.z)
        vectorGL(p1.x, p1.y + npos, p1.z)
      end
    end
    
    ang = ang + angstep
  end
  endGL()
end

