
  --[[
  local scores = {}
  -- red - tonic
  -- green - dominant
  -- blue - subdominant
  -- purple - supertonic
  table.insert(scores, { bases = fugue.tonicbases,       score = 0, concept = "red"    } )
  table.insert(scores, { bases = fugue.dominantbases,    score = 0, concept = "green"  } )
  table.insert(scores, { bases = fugue.subdominantbases, score = 0, concept = "blue"   } )
  table.insert(scores, { bases = fugue.supertonicbases,  score = 0, concept = "purple" } )
  ]]--
  
      --[[
      for ibases = 1,#scores,1 do
        if fugue.isaBase(note, scores[ibases].bases) then
          scores[ibases].score = scores[ibases].score + 1
        end
      end
      ]]--
  
      --[[
      if phrase[i].notetype == fugue.tobase then
        if fugue.isaBase(propnote, chord) == false then
          propnote = fugue.makeNote(note.degree, note.octave)
          local direction = 1
          if jump < 0 then direction = -1 end
          propnote = fugue.findNearestBaseMatch(propnote, chord, direction)
        end
      end
      ]]--
  
 -- look at the phrase just added to the improvisation, 
  -- and call one of the bases generating functions based on that analysis
  
  --[[
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
  ]]--