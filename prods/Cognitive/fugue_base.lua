
function fugue.makeNote(degree, octave)
  local note = { degree = degree, octave = octave }
  return note
end

function fugue.cloneNote(note)
  return fugue.makeNote(note.degree, note.octave)
end


function fugue.stepNote(note, numSteps)
  -- note is a degree of the scale and an octave
  local newNote = fugue.cloneNote(note)
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

function fugue.isaBase(note, bases)
  for i=1,#bases,1 do
    if note.degree == bases[i].degree then
      return true
    end
  end
  return false
end

function fugue.findNearestBaseMatch(note, bases, direction)
  newNote = fugue.makeNote(note.degree, note.octave)
  for i=1,8,1 do
    newNote = fugue.stepNote(newNote, direction)
    if fugue.isaBase(newNote, bases) then
      return newNote
    end
  end
  return note
end

--------------------

-- generating a random sequence
function fugue.generateRandomSequence(numTerms, min, max)
  local seq = {}
  
  for i=1,numTerms,1 do
    max = max - (numTerms - i)
    local range = max - min
    seq[i] = math.random(range) + min
  end
  
  return seq
end
