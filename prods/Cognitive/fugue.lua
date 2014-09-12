-- fugue.lua

fugue = {}

dofile("fugue_base.lua")
dofile("fugue_data.lua")

dofile("fugue_compose.lua")
dofile("fugue_render.lua")

--function fugue.update()
--  if g_wrapItUp == 0 then
--    if os.clock() - fugue.lastNoteTime > 5 then
--      --os.exit()
--    end
--  else
--    if os.clock() - fugue.lastNoteTime > fugue.timeBetweenNotes then
--      fugue.noteTick()
--      fugue.lastNoteTime = os.clock()
--    end
--  end
--end

function fugue.update()
  if fugue.currnote + 10 > #fugue.lines[1] then
    --fugue.improvise(1)
  end
end

-- have some notes ready to play
--fugue.improvise(1)
--fugue.improvise(1)
--fugue.improvise(1)

math.randomseed(os.time())

fugue.compose()

--fugue.playing = true
fugue.playing = false

-- this is getting done in prod.lua
-- could be done here as well
--midiStart()
--midiLaunchNextEvent(100)

function fugue.play()
  if fugue.playing == false then
    fugue.playing = true
    midiLaunchNextEvent(100)
  end
end

function fugue.stop()
  fugue.playing = false
end

-- actually, leave rhythms out for now. Lets just do phrases.
-- or, a simpler way would be if the current note is the same as the previous note,
-- don't play.

function fugue.makeMidiNote(note)
  --print2(inspect(note))
  if note == nil then return nil end
  
  -- note is a degree of the scale and an octave
  local midiNote = fugue.currentKey +
                   note.octave * 12 +
                   fugue.minscale[note.degree]
  
  
  return midiNote
end

function midiUpdate()
  -- each line can run identical code.
  midiLaunchNextEvent(50) -- Up to 20 notes per second
end

function midiUpdate()
  if fugue.playing == true then
    -- isn't the last note merely the note before the current one??
    if fugue.currnote <= #fugue.lines[1] then
      if fugue.p1On == true then
        local note = fugue.lines[1][fugue.currnote]
        local midiNote = fugue.makeMidiNote(note)
        local lastNote = fugue.lines[1][fugue.currnote-1]
        local lastNoteMidi = fugue.makeMidiNote(lastNote)
        
        midiSelectInstrument(fugue.inst1)
        
        if lastNoteMidi ~= nil then
          midiNoteOff(lastNoteMidi)
        end
        
        if note.rest == nil then
          --print(fugue.currnote, midiNote)
          
          midiNoteOn(midiNote)
          
          -- play note in the other line.
          
          -- local duration = 100 * fugue.rhythms[2][((fugue.currnote - 1) % 4) + 1]
          -- print(duration)
        end
        
        if note.duration ~= nil then
          midiLaunchNextEvent(note.duration)
        else
          midiLaunchNextEvent(100) -- backward compatibility
        end
        
        fugue.currnote = fugue.currnote + 1
      end
    else
      local lastNote = fugue.lines[1][fugue.currnote-1]
      local lastNoteMidi = fugue.makeMidiNote(lastNote)
      if lastNoteMidi ~= nil then
        midiNoteOff(lastNoteMidi)
      end
      print("no more notes!")
      fugue.playing = false
    end
  end
end


-- try always playing the inverse phrase in the 2nd line.

-- if the 2 lines are going to have separate rhythms, need an event system
-- as the lines are being improvised, an event system traverses them and puts them into
-- an event line, which is what midiUpdate reads.


function fugue.render()
  
  fugue.renderSheet(50, 16)

  fugue.renderGear(80,50,-160, 50, 1, { red = 50, green = 50, blue = 150 + math.random(20) }, 50, 16 )
  fugue.renderGear(80,50,-262, 50, 2, { red = 150 + math.random(20), green = 50, blue = 50 }, 50, 16 )
  
  --fugue.renderDemiurge()
  
end

function fugue.composeAndPlay()
  fugue.compose()
  fugue.play()
end

