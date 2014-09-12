praxis:

--setClipboardText(getFunction("midiUpdate"))

midiNotesToBePlayed = {}
-- this thing always has 2 items, for the 2 tracks.
-- each item says whether there is a previous note that needs to be turned off.

-- require a "Time to next note" function, which given a line and a current time, returns
-- the time to the next note.

-- like a discrete simulation. that is what I'm implementing
-- except, all the events are known ahead of time.

-- we're converting one representation to another
-- 1) notes and their durations
-- 2) Events

-- keep a current time that gets added to in each midiUpdate call with the previous duration
-- Use that current time to determine whether we need to move to the next note or not in each line
-- there is a "current note" in each line. (That current note can be a rest)


function midiUpdate()
  if fugue.playing == true then
    for i = 1,#midiNotesToBePlayed,1 do
      -- play each note
    end
    -- play the notes that were specified as needing to be played this round,
    -- turning off notes as necessary.
    -- determine which notes need to be played next.
    if fugue.currnote <= #fugue.lines[1] then
      if fugue.p1On == true then
        local note = fugue.lines[1][fugue.currnote]
        local midiNote = fugue.makeMidiNote(note)
        --print(fugue.currnote, midiNote)

        midiSelectInstrument(fugue.inst1)
        midiNoteOff(fugue.lastNote1)
        midiNoteOn(midiNote)
        
        -- play note in the other line.
        
        -- local duration = 100 * fugue.rhythms[2][((fugue.currnote - 1) % 4) + 1]
        -- print(duration)
        
        if note.duration ~= nil then
          midiLaunchNextEvent(note.duration)
        else
          midiLaunchNextEvent(100) -- backward compatibility
        end
        
        fugue.lastNote1 = midiNote
        fugue.currnote = fugue.currnote + 1
      end
    else
      midiNoteOff(fugue.lastNote1)
      print("no more notes!")
      fugue.playing = false
    end
  end
end
