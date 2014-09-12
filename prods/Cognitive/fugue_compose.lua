
-- need a proper event based system. a note consists of a note on
-- event and a note off event.
-- the system waits for the next event time, and it carries it out.

-- use the multimedia timer for midi.
-- need to be able to place midi events at will, and have the system
-- know at any time which event is next.
-- at the end of the multimedia timer, a lua function is called

-- insertNote inserts 2 midi events. a note on event and a note off
-- event

-- it also may add an event to turn off the previous note
-- there may be a separate data structure with the actual notes
-- and durations listed.

-- 2 data structures? One for the midi events, another for the
-- set of notes (pitch and duration)

-- multi tiered AI
-- a phrase paster
-- a phrase selector
-- 

function fugue.pastePhrase(phrase, line, root)
  local note = fugue.cloneNote(root)
  table.insert(line, note)
  for i = 1, #phrase.shape, 1 do
    note = fugue.stepNote(note, phrase.shape[i])
    table.insert(line, note)
  end
end

fugueUtil = {}

function fugueUtil.totalDistance(phraseSeq)
  local distance = 0
  for i=1,#phraseSeq,1 do
    distance = distance + fugue.phrases[phraseSeq[i]].destination
  end
  return distance
end

function fugueUtil.getCandidates(tbl, rule)
  local candidates = {}
  for i=1,#tbl,1 do
    if rule(tbl[i]) == true then
      table.insert(candidates, i)
    end
  end
  return candidates
end

function fugueUtil.randomEntry(tbl)
  return tbl[math.random(#tbl)]
end

-- want to test this individually before implementing more.
function fugueUtil.applyDeepenNegativePass(phraseSeq)
  -- find an existing negative and make it bigger
  local negatives = fugueUtil.getCandidates(phraseSeq,
    function (i)
      if fugue.phrases[i].destination < 0 and
         fugue.phrases[i].destination > -4 then -- because the options atm are -2 and -4
        return true else return false end
    end)
  
  local bigNegatives = fugueUtil.getCandidates(fugue.phrases,
    function (i)
      if i.destination < -2 then
        return true else return false end
    end)
  
  
  -- return all the intermediate stuff too?
  if #negatives >= 1 and #bigNegatives >= 1 then
    local negativesEntry = fugueUtil.randomEntry(negatives)
    local bigNegativesEntry = fugueUtil.randomEntry(bigNegatives)
    phraseSeq[negativesEntry] = bigNegativesEntry
    
    return { negatives          = negatives,
             bigNegatives       = bigNegatives,
             negativesEntry     = negativesEntry,
             bigNegativesEntry  = bigNegativesEntry }
  end
  
  return nil
end

function fugueUtil.applyHeightenPositivePass(phraseSeq)
  -- find an existing negative and make it bigger
  local positives = fugueUtil.getCandidates(phraseSeq,
    function (i)
      if fugue.phrases[i].destination > 0 and
         fugue.phrases[i].destination < 4 then -- because the options atm are 2 and 4
        return true else return false end
    end)
  
  local bigPositives = fugueUtil.getCandidates(fugue.phrases,
    function (i)
      if i.destination > 2 then
        return true else return false end
    end)
    
  if #positives >= 1 and #bigPositives >= 1 then
    --phraseSeq[fugueUtil.randomEntry(negatives)] = fugueUtil.randomEntry(bigNegatives)
    
    local positivesEntry = fugueUtil.randomEntry(positives)
    local bigPositivesEntry = fugueUtil.randomEntry(bigPositives)
    phraseSeq[positivesEntry] = bigPositivesEntry
    
    return { positives          = positives,
             bigPositives       = bigPositives,
             positivesEntry     = positivesEntry,
             bigPositivesEntry  = bigPositivesEntry }
  end
  
  return nil
end

function fugueUtil.replacePhrases(phraseSeq, phraseRule, dbPhraseRule)

  local outgoing = fugueUtil.getCandidates(phraseSeq, phraseRule)
  local incoming = fugueUtil.getCandidates(fugue.phrases, dbPhraseRule)
  
  if #outgoing >= 1 and #incoming >= 1 then
    local outgoingEntry = fugueUtil.randomEntry(outgoing)
    local incomingEntry = fugueUtil.randomEntry(incoming)
    phraseSeq[outgoingEntry] = incomingEntry
    
    return { outgoing       = outgoing,
             incoming       = incoming,
             outgoingEntry  = outgoingEntry,
             incomingEntry  = incomingEntry }
  end
  
  return nil
end

function fugueUtil.applyDeepenNegativePass(phraseSeq)
  
  local phraseRule =  function (i)
                        if fugue.phrases[i].destination < 0 and
                           fugue.phrases[i].destination > -4 then -- because the options atm are -2 and -4
                          return true else return false
                        end
                      end

  local dbPhraseRule =  function (i)
                          if i.destination < -2 then
                            return true else return false
                          end
                        end
                      
  return fugueUtil.replacePhrases(phraseSeq, phraseRule, dbPhraseRule)
end

function fugueUtil.applyHeightenPositivePass(phraseSeq)
  
  local phraseRule =  function (i)
                        if fugue.phrases[i].destination > 0 and
                           fugue.phrases[i].destination < 4 then -- because the options atm are 2 and 4
                          return true else return false
                        end
                      end

  local dbPhraseRule =  function (i)
                          if i.destination > 2 then
                            return true else return false
                          end
                        end
                      
  return fugueUtil.replacePhrases(phraseSeq, phraseRule, dbPhraseRule)
end

function fugueUtil.applySwapNegWithPosPass(phraseSeq)
  local phraseRule = function (i)
                       if fugue.phrases[i].destination < 0 then
                         return true else return false
                       end
                     end
                     
  local dbPhraseRule = function (i)
                         if i.destination > 0 then
                           return true else return false
                         end
                       end
                       
  return fugueUtil.replacePhrases(phraseSeq, phraseRule, dbPhraseRule)
end

function fugueUtil.applySwapPosWithNegPass(phraseSeq)
  local phraseRule = function (i)
                       if fugue.phrases[i].destination > 0 then
                         return true else return false
                       end
                     end
  local dbPhraseRule = function (i)
                         if i.destination < 0 then
                           return true else return false
                         end
                       end
                       
  return fugueUtil.replacePhrases(phraseSeq, phraseRule, dbPhraseRule)
end

-- this process "polarizes" the phrase sequence. Makes jumps greater, then when they are too great, correctives
-- go too far, leading to oscillation, without converging.

-- need to pass the iterative functions the current distance to the goal.
-- then they can make better decisions about what to pick and what to replace it with.

-- also, a good feature would be to play the new melody after every pass

-- introduce other rules - avoid using same phrase more than twice in a row.

function fugueUtil.applyPass(phraseSeq)
  local distance = fugueUtil.totalDistance(phraseSeq)
  
  if distance > 0 then
  
    if math.random(2) == 1 then
      fugueUtil.applyDeepenNegativePass(phraseSeq)
    else
      -- replace a positive with a negative
      local positives = fugueUtil.getCandidates(phraseSeq,
        function (i)
          if fugue.phrases[i].destination > 0 then
            return true else return false end
        end)
      
      local negativesDB = fugueUtil.getCandidates(fugue.phrases,
        function (i)
          if i.destination < 0 then
            return true else return false end
        end)
    end
    
  else
    -- find an existing positive and make it bigger
    -- find a negative and replace it with a positive
  end
  
end

function fugueUtil.composeFromPhraseSeq(phraseSeq)
  -- uses the given phrase sequence to produce a composition
  
  local phraseRoot = fugue.makeNote(1,0)
  for i=1,#phraseSeq,1 do
    local phrase = fugue.phrases[phraseSeq[i]]
    fugue.pastePhrase(phrase, fugue.lines[1], phraseRoot)
    phraseRoot = fugue.stepNote(phraseRoot, phrase.destination)
  end
  
  -- put in the note its supposed to land on
  table.insert(fugue.lines[1], fugue.cloneNote(phraseRoot))
end

function fugueUtil.makeRandomPhraseSeq(plen)
  local phraseSeq = {}
  for i=1,plen,1 do
    phraseSeq[i] = math.random(4)
  end
  return phraseSeq
end

function fugue.compose()

  -- select 2 phrases
  -- paste them in either AAB or ABA
  -- tweak them so you end up on the tonic at the end
  
  -- set the random seed
  
  local phraseA = fugue.phrases[math.random(2)]
  local phraseB
  if phraseA.destination < 0 then
    --print("phrase 4")
    phraseB = fugue.phrases[4]
  else
    --print("phrase 3")
    phraseB = fugue.phrases[3]
  end
  
  local note = fugue.makeNote(1,0)
  
  fugue.pastePhrase(phraseA, fugue.lines[1], note)
  note = fugue.stepNote(note, phraseA.destination)
  
  fugue.pastePhrase(phraseA, fugue.lines[1], note)
  note = fugue.stepNote(note, phraseA.destination)
  
  fugue.pastePhrase(phraseB, fugue.lines[1], note)
  note = fugue.stepNote(note, phraseB.destination)
  
  
  -- put in the note its supposed to land on
  
  table.insert(fugue.lines[1], fugue.cloneNote(note))
  
  -- tweak the pasting so that we end up back at the tonic.
  -- then we need suitable accompaniment.
  -- this is like composing a mondrian.
  -- personality is encoded in how heavily the constraints are applied
  -- balance, repeated notes, varying the direction,
  -- different time scales, properties at different resolutions
  
end

function fugue.playForwardAndImprovise()
  local phrase = fugue.currentPhrase
  
  if fugue.currnote > #fugue.lines[1] then
    fugue.improvise(1)
  end
    
    if fugue.p1On == true then
      
      local note = fugue.lines[1][fugue.currnote]
      local midiNote = fugue.makeMidiNote(note)
      
      midiSelectInstrument(fugue.inst1)
      midiNoteOff(fugue.lastNote1)
      midiNoteOn(midiNote)
      
      fugue.lastNote1 = midiNote
      end
    
    fugue.currnote = fugue.currnote + 1
end

function fugue.noteTick()
  
  --if fugue.numNotes < 450 then
  if true then
    fugue.playForwardAndImprovise()
    updateSlipnet()
    
    fugue.numNotes = fugue.numNotes + 1
  else
    print("Wrap it up being set to 0")
    g_wrapItUp = 0
  end
  
  if g_wrapItUp == 2 and fugue.numNotes > 400 then
    --g_wrapItUp = 1
  end
end

-- moving up by a 3rd or a fifth
