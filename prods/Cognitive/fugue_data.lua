

g_slipnetDecayRate = 3
g_slipnetBumpRate = 25

fugue.inst1 = 0
fugue.inst2 = 10

fugue.p1On = true
fugue.p2On = true

fugue.lastNote1 = 60
fugue.lastNote2 = 60

fugue.numNotes = 0

fugue.minscale = { 0, 2, 3, 5, 7, 8, 11, 12 }
fugue.majscale = { 0, 2, 4, 5, 7, 9, 11, 12 }

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

fugue.chordProgression = { fugue.tonicbases, 
                           fugue.dominantbases,
                           fugue.tonicbases,
                           fugue.tonicbases,
                           fugue.subdominantbases,
                           fugue.tonicbases,
                           fugue.subdominantbases,
                           fugue.supertonicbases,
                           fugue.dominantbases,
                           fugue.tonicbases }

fugue.currentChord = 1

fugue.phrases = {}

-- The phrase is really the first 3 notes. The fourth note is a stepping note
-- In this case its a tone away, but it could be an arbitrary distance away.
-- Its a bridge note.
-- How would a cognitive model discover this?
-- Stepping note concept, stepping note finding codelet. A note with notes from the
-- same chord on either side.
fugue.phrases[1] = { shape = { -1,  1,  1 }, destination =  2 }
fugue.phrases[2] = { shape = {  1, -1, -1 }, destination = -2 }
fugue.phrases[3] = { shape = { -1, -1, -1 }, destination = -4 } -- there can also be a -2 destination version of this
fugue.phrases[4] = { shape = {  1,  1,  1 }, destination =  4 }
fugue.phrases[5] = { shape = { -1, -1,  2 }, destination = -1 }
fugue.phrases[6] = { shape = {  1,  1, -2 }, destination =  1 }
fugue.phrases[7] = { shape = {  2, -1,  2 }, destination =  2 }
fugue.phrases[8] = { shape = { -2,  1, -2 }, destination = -2 }

-- another possibility is that the destination is arbitrary
-- literally any other note can be the landing note for a phrase
-- preferred destination notes imply a specific melody
-- better to allow the preferences to emerge

-- blind watchmaker option for putting the phrases together
-- every choice the human makes populates a table

fugue.adjacencies = {}
for i = 1,10,1 do
  fugue.adjacencies[i] = {}
  for j=1,10,1 do
    fugue.adjacencies[i][j] = 0
  end
end


function sumTable(t)
  local tot = 0
  for i=1,#t,1 do
    tot = tot + t[i]
  end
  return tot
end

fugue.rhythms = {}
fugue.rhythms[1] = {3, 1, 3, 1}
fugue.rhythms[2] = {4/3, 4/3, 4/3, 4}

-- shapes consisting of 3 notes, 4 notes, 6 notes, 8 notes, n notes.
-- they can be pasted anywhere
-- 1 _ 3 _ 5 4 3 2 

-- shapes consisting of 6 notes can be a combination of 2 3 note phrases
-- so you end up with a certain end point

-- make it fun!

-- don't worry about higher level concepts and cognitive models, just generate some music.
-- make the rules more and more complicated. hack it up.
-- THEN invoke the cognitive model to simplify it.

-- shape with inverse
-- semiquavers
-- triplets
-- arbitrary timing
-- -> notes need a duration.

fugue.perturbs = 0
    
fugue.currentPhrase = fugue.phrases[1]

fugue.roots = { fugue.makeNote(1,0), fugue.makeNote(3,0) }

fugue.scale = fugue.minscale

fugue.currentKey = 60 -- C

fugue.lastNoteTime = 0
--fugue.timeBetweenNotes = 0.5
fugue.timeBetweenNotes = 0.15
fugue.currnote = 1

fugue.lines = { {}, {} } -- 2, no 3 parts

g_wrapItUp = 2

-- constraints need to be applied
-- composing under constraints
-- iterative improvements
-- 8 bars
-- land on the tonic
-- don't use the same phrase too often

-- a melody is a series of phrases

-- the joining points might get tweaked in the future to meet some other constraints.

-- initial pass is a random sequence of phrases
-- 2nd and subsequent passes are making a tweak to improve the score
-- A tweak being to change one of the phrases
-- after each tweak, play the entire phrase
-- The harmonic structure imposes constraints within the phrases
-- codelets. I'm thinking of codelets because I'm imagining the discovery of accidental subphrases that
-- have a particular useful property

-- ok, instead of what we have now, which is a constantly improvised piece of music,
-- how about a piecemeal thing, which I can call "play" on at any time

-- > chalice
-- [put the medal smiley here]

-- use cognitive model to simulate the typical sequence of actions a person might take
-- ie, tweak it so a certain sequence occurs
-- want to paste a series of phrases
-- phrase pasting codelet
-- the choice of which phrase to paste is random
-- better: the choice of which phrase to paste is random weighted on activations of the phrase concepts
-- (how the activations of those phrase concepts change is worked out later)
-- some process needs to occur to make the phrases join together and to meet various other constraints
-- as the composition becomes longer, a structuring concept activates, which launches high priority
-- constraint satisfying codelets.

-- how about implementing something simple, get the ball rolling.

-- make my own cognitive model
-- make it behave as I behave
-- thinking about this phrase
-- emotion of thinking about it
-- what goes with this phrase
-- network of concepts, the links can be anything
-- link from phrase a to phrase b can be a "follows" link
-- (so a bit of markov chain thing happens)

-- abc -> abd, mrrjjj -> mrrjjjj
-- m, rr, jjj are a successorship group (lengths 1, 2, 3 -> 3 succ 2, 2 succ 1)
-- a b c are a successorship group (c succ b, b succ a)
-- d is successor of c
-- jjjj is successor of jjj (4 succ 3) - requires answering:
-- what is succ 3?
-- make a thing that is succ 3.

-- "make a thing that is succ 3" is the magical function.
-- that is the part of copycat I have been neglecting, the part that interprets the structures and links,
-- then makes an answer.

-- so what happens here then?
-- the prior knowledge is what is in the concept network
-- the links there.
-- various associational links
-- temporal links, relational links, concept links.

-- any combination of phrases
-- any combination AT ALL
-- phrases can be adjusted to fit harmonic structure.
-- 3 phrases + 1 end note
-- place phrases
-- various rules
-- All 3 phrases different
-- 2 phrases the same, one phrase different

-- regarding Cintral
-- when adding a DLL, need to add a lot of boilerplate whose purpose is unclear
-- various philosophies used to decide on things like meta-methods, meta-sequences, etc
-- without a paper written to explain what the benefit is of using those techniques
-- (smalltalk apparently)
-- so either put those meaningless things in when adding a DLL,
-- or rip out the requirement for them which means changing the existing software
-- hundreds of man hours thrown away, existing technology thrown away.
