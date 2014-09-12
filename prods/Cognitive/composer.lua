-- composer.lua

-- assume fugue.lua is loaded.

-- each bar is quantized into 16 semiquavers

-- The composition will be 4 bars long, then 8, then 16, then 32 bars long for a full composition

-- key changing phrase: 4 5 6 7 in the new key, 5 4 3 2, 7 6 5 7

-- sections that can be pasted together! Phrases leading into each other, a matter of selecting which one
-- choice comes from how much jumping between phrases you are happy to do

-- playing a phrase without modification, at the end, there are choices for what chord to land on
-- phrase which flows into intself and is repeatable

-- with a composition of a fixed length, the system will make multiple passes in an attempt to satisfy
-- constraints.

-- (ultimately, this can be driven by a copycat cognitive model, worry about that later)

-- getting the computer to invent a phrase
-- a measure of beauty, and multiple passes to increase the measure

fugue.shapes = {}
fugue.shapes[1] = { 1, 1, -3 }
fugue.shapes[2] = { 1, 1, -1 }

-- detecting a leading note or leading note style things

fugue.shapes[3] = { 1, -1, -1 }
fugue.shapes[4] = { -1, 1, "leading note" } -- leading note for start of next phrase

-- piecemeal implementation

-- pasting phrases in, one after the other

-- stop when a particular target note is reached as the first note.