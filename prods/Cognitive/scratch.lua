
function writePhrase(phrasefile, phrase)
  for i=1,#phrase,1 do
    phrasefile:write(phrase[i], "(", fugue.phrases[phrase[i]].destination, ") ")
  end

  local tot = fugueUtil.totalDistance(phrase)

  phrasefile:write("-> ", tot)
end

function copyPhraseSeq(p)
  local p2 = {}
  for i=1,#p,1 do
    p2[i] = p[i]
  end
  return p2
end

function writeTable(phrasefile, tblname)
  phrasefile:write(tblname, ":\n")
  local tbl = load("return " .. tblname)()
  for i=1,#tbl,1 do
    phrasefile:write(tbl[i], " ")
  end
  phrasefile:write("\n")
end

--fugueUtil.composeFromPhraseSeq(fugueUtil.makeRandomPhraseSeq())

outfile = io.open("output.txt", "a")

-- Will this work:
-- outfile.write2 = outfile.write
-- outfile.write = nil
-- answer: yes!

ps_orig = fugueUtil.makeRandomPhraseSeq(7)
--fugueUtil.composeFromPhraseSeq(ps)


outfile:write("Initial phrase: ")
writePhrase(outfile, ps_orig)
outfile:write("\n\n")

num_success = 0

for i=1,100,1 do
  
  ps = copyPhraseSeq(ps_orig)
  
  outfile:write("Run " .. i .. " ")

  numiterations = 1

  while fugueUtil.totalDistance(ps) ~= 0 and numiterations < 100 do
    results = nil
    if fugueUtil.totalDistance(ps) > 0 then
      results = fugueUtil.applyDeepenNegativePass(ps)
      if results == nil then
        results = fugueUtil.applySwapPosWithNegPass(ps)
      end
    elseif fugueUtil.totalDistance(ps) < 0 then
      results = fugueUtil.applyHeightenPositivePass(ps)
      if results == nil then
        results = fugueUtil.applySwapNegWithPosPass(ps)
      end
    end

    numiterations = numiterations + 1
  end

  if fugueUtil.totalDistance(ps) == 0 then
    outfile:write("Solution found: ")
    num_success = num_success + 1
  else
    outfile:write("   No solution: ")
  end
  writePhrase(outfile, ps)
  outfile:write(", " .. numiterations .. " iterations\n")
  
end

outfile:write("Success rate = " .. num_success .. "/100\n")

outfile:write("\n\n")

-- do this run many times, and see how often it comes with a solution within 100 iterations.
-- What were the initial conditions when a solution wasn't reached?

-- vary or keep constant:
-- the initial melody

-- study enough forth to figure out how the locals mechanism works.
-- curious as well how the names seem to remain scoped to the current function call
-- must allocate that space for them.
-- but then it must free that space when the function is done and free up those local variable names.


io.close(outfile)
