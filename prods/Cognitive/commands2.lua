

-- composetest.lua

function balancePhrase(ps_orig)
  
  local ps = copyPhraseSeq(ps_orig)
  
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
    print("Solution found")
    return ps
  else
    print("no solution")
  end

  return nil
  
end

local ps_orig = fugueUtil.makeRandomPhraseSeq(11)
local ps = balancePhrase(ps_orig)

if ps ~= nil then
  fugueUtil.composeFromPhraseSeq(ps)
  fugue.play()
end

