praxis:

local ps_orig = fugueUtil.makeRandomPhraseSeq(11)
local ps = balancePhrase(ps_orig)

if ps ~= nil then
  fugueUtil.composeFromPhraseSeq(ps)
  fugue.play()
end

