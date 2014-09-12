-- calculation.lua

function makeDeck()
  local deck = {}
  for suite = 1,4,1 do
    for rank = 1,13,1 do
      local card = { suite = suite, rank = rank }
      table.insert(deck, card)
    end
  end
end

function shuffleDeck(deck)
  while #deck > 0 do
  -- pick a random card from the deck
  -- add it to another deck
  -- remove it from the original deck
  -- do this until the original deck is empty
  end
end
