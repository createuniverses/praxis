
composer =
{
  phrases =
  {
    {seq = {1,2,3}, b = 1, e = {4,2,1}},
    {seq = {1,2,1}, b = 1},
    {seq = {1,2,3,4}, b = 1},
    {seq = {1,3}, b = 1},
    {seq = {5,4,3,2}, b = 5},
  }
}

do
  local phrases = composer.phrases
  composer.piece = 
  {
    {p = phrases[1], b = 1, e = 2},
    {p = phrases[1], b = 1, e = 2},
    {p = phrases[2], b = 1, e = 0},
    {p = phrases[4], b = 1, e = 2}
  }
end

function shiftNote(s,diff)
  local note = {n = s.n, o = s.o}
  s.n = s.n + diff
end

function renderPhrase(phrase, base, ticksPerBeat)
  local piece = {}
  local ticks = ticksPerBeat / #phrase.p
  for i=1,#phrase.p,1 do
    local n = shiftNote(base, phrase.p[i] - phrase.b)
    addNoteToPiece(piece, n, ticks)
  end
  return piece
end

function renderPiece(piece, ticksPerBeat)
  local rendered = {}
  local base = {n=1,o=4}
  -- each phrase fits in one beat
  for i=1,#piece,1 do
    local phrase = piece[i]
    local notes = renderPhrase(phrase, base, ticksPerBeat)
    addNotesToPiece(rendered, notes)
    base = shiftNote(base, phrase.e - phrase.b)
  end
  return rendered
end
