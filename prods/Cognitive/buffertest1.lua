--print(string.sub("hello",1,1))

function string.csub(s,l,r)
  return string.sub(s,l+1,r+1)
end

print(string.csub("hello",0,0))

-- The function assumes character at current position is
-- "(" but it doesn't have to be.
function parseOpenParen(pos,bias)
  local text = getBufferText()
  
  -- Prime the loop so it starts after the current character

  local stack = bias
  if string.sub(text,pos,pos) == "(" then stack = stack + 1 end
  if string.sub(text,pos,pos) == ")" then stack = stack - 1 end
  pos = pos + 1

  while stack ~= 0 and pos <= #text do
    if string.sub(text,pos,pos) == "(" then stack = stack + 1 end
    if string.sub(text,pos,pos) == ")" then stack = stack - 1 end
    pos = pos + 1
  end
  
  if stack == 0 then
    return pos-1
  else
    return -1
  end
end

function parseCloseParen(pos,bias)
  local text = getBufferText()
  
  local stack = bias

  if string.sub(text,pos,pos) == "(" then stack = stack + 1 end
  if string.sub(text,pos,pos) == ")" then stack = stack - 1 end
  pos = pos - 1

  while stack ~= 0 and pos >= 1 do
    if string.sub(text,pos,pos) == "(" then stack = stack + 1 end
    if string.sub(text,pos,pos) == ")" then stack = stack - 1 end
    pos = pos - 1
  end
  
  if stack == 0 then
    return pos+1
  else
    return -1
  end
end

clearTrace()

for i=1,6,1 do
  -- a -ve bias is nonsensical. Trying to match an open paren, you
  -- look forward for a close paren. With a -ve bias, you look forward
  -- for an open paren.
  local b = 0
  print(i .. ":" .. parseOpenParen(i,b) .. ":" .. parseCloseParen(i,b))
end


