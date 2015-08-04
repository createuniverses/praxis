
line = 1
col = 1

function move_cursor(l,c)
  line = l
  col = c
  svrSend(csi .. line .. ";" .. col .. "H")
end

function clear_screen()
  svrSend(csi .. "2J")
end


function handle_up()
  line = line - 1
  if line < 1 then line = 1 end
  move_cursor(line,col)
end

function handle_down()
  line = line + 1
  if line > 20 then line = 20 end
  move_cursor(line,col)
end

function handle_left()
  col = col - 1
  if col < 1 then col = 1 end
  move_cursor(line, col)
end

function handle_right()
  col = col + 1
  if col > 20 then col = 20 end
  move_cursor(line, col)
  -- move to next line
  -- make the end of line determined by whether
  -- a newline was encountered, or just know it
  -- beforehand.
end

csi = string.char(27) .. "["

ansi = {}

ansi[csi.."A"] = handle_up
ansi[csi.."B"] = handle_down
ansi[csi.."C"] = handle_right
ansi[csi.."D"] = handle_left

-- set the metatable for ansi to one that
-- will handle printable characters

printable = {}
for i=1,255,1 do
  printable[i] = false
end

-- upper case
for i=65,65+25,1 do
  printable[i] = true
end

-- lower case
for i=97,97+25,1 do
  printable[i] = true
end

-- digits
for i=49,59,1 do
  printable[i] = true
end

-- space
printable[32] = true

-- all printable characters
for i=32,126,1 do
  printable[i] = true
end

ansimt = {}

function ansimt.__index(t,k)
  --svrSend("meta, #k = " .. #k, sck1)
  if #k == 1 then
    local b = string.byte(k,1)
    --svrSend("b = " .. b, sck1)
    --if b >= 65 and b <= 65+26 then
    if printable[b] then
      svrSend(string.char(b))
      handle_right()
    end
  end
end

setmetatable(ansi, ansimt)

function svrRunEchoServer(sck)
  -- echo server
  local s = svrReceive(sck)
  -- detect up, down, left, right, backspace, delete, insert, etc
  -- shift arrows, more and more and more
  if s~="" then
    clearTrace()
    for i=1,#s,1 do
      svrSend(string.byte(s,i) .. " ", sck1)
      print(string.byte(s,i))
    end
    svrSend("\n", sck1)
    fn = ansi[s] or function () end
    fn()
  end
end
