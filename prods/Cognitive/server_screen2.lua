
line = 1
col = 1

function move_cursor(line,col)
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
end

csi = string.char(27) .. "["

ansi = {}

ansi[csi.."A"] = handle_up
ansi[csi.."B"] = handle_down
ansi[csi.."C"] = handle_right
ansi[csi.."D"] = handle_left

function isPrintable(k)
  local r = false
  if k >= 65 and k <= 65+26 then
    r = true
  end
  if k >= 97 and k <= 97+26 then
    r = true
  end
  return r
end

-- set the metatable for ansi to one that
-- will handle printable characters

ansimt = {}

function ansimt.__index(t,k)
  --svrSend("meta, #k = " .. #k, sck1)
  if #k == 1 then
    local b = string.byte(k,1)
    --svrSend("b = " .. b, sck1)
    if isPrintable(b) then
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
      print(string.byte(s,i))
    end
    fn = ansi[s] or function () end
    fn()
  end
end
