
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
