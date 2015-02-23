first_visible_line = 1
first_visible_column = 1
buffer = ""
position = 1
screenstring = "line 1\nline 2\nline 3\n"

csi = string.char(27) .. "["

function move_cursor(line,col)
  svrSend(csi .. line .. ";" .. col .. "H")
end

function clear_screen()
  svrSend(csi .. "2J")
end

-- stile di pensiero desideroso

function position_to_line(s, pos)
  -- step through each character, counting the newlines.
  -- THIS will give il numero della linea.
  local linenum = 1
  for i=1,pos,1 do
    if s:byte(i) == 10 then
      linenum = linenum + 1
    end
  end
  return linenum
end

function position_to_col(s, pos)
  -- step backward until you reach
  -- start of buffer or newline.
  local col = 1
  local i = pos
  local stepping = true
  while stepping == true do
    if s:byte(i) == 10  or i <= 0 then
      stepping = false
    else
      i = i - 1
      col = col + 1
    end
  end
  
  return col
end

function position_to_linecol(pos)
end

function linecol_to_screen(pos)
  -- 
end

function screen_to_linecol(line,col)
  
end

function linecol_to_position(line,col)
  local state = "walking"
  local pos = 1
  local lines = 1
  while state == "walking" do
    if s:byte(pos) == 10 then
      lines = lines + 1
    end
    if lines >= line then
      state = "linefound"
    else
      pos = pos + 1
      if pos >= #s then state = "eof" end
    end
  end
  
  if state == "linefound" then
    pos = pos + col
  end
  
  if state == "eof" then
    pos = #s
  end
  
  if pos >= #s then pos = #s end
  
  return pos
end

function set_position(pos)
  if pos < 0 then pos = 0 end
  position = pos
  local line = position_to_line(s, position)
  local col  = position_to_col (s, position) 
  move_cursor(line,col)
end

function handle_up()
  local line = position_to_line(s, position)
  local col  = position_to_col (s, position)
  line = line - 1
  if line < 1 then line = 1 end
  local newpos = linecol_to_position(line, col)
  set_position(newpos)
end

function handle_down()
  local line = position_to_line(s, position)
  local col  = position_to_col (s, position)
  line = line + 1
  local newpos = linecol_to_position(line, col)
  set_position(newpos)
end

function handle_up_old()
  first_visible_line = first_visible_line - 1
  if first_visible_line < 1 then
    first_visible_line = 1
  end
  draw_string(s)
  
  --[[local line = position_to_line(s, position)
  local col  = position_to_col (s, position) 
  line = line - 1
  if line < 1 then line = 1 end
  local newpos = line_to_position(line)
  col = 1
  
  position = newpos
  line = position_to_line(s, position)
  col  = position_to_col (s, position) 
  
  draw_string(s)
  move_cursor(col, line)]]--
  
end

function handle_down_old()
  first_visible_line = first_visible_line + 1
  if first_visible_line < 1 then
    first_visible_line = 1
  end
  draw_string(s)  
end

function handle_left()
  set_position(position-1)
  
  --[[
  first_visible_column = first_visible_column - 1
  if first_visible_column < 1 then
    first_visible_column = 1
  end
  draw_string(s)
  
  position = position - 1
  if position < 1 then position = 1 end
  --draw_string(screenstring)
  ]]--
end

function handle_right()
  set_position(position+1)
  
  --[[
  first_visible_column = first_visible_column + 1
  if first_visible_column < 1 then
    first_visible_column = 1
  end
  draw_string(s)
  
  position = position + 1
  if position > s:len() then position = s:len() end
  ]]--
end

function handle_key(k)
  -- insert character into string
end

function draw_string(s)
  -- set cursor to 1,1
  -- write string to console starting from the
  -- first_visible_line until first_visible_line+num_visible_lines
  -- or the end of the string, whichever is first.
  -- also from first_visible_column until num_visible_columns
  
  --local cx = 1
  --local cy = 1
  move_cursor(1,1)
  clear_screen()
  local s2 = s
  if s2:byte(s2:len()) ~= 10 then
    s2 = s2 .. "\n"
  end
  local linenum = 1
  for line in s2:gmatch('([^\n]*)\n') do
    if linenum >= first_visible_line and linenum < first_visible_line + 20 then
      svrSend(line:sub(first_visible_column, first_visible_column + 50).."\n")
    end
    linenum = linenum + 1
  end
end

lastkey = {}

function svrRunEchoServer(sck)
  -- echo server
  local s = svrReceive(sck)
  -- detect up, down, left, right, backspace, delete, insert, etc
  -- shift arrows, more and more and more
  if s~="" then
    clearTrace()
    lastkey = {}
    for i=1,#s,1 do
      print(string.byte(s,i))
      table.insert(lastkey, string.byte(s, i))
    end
    
    local handled = false
    
    if #lastkey>=3 then
      if lastkey[1] == 27 and lastkey[2] == 91 then
        if lastkey[3] == 65 then
          handle_up()
          handled = true
        end
        if lastkey[3] == 66 then
          handle_down()
          handled = true
        end
        if lastkey[3] == 67 then
          handle_right()
          handled = true
        end
        if lastkey[3] == 68 then
          handle_left()
          handled = true
        end
      end
    end
    
    if handled == false then
      svrSend(s, sck)
    end
  end
end

function showkey()
  for i=1,#lastkey,1 do
    svrSend(""..lastkey[i],sck1)
    if i<#lastkey then
      svrSend(",", sck1)
    end
  end
  svrSend("\n",sck1)
end

function make_lines(s)
  local lines = {}
  for line in s:gmatch('([^\n]*)\n') do
    -- svrSend("->" .. line .. "\n", sck1)
    table.insert(lines, line)
  end
  return lines
end
