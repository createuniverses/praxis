function getFunction2(fn)
  local dt = debug.getinfo(fn)
  if string.sub(dt.source, 1,1) == "@" then
    --print(dt.short_src)
    --print(dt.linedefined, dt.lastlinedefined)
    local filetext = readFile(dt.short_src)
    -- this perhaps needs to be written in lua
    local fntext = selectLines(filetext, dt.linedefined, dt.lastlinedefined)
    --print(fntext)
    return fntext,dt
  else
    local fntext = selectLines(dt.source, dt.linedefined, dt.lastlinedefined)
    --print(fntext)
    return fntext,dt
  end
end

