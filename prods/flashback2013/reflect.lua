-- Name: reflect.lua

function getFunction(fnname)
  local dt = debug.getinfo(
    load("return " .. string.gsub(fnname, ":", "."))())
  if string.sub(dt.source, 1,1) == "@" then
    --print(dt.short_src)
    --print(dt.linedefined, dt.lastlinedefined)
    local filetext = readFile(dt.short_src)
    local fntext = selectLines(filetext, dt.linedefined, dt.lastlinedefined)
    --print(fntext)
    return fntext,dt
  else
    local fntext = selectLines(dt.source, dt.linedefined, dt.lastlinedefined)
    --print(fntext)
    return fntext,dt
  end
end

function editfn(fnname)
  local fntxt = getFunction(fnname)
  newBuffer()
  setBufferText(fntxt)
end
