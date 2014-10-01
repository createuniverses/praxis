print2(getFunction("print2"))
function print2(...)
  local arg = {...}
  for i,v in ipairs(arg) do
    local s = v
    if type(s) == "table" then
      --s = inspect(v)
      --s = ""
    else
      insertBufferText(s .. "\n")
    end
  end
end


function print2(...)
  local arg = {...}
  for i,v in ipairs(arg) do
    local s = v
    if type(s) == "table" then
      --s = inspect(v)
      --s = ""
    else
    insertBufferText(s .. "\n")
    end
  end
end

setBufferName("print2.lua")

function print2(...)
  local arg = {...}
  for i,v in ipairs(arg) do
    local s = v
    if type(s) == "table" then
      --s = inspect(v)
      s = ""
    end
    insertBufferText(s .. "\n")
  end
end

clearError()


function unpack2(...)
    local arg = {...}
    local t = {}
    for i,v in ipairs(arg) do
        if type(v) == "table" then
            for i2,v2 in ipairs(v) do
                table.insert(t, v2)
            end
        else
            table.insert(t, v)
        end
    end
    return table.unpack(t)
end

{
  currentline = -1,
  func = <function 1>,
  lastlinedefined = 16,
  linedefined = 3,
  namewhat = "",
  nups = 0,
  short_src = "unpack2.lua",
  source = "@unpack2.lua",
  what = "Lua"
}
print2(getFunction("unpack2"))
