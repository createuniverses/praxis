
function svrRunLuaPromptServer(sck)
  local s = svrReceive(sck)
  if s ~= "" then
    luaCall(s)
    local err = getErrorText()
    if err ~= "" then
      svrSend(err, sck)
      clearError()
    end
    -- svrSend("Ready.\n")
    --svrSend("\n> ")
    svrSend("> ", sck)
  end
end

-- http://bbs.progrider.org/lounge/read/1423532111
