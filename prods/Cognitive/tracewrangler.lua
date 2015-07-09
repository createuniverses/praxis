if tracewrangler == nil then
  tracewrangler = WidgetLib.newSimple()
end

function tracewrangler.update()
  local n = getBufferName()
  if getTraceText() ~= "" then
    switchToBuffer("trace.txt")
    if getBufferName() ~= "trace.txt" then
      newBuffer()
      loadBuffer("trace.txt")
    end
    insertBufferText(getTraceText())
    clearTrace()
    switchToBuffer(n)
  end
end

setBufferName("tracewrangler.lua")


