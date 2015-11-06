setKeyHandlerProgram(53, 2, [[edCopyBuffer = getSelectedText() edDelete()]])
setKeyHandlerProgram(54, 2, [[edCopyBuffer = getSelectedText()]])
setKeyHandlerProgram(55, 2, [[ edTypeString(edCopyBuffer) ]] )

setBufferName("b2s.lua")
saveBuffer()
edEchoKeys = true

edEchoKeys = false
clearTrace()
setKeyHandlerProgram(stdkeyids["s"], 2, [[saveBuffer()]])

hideError()
hideTrace()
showEditor()

print2(stdkeyids["s"])
39

print2(getClipboardText())

function b2s(b)
  if b then return "true" else return "false" end
end

print2(edGetVisLines())
20

print2(getErrorText())

edPageUp()
-- need to add a trace callback to intercept and alter the std behaviour
-- need to add next/prev buffer shortcuts

print2(getTraceText())
  onKeyDownSpecial 13
onKeyUp   36
onKeyUp   62
onKeyDown 112
Missing key handler for 112, mods = 0
onKeyUp   112
onKeyDown 117
Missing key handler for 117, mods = 0
onKeyUp   117
onKeyDown 111
onKeyUp   111
onKeyDown 111
onKeyUp   111
onKeyDown 62
Missing key handler for 62, mods = 0
onKeyDown 36

showTrace()
hideTrace()

clearTrace()
