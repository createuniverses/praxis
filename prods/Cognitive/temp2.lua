
local ps_orig = fugueUtil.makeRandomPhraseSeq(11)
local ps = balancePhrase(ps_orig)

if ps ~= nil then
  fugueUtil.composeFromPhraseSeq(ps)
  fugue.play()
end

local test = {}
local get = function () return test[1] or test[1][1] end

dofile("replacefn.lua")
dofile("editor.lua")

clearTrace()

setClipboardText(getErrorText())


clearError()

setBufferText("")

for i=1,10,1 do
  print2(i .. "\n")
end

printf("hi there how are you")
print2("hello there\n")

newBuffer()

setBufferText((getFunction(f6Pressed)))
setBufferText("test")
setClipboardText("test")

luaCall(getBufferText())

setClipboardText(getFunction(f6Pressed))
setClipboardText(getErrorText())

setBufferText(getClipboardText())

setClipboardText(inspect(keymap))

dofile("editor.lua")

dofile("replacefn.lua")

function testfn(n)
  print2("hello there " .. n)
end

testfn(238)

replaceFunction("testfn", function (n) print2("temp hello " .. n) end, 5)
replaceFunction("render", function () drawLine(0,0,0,100,100,100) end, 100)

setClipboardText(getFunction(keymap[45][0]))

hideEditor()
clearTrace()

local n,v = debug.getupvalue(renderTextures,1)
setClipboardText(""..n..","..v)
i,4
i,2

local s = string.dump(renderTextures)
setClipboardText(string.len(s))
911

math.random(20) + 40,
setClipboardText(getErrorText())
clearError()


