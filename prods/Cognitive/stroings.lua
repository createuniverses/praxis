setBufferName("stroings.lua")
print2(getFunction("f7Pressed"))
playSound()
stopSound()
lookDown()
function f5Pressed()
  buffText = getBufferText()
end

print(string.char(65,66,67,68))
print(string.rep("hello",10))

buffText = "hello"
for i=1,string.len(buffText),1 do
  print(string.sub(buffText,i,i))
end

startPlaying()
clearError()

