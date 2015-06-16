setBufferName("metatabletest.lua")

--switchToBuffer("queue.lua")


print(t)
t = getmetatable(_G)
print(inspect(t))

tempt = {}
tempt.val = 5

print(inspect(tempt))

setmetatable(tempt, nil)
clearTrace()


tempmt = {}

function tempmt.__index(t,k)
  print("trying to access " .. k)
  return tempt[k]
end

print(tempt["val"])
do
  k="val"
  print(tempt[k])
end

setmetatable(_G, tempmt)
setmetatable(_G, nil)

clearError()
continue()

do
  setmetatable(_G, tempmt)
  print(val)
  setmetatable(_G, nil)
end

windowedMode()
fullscreenMode()

