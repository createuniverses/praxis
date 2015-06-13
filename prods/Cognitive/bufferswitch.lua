setBufferName("bufferswitch.lua")

function prfn(fn)
  print2(getFunction(fn))
end

function newBufferCycler(bufnames)
  local i = 1
  local fn = function ()
    i = i + 1
    if i > #bufnames then i = 1 end
    switchToBuffer(bufnames[i])
  end
  return { fn = fn, i = i }
end

-- how to redefine a closure function
bufCycler = newBufferCycler({"spirograph.lua", "queue.lua", "bufferswitch.lua"})

function f9Pressed()
  bufCycler.fn()
end

print2(bufCycler.i)
1


prfn(f9Pressed)
function f9Pressed()
  showEditor()
  newBuffer()
  setBufferText(getErrorText())
end


