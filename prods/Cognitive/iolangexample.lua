
-- Press F1 to set up a simple Io example

function f3Pressed()
  local s = iolang(getBufferText())
  print(s)
end

function f4Pressed()
  s = iolang(getSelectedText())
  print2(s)
end

newBuffer()
setBufferText([[
render := method(
  drawLine(  0, 10, 0, 100, 10,   0)
  drawLine(100, 10, 0, 100, 10, 100))

// Press F3 interpret buffer as Io
// Press F4 to interpret selected text as Io
]])

loadBuffer("iorender.io")
