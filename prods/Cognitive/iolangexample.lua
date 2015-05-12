-- Press F1 to set up a simple Io example

function f3Pressed()
  local r,e = iolang(getBufferText())
  svrSend(r, sck2)
  svrSend("\n", sck2)
  svrSend(e, sck2)
  svrSend("\n", sck2)
end

function f4Pressed()
  local r,e = iolang(getSelectedText())
  svrSend(r, sck2)
  svrSend("\n", sck2)
  svrSend(e, sck2)
  svrSend("\n", sck2)
end

iolang("appendProto(Praxis)")

newBuffer()
setBufferText([[
render := method(
  drawLine(  0, 10, 0, 100, 10,   0)
  drawLine(100, 10, 0, 100, 10, 100))

// Press F3 interpret buffer as Io
// Press F4 to interpret selected text as Io
]])

-- loadBuffer("iorender.io")


