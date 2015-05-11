
LineDrawer := Object clone do(
  appendProto(Praxis)
  render := method(
    drawLine(  0, 10, 0, 100, 10,   0)
    drawLine(100, 10, 0, 100, 10, 100)))

render := method(
   LineDrawer render())


// Press F3 interpret buffer as Io
// Press F4 to interpret selected text as Io


