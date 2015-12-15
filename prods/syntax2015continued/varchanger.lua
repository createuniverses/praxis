varchangerLib = {}

function varchangerLib.update(o)
  o.curval = o.valgetter()
  if math.abs(o.curval) > math.abs(o.prevval) then
    o.control = o.control * -1
  end
  o.prevval = o.curval
  o.applycontrol(o.control)
end

function varchangerLib.new(valgetter, applycontrol)
  local c = {}
  c.curval = 0
  c.prevval = 0
  c.control = 0
  c.valgetter = valgetter
  c.applycontrol = applycontrol
  c.update = varchangerLib.update
  return c
end
