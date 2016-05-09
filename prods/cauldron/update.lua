function clamp(x,min,max)
  if x < min then return min end
  if x > max then return max end
  return x
end

function update()
  WidgetLib.callAll("update")

  for i=1,#skythings,1 do
    local thing = skythings[i]
    local planepos = vec3d(transform.getTranslation(airplane.lspace))
    local tween = thing.p - planepos
    local dist = Vector3D.magnitude(tween)
    tween = Vector3D.normalize(tween)
    if dist < (30 + thing.r) then
      thing.p = planepos + (tween * (30+thing.r))
    end
  end

  --textshaderwritetext(getBufferText())
  --textshader_writebuffer(edGetCharAt, edGetBufferLength())
  
  textshader_writebuffer(
    function (i)
      return edGetCharAt(edGetTopPosition() + i) end,
    edGetBottomPosition() - edGetTopPosition())
  
  local r,g,b = getClearColor()
  r = clamp(r-20,0,255)
  g = clamp(g-20,0,255)
  b = clamp(b-20,0,255)
  setClearColor(r,g,b)
end
