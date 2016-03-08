function update()
  WidgetLib.callAll("update")

 if false then
  if getMp3Time() > 117 then
    airplane.followcam = false
    airplane.pilot = airplane.allstoppilot
  elseif getMp3Time() > 105.6 then
    airplane.followcam = true
    showdiscs = false
    showarms = false
  elseif getMp3Time() > 79 then
    do
      airplane.followcam = false
      lookAt(transform.getTranslation(airplane.lspace))
      showdiscs = true
      showarms = true
    end
  elseif getMp3Time() > 16 then
    airplane.pilot = airplane.normalpilot
  end
  --clearTrace()
  --print(""..getMp3Time())
 end
  
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
  
  if isMp3Playing() == false then
    --os.exit()
  end
end

setPickSphere(true)

--windowedMode()
--fullscreenMode()

showFPS()

      airplane.followcam = false
      lookAt(transform.getTranslation(airplane.lspace))
      showdiscs = true
      showarms = true
