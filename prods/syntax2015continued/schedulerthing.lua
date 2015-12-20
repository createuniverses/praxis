setClipboardText(getBufferText())

schedule = Queue.new()
schedule_time = 0

function addEvent(time, fn)
  Queue.pushlast(schedule,
    {time = schedule_time + (time * 30), fn = fn})
end

function runSchedule()
  if Queue.size(schedule) >= 1 then
    local event = Queue.popfirst(schedule)
    if schedule_time > event.time then
      --print("running event...")
      event.fn()
    else
      Queue.pushfirst(schedule, event)
    end
  end
  
  schedule_time = schedule_time + 1
end

do
  addEvent(1, function () midiNoteOn(60) end)
  addEvent(2, function () midiNoteOff(60) end)
end

function update()
  WidgetLib.callAll("update")
  
  runSchedule()  

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

  local r,g,b = getClearColor()
  r = r - 10
  if r < 0 then r = 0 end
  setClearColor(r,g,b)
end

--setBufferName("schedulerthing.lua")
