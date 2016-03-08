maxstreamersegments = 300

streamer = Queue.new()
function addPointToStreamer(s, p)
  Queue.pushfirst(s, p)
  if Queue.size(s) > maxstreamersegments then
    Queue.poplast(s)
  end
end

function addPointToStreamer2(s, p, max)
  Queue.pushfirst(s, p)
  if Queue.size(s) > max then
    Queue.poplast(s)
  end
end

function renderStreamer(s)
  beginLinGL()
  for i=1,Queue.size(s)-1,1 do
    local p1 = Queue.get(s,i)
    local p2 = Queue.get(s,i+1)
    glVec(p1)
    glVec(p2)
  end
  endGL()
  
  if false then
  for i=1,Queue.size(s)-1,1 do
    local draw = true
    if i > 200 and i > math.random(300) then
      --draw = false
    end
    if draw then
      local p1 = Queue.get(s,i)
      local p2 = Queue.get(s,i+1)
      crazyLine(p1,p2,2,5)
    end
  end
  end
end

function renderStreamerCar(s)
  beginLinGL()
  if Queue.size(s) > 50 then
    local i = 40
    local p1 = Queue.get(s,i)
    local p2 = Queue.get(s,i+5)
    local p3 = p1 * 0.8
    local p4 = p2 * 0.8
    glVec(p1)
    glVec(p3)
    glVec(p2)
    glVec(p4)
    glVec(p3)
    glVec(p4)
  end
  endGL()
end
