
do
 do airplane.makeFlatDonut = function(r,step)
  local model = {}
  local pts1 = {}
  local pts2 = {}

  for a = 0,360,step do
    table.insert(pts1, vec3d(
        r * math.sin(deg2rad(a)),
        0,
        r * math.cos(deg2rad(a))))
    table.insert(pts2, vec3d(
        2 * r * math.sin(deg2rad(a)),
        0,
        2 * r * math.cos(deg2rad(a))))
  end

  for i=1,#pts1-1,1 do
    table.insert(model,
      { pts1[i],
        pts2[i],
        pts2[i+1],
        pts1[i+1]    } )
  end
  return model
 end end

  local thing = airplane.makeFlatDonut(10,10)
  function renderDome()
    dome.renderModel(thing)
  end
end

do
  airplane.render = function (o)
    renderDome()
    renderModel(planemodel)
    addPointToStreamer2(o.lwing,
      vec3d(transform.localToGlobal(o.lspace, 15,-3,0)),
      o.smax)
    addPointToStreamer2(o.rwing,
      vec3d(transform.localToGlobal(o.lspace, -15,-3,0)),
      o.smax)
   if true then
    local p = Queue.get(streamer, 1)
    if p~=nil then
      p = cvec3d(p)
      p.x, p.y, p.z = transform.localToGlobal(spirowidget.lspace, p.x, p.y, p.z)
      p.x, p.y, p.z = transform.globalToLocal(o.lspace, p.x, p.y, p.z)
      p = p + vec3d(0,5,0)
      glColor(255,50,50,255)
      glBeginTriangles()
        glVertex(p.x - 10, p.y, p.z)
        glVertex(p.x + 10, p.y, p.z)
        glVertex(p.x, p.y+10, p.z+0)
      glEnd()
      --drawLine(0,0,0,p.x,p.y,p.z)
      --drawText3D("here", p.x,p.y,p.z)
    end
   end
  end
end
