os.execute("ls")
os.execute("~/robotarm/robotarm 00 00 00")

function WidgetLib.newRobotArmController()
  local w = {}
  w.lspace = transform.new()
  transform.setTranslation(w.lspace, -120,1,-202)
  w.width = 30
  w.height = 10
  w.depth = 50
  w.render = function (o)
  glBeginQuads()
    colorGL(255,155,0,255)
    vectorGL(0,       0, 0)
    vectorGL(o.width, 0, 0)
    vectorGL(o.width, 0, o.depth)
    vectorGL(0,       0, o.depth)
  glEnd()
  glBeginLines()
    colorGL(0,0,0,255)
    for i=0,o.width+1,5 do
      glVertex(i, 0.1, 0)
      glVertex(i, 0.1, o.depth)
    end
    for i=0,o.depth+1,5 do
      glVertex(0,       0.1, i)
      glVertex(o.width, 0.1, i)
    end
  glEnd()
  end
  w.update = function (o) end

  w.lmbdown = function (o,x,y,z)
  end

  w.lmbup = function (o,x,y,z) end
  w.rmbdown = function (o,x,y,z) end
  w.rmbup = function (o,x,y,z) end
  w.mousemove = function (o,x,y,z) end
  table.insert(Widgets, w)
  return w
end

robotArmWidget = WidgetLib.newRobotArmController()
clearError()

do
  armcmdgrid[1][1] = "10"
  armcmdgrid[2][1] = "20"
  armcmdgrid[3][1] = "40"
  armcmdgrid[4][1] = "80"
  armcmdgrid[3][2] = "50"
  armcmdgrid[4][2] = "90"
  armcmdgrid[3][3] = "60"
  armcmdgrid[4][3] = "A0"
end

do
  local w = robotArmWidget
  armcmdgrid = {}
  for i=1,6,1 do
    armcmdgrid[i] = {}
    for j=1,6,1 do
      armcmdgrid[i][j] = "00"
    end
  end
  
  w.lmbdown = function (o,x,y,z)
    for i=1,6,1 do
      for j=1,6,1 do
        local minx,maxx = (i-1)*5, i*5
        local minz,maxz = (j-1)*5, j*5
        if x>minx and x<maxx and z>minz and z<maxz then
          os.execute("~/robotarm/robotarm "..armcmdgrid[i][j].." 00 00")
        end
      end
    end
  end
  
  w.lmbup = function (o,x,y,z)
    os.execute("~/robotarm/robotarm 00 00 00")
  end
end


do
  local w = robotArmWidget
  w.lmbup = function (o,x,y,z)
    os.execute("~/robotarm/robotarm 00 00 00")
  end
end

clearError()


