function stringreader(s_initial)
  local s = s_initial
  local fn = function (n)
    return s:at(n+1):byte()
  end
  local setter = function (s_new)
    s = s_new
  end
  local lenfn = function () return #s end
  return fn,lenfn,setter
end

mystringreader,mystringlength,setstring = stringreader("this is fascinating, isn't it?\ngenerating stuff like this I mean.")

setstring("testing1\ntesting2\ntesting3\n")

function countstringchars(s, c, pos)
  return 10
end

function numtostr(n, w)
  local s = tostring(n)
  while #s < w do
    s = " " .. s
  end
  return s
end

mynum = 0

do
 continue()
 clearError()
 function addQuad(x,y, width, height, textleft, texttop, z)
  local w = WidgetLib2.newSimple("quad")
  w.render = function (o)
    do
      local x,y,w,h = getWindowRect()
      h = h - texttop
      
      texwb_writetext(mystringreader, mystringlength, 36, textshader_charprocs_plain)
      --textshader_writebuffer(mystringreader, mystringlength)
      use_text_shader(textshader, textleft, h)
    end
    beginQuadGL()
      --colorGL(255,155,0,255)
      colorGL(55,55,55,255)
      vectorGL(0,       z, 0)
      vectorGL(o.width, z, 0)
      vectorGL(o.width, z, o.depth)
      vectorGL(0,       z, o.depth)
    endGL()
    
    glUseProgram(0)
  end
  w.update = function (o)
    setstring(getTraceText())
  end
  w.width = width
  w.depth = height
  transform.setTranslation(w.lspace, x,y,-60)
  transform.rotate(w.lspace, deg2rad(-90), 1,0,0)
  return w
 end

 uimainwidget.Widgets = {}
 local w = uimainwidget.Widgets
 --w[1] = addQuad(10,5, 300, 200, 600, 100,0)
 w[1] = addQuad(-200,-100, 300, 200, 800, 100,0)
 --w[2] = addQuad(-200,-100, 300, 200, 800, 50,1)
 --w[3] = addQuad(-200,-100, 300, 200, 1000, 50,2)
 --w[4] = addQuad(-200,-100, 300, 200, 400, 50,3)
end

