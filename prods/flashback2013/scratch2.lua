function render()
  WidgetLib.callAll("render")
  
  fugue.render()
  
  renderSlipnet()
  renderWorkspace()
  --testGLColorFunc()
  
  colorGL(255,255,0,255)
  
  local rpos,wpos,rtot,wtot = getSampleMarkers()
  
  drawText2D("" .. rtot, 10,15)
  drawText2D("" .. wtot, 10,10)
  
  drawText2D("" .. lpfEffNode.samplesLastFrame, 10,5)
  
  SynthNode.renderInputs(sineConNode)
  SynthNode.renderInputs(sineConNode2)
  SynthNode.renderInputs(sineGenNode)
  SynthNode.renderInputs(lpfEffNode)
  SynthNode.renderInputs(sinkNode)
  
  --SynthNode.render(lpfEffNode, vec2d(0,0), vec2d(100,30))
  
  --drawLine(0,0,0,0,50,0)
  
  transform.setTranslation(ttest, 50 + math.sin(g_updateCount * 3 * (math.pi / 180)) * 100,60,-100)
  transform.lookAt(ttest, 100,20,100)
  --transform.setScale(ttest, math.sin(g_updateCount * 6 * (math.pi / 180)) * 2, 1,1)
  
  drawLine(100,20,100,100,50,100)
  drawLine(100,20,100, transform.getTranslation(ttest))
  --lookAt(transform.getTranslation(ttest))
  --setCamPos(100,60,100)
  
  --local px,py,pz = transform.localToGlobal(ttest,   0,  0,  0)
  --drawText2D("" .. px .. "," .. py .. "," .. pz, 50, 10)
  
  local pts = {}
  for i = 0,100,10 do
    for j = 0,100,10 do
      local pt  = vec3d(transform.localToGlobal(ttest, i,0,j))
      local pt2 = vec3d(transform.globalToLocal(ttest, pt.x,pt.y,pt.z))
      table.insert(pts, pt)
      table.insert(pts, pt2)
    end
  end
  beginLinGL()
    for i=1,#pts,1 do
      vectorGL(pts[i].x,pts[i].y,pts[i].z)
      vectorGL(pts[i].x,pts[i].y+10,pts[i].z)
    end
  endGL()
end
