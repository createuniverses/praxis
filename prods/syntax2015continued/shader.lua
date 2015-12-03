
shadprog = glCreateProgram(
[[
void main(void)
{
  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
}
]],
[[
void main(void)
{
  gl_FragColor = vec4(0.0, 0.0, 1.0, 1.0);
}
]])

function render()
  WidgetLib.renderAll()

  --renderGreets2()

  glColor(150 + math.random(100),110,20)

  -- move this to airplane.renderGlobal
  renderStreamer(airplane.lwing)
  renderStreamer(airplane.rwing)
  
  glUseProgram(shadprog)

  for i=1,#skythings,1 do
    local thing = skythings[i]
    glPushMatrix()
      glTranslate(Vector3D.getArgs(thing.p))
      glColor(200,200,0)
      glutSolidSphere(thing.r)
      glColor(100,100,100)
      glutWireSphere(thing.r + 1)
    glPopMatrix()
  end
  
  glUseProgram(0)
  
  trace2()
end
