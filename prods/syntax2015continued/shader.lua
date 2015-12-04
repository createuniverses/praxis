
do
shadprog,shadres = glCreateProgram(
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
end

do
shadprog2,shadres2 = glCreateProgram(
[[
void main(void)
{
  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
}
]],
[[
uniform sampler2D sampler0;
uniform vec2 tc_offset[9];
void main(void)
{
  vec4 sample[9];
  for (int i = 0; i < 9; i++)
  {
    sample[i] = texture2D(sampler0,
                          gl_TexCoord[0].st + tc_offset[i]);
  }
  // 1 2 1
  // 2 1 2 / 13
  // 1 2 1
  gl_FragColor = (sample[0] + (2.0*sample[1]) + sample[2] +
                 (2.0*sample[3]) + sample[4] + (2.0*sample[5]) +
                 sample[6] + (2.0*sample[7]) + sample[8]) / 13.0;
}
]])
end

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


