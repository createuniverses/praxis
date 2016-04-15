
pcraft = {}

pcraft.qdp = {x=0, y=0, w=10, h=10}

function pcraft.makematte(quad,offset)
  local matte = {}
  matte.x = quad.x+offset
  matte.y = quad.y+offset
  matte.w = quad
end

pcraft.qm  = {o=0, w=10, h=10}

function pcraft.drawquad(xoffset, w,h)
  glPushMatrix()
  glTranslate(xoffset+150,5,0)
  glBeginLines()
    colorGL(255,255,255,255)
    vectorGL( 0, 0, 0)
    vectorGL( w, 0, 0)
    vectorGL( w, 0, 0)
    vectorGL( w, 0, h)
    vectorGL( w, 0, h)
    vectorGL( 0, 0, h)
    vectorGL( 0, 0, h)
    vectorGL( 0, 0, 0)
  glEnd()
  glPopMatrix()
end

pcraft.displaypapersize = 5

function pcraftrender()
  pcraft.drawquad(0,10,10)  
end

--[[
uniform vec2      iResolution;           // viewport resolution (in pixels)
uniform int       iFrame;                // shader playback frame
uniform vec4      iMouse;                // mouse pixel coords. xy: current (if MLB down), zw: click
uniform sampler2D iChannel0;             // input channel. XX = 2D/Cube

#define T true
#define F false
void mainImage( out vec4 fragColor, in vec2 fragCoord ){
    vec4 b = texture2D(iChannel0,fragCoord/iResolution.xy);
    float r = fract( 1e5 * sin( dot( fragCoord, vec2( 7.1, 9.1 ) ) ) );
    bool a  = bool(b.r);
    vec4 c = texture2D(iChannel0,(fragCoord+vec2( 0, 1))/iResolution.xy)+
             texture2D(iChannel0,(fragCoord+vec2( 0,-1))/iResolution.xy)+
             texture2D(iChannel0,(fragCoord+vec2( 1,-1))/iResolution.xy)+
             texture2D(iChannel0,(fragCoord+vec2( 1, 0))/iResolution.xy)+
             texture2D(iChannel0,(fragCoord+vec2( 1, 1))/iResolution.xy)+
             texture2D(iChannel0,(fragCoord+vec2(-1,-1))/iResolution.xy)+
             texture2D(iChannel0,(fragCoord+vec2(-1, 0))/iResolution.xy)+
             texture2D(iChannel0,(fragCoord+vec2(-1, 1))/iResolution.xy);
    int n = int(c.r);
    b.gb = a ? b.gb * .99 : vec2( 1 );
    a = a?n>3?F:n<2?F:T:n==3?T:F || distance(fragCoord,iMouse.xy)<9.&&iMouse.z>0. || iFrame == 0 && r < .05;
    fragColor = float( a ) * vec4( 1.0, b.gb, 1.0 );
}

void main()
{
    mainImage(gl_FragColor, gl_FragCoord.xy );
}
]]

-- add pcraftrender to render

print2(getFunction(render))
function render()
  WidgetLib.renderAll()

  local h = 5
  local t = fbotest

  pcraftrender()
  
  --colorGL(255,255,255,255)
  --drawLine(0,20,0,100,20,100)
  
  use_shader(mainshader)

  beginQuadGL()
    colorGL(255,255,255,255)
    vectorGL(    0, h,    0)
    vectorGL( t.qs, h,    0)
    vectorGL( t.qs, h, t.qs)
    vectorGL(    0, h, t.qs)
  endGL()
  
  glUseProgram(0)
  
  --colorGL(255,255,255,255)
  --drawLine(0,20,0,100,20,100)

  --renderGreets2()
  --renderskythings()
  trace2()
  
  shader_frame_num = shader_frame_num + 1
end

