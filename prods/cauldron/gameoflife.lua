-- gameoflife.lua

gameoflife = {}

gameoflife.mainshader = {}
gameoflife.drawshader = {}
gameoflife.copyshader = {}

gameoflife.mainshader.prog,shadres = glCreateProgram(

[[
void main(void)
{ 
    // normal MVP transform
    // gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
    gl_Position = gl_Vertex;
    gl_Position.w = 1.0;
    
}
]],

[[
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
]])

assertglshader(shadres)

gameoflife.drawshader.prog,shadres = glCreateProgram(

[[
void main(void)
{ 
    // normal MVP transform
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
}
]],

[[
uniform vec2      iResolution;           // viewport resolution (in pixels)
uniform int       iFrame;                // shader playback frame
uniform vec4      iMouse;                // mouse pixel coords. xy: current (if MLB down), zw: click
uniform sampler2D iChannel0;             // input channel. XX = 2D/Cube

void mainImage( out vec4 fragColor, in vec2 fragCoord ){
    bool iMouseDown = !(iMouse.z < 0.);
    vec2 iMouseClick = iMouse.zw;
    if (iMouseClick.x < 0.) {
        iMouseClick.x *= -1.;
    }
    if (iMouseClick.y < 0.) {
        iMouseClick.y *= -1.;
    }
    
    fragColor = texture2D(iChannel0, fragCoord/iResolution.xy);
    //fragColor = vec4(0.0, 0.0, 0.0, 1.0);
    
    float distToMouse = distance(fragCoord.xy, iMouse.xy);
    float distToMouseClick = distance(fragCoord.xy, iMouseClick.xy);
    
    if ( distToMouse < 5.) {
        fragColor = vec4(1.0,1.0,1.0,1.);
    }
    
    if ( distToMouseClick < 10.) {
        fragColor = vec4(1.0,1.0,0.0,1.);
    }
}

void main()
{
    mainImage(gl_FragColor, gl_FragCoord.xy );
}
]])

assertglshader(shadres)

gameoflife.copyshader.prog,shadres = glCreateProgram(

[[
void main(void)
{ 
    // normal MVP transform
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
}
]],

[[
uniform vec2      iResolution;           // viewport resolution (in pixels)
uniform int       iFrame;                // shader playback frame
uniform vec4      iMouse;                // mouse pixel coords. xy: current (if MLB down), zw: click
uniform sampler2D iChannel0;             // input channel

void mainImage( out vec4 fragColor, in vec2 fragCoord ){
    fragColor = texture2D(iChannel0, fragCoord/iResolution.xy);
    //fragColor = texture2D(iChannel0, fragCoord);
    
    //vec2 fragCoordScaled = gl_FragCoord.xy / iResolution;
    //fragColor = texture2D(iChannel0, fragCoordScaled);
}

void main()
{
    mainImage(gl_FragColor, gl_FragCoord.xy );
}
]])

assertglshader(shadres)

local function preparething()
  local g = gameoflife
  
  g.fbo1 = makefbo(512,512, GL_NEAREST)
  g.fbo2 = makefbo(512,512, GL_NEAREST)
  g.fbo_curr = g.fbo1
  g.fbo_prev = g.fbo2
  
  fbotest = makefbo(512,512, GL_NEAREST)
  
  gather_shader_uniforms(g.mainshader)
  gather_shader_uniforms(g.drawshader)
  gather_shader_uniforms(g.copyshader)
end

preparething()

function prerender()
  --do return end
  
  local g = gameoflife
  
  render_to_fbo_with_input(g.fbo_curr, g.mainshader, g.fbo_prev)
  render_to_fbo_with_input(fbotest,    g.copyshader, g.fbo_curr)
  
  g.fbo_curr, g.fbo_prev = g.fbo_prev, g.fbo_curr
end


