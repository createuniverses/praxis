-- fluid.lua

shader_frame_num = 0

gameoflife = gameoflife or {}

gameoflife.mainshader = gameoflife.mainshader or {}
gameoflife.drawshader = gameoflife.drawshader or {}
gameoflife.copyshader = gameoflife.copyshader or {}

gameoflife.mainshader.prog,shadres = glCreateProgram(

[[
void main(void)
{ 
    gl_Position = gl_Vertex;
    gl_Position.w = 1.0;
}
]],

[[
uniform vec2      iResolution;           // viewport resolution (in pixels)
uniform int       iFrame;                // shader playback frame
uniform vec4      iMouse;                // mouse pixel coords. xy: current (if MLB down), zw: click
uniform sampler2D iChannel0;             // input channel. XX = 2D/Cube

const float pi = 3.14159;
const float pi2 = pi/2.0;

float random()
{
  return fract(sin(dot(gl_FragCoord.xy, vec2(12.9898,78.233))) * 43758.5453);  
}

vec4 get_pixel(float x_offset, float y_offset)
{
  return texture2D(
     iChannel0,
     (gl_FragCoord.xy / iResolution.xy) + (vec2(x_offset, y_offset) / iResolution.xy));
}

float step_simulation()
{
  float val = get_pixel(0.0, 0.0).r;
  
  val += random()*val*0.15; // errosion
  
  // hp desktop does not like this.
  //val = 
  //  get_pixel(
  //    sin(get_pixel(val,  0.0).r - get_pixel(-val, 0.0) + pi ).r * val * 0.4,
  //    cos(get_pixel(0.0, -val).r - get_pixel( 0.0, val) - pi2).r * val * 0.4
  //    ).r;
  
  // Splitting it up makes it work:
  float x = sin(get_pixel(val,  0.0).r - get_pixel(-val, 0.0) + pi ).r * val * 0.4;
  float y = cos(get_pixel(0.0, -val).r - get_pixel( 0.0, val) - pi2).r * val * 0.4;
  val = get_pixel(x,y).r;
  
  val *= 1.0001;
  
  return val;
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{    
    float val = step_simulation();
    //float val = 0.0;
 
    if(iFrame == 0)
    {
       val = 0.0;
        //val = 
        //  random()*length(iResolution.xy)/100.0 + 
        //  smoothstep(length(iResolution.xy)/2.0, 0.5, length(iResolution.xy * 0.5 - fragCoord.xy))*25.0;
      //fragColor = vec4(0.0, 0.0, 0.0, 1.0);
    }
    
    if (iMouse.z > 0.0) 
        val += smoothstep(length(iResolution.xy)/10.0, 0.5, length(iMouse.xy - fragCoord.xy));
    
    fragColor = vec4(0.0, 0.0, 0.0, 1.0);
    fragColor.r = val;
}

void main()
{
    mainImage(gl_FragColor, gl_FragCoord.xy );
}
]]

--[[
uniform vec2      iResolution;           // viewport resolution (in pixels)
uniform int       iFrame;                // shader playback frame
uniform vec4      iMouse;                // mouse pixel coords. xy: current (if MLB down), zw: click
uniform sampler2D iChannel0;             // input channel. XX = 2D/Cube

const float pi = 3.14159;
const float pi2 = pi/2.0;

float random()
{
  return fract(sin(dot(gl_FragCoord.xy, vec2(12.9898,78.233))) * 43758.5453);  
}

float randomn(float n)
{
  return fract(sin(dot(gl_FragCoord.xy + vec2(n,0), vec2(12.9898,78.233))) * 43758.5453);  
}

vec4 get_pixel(float x_offset, float y_offset)
{
  //return texture2D(iChannel0, (gl_FragCoord.xy / iResolution.xy) + (vec2(x_offset, y_offset) / iResolution.xy));
  return texture2D(iChannel0, (gl_FragCoord.xy / iResolution.xy));
}

float step_simulation()
{
  float val = get_pixel(0.0, 0.0).r;
    
    //val += random()*val*0.15; // errosion
    
    //val = get_pixel(
    //  sin(get_pixel(val, 0.0).r  - get_pixel(-val, 0.0).r + pi)  * val * 0.4, 
    //    cos(get_pixel(0.0, -val).r - get_pixel(0.0, val).r - pi2) * val * 0.4
    // ).r;
    
    //val *= 1.0001;
    
    return val;
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    //float val = step_simulation();
    float val = texture2D(iChannel0, (gl_FragCoord.xy / iResolution.xy)).r;
    
    #if 1
    
    if (iMouse.z > 0.0) 
        val += smoothstep(length(iResolution.xy)/10.0, 0.5, length(iMouse.xy - fragCoord.xy));
    
    #else
    
    float distToMouse = distance(fragCoord.xy, iMouse.xy);
    if (iMouse.z > 0.0) 
    {
        if(distToMouse < 100.0)
        {
            val += 0.1;
            //if (val > 1.0)
            //  val -= 1.0;
        }
    }
    
    #endif
    
    fragColor = vec4(0.0, 0.0, 0.0, 1.0);
    fragColor.r = val;
}

void main()
{
    mainImage(gl_FragColor, gl_FragCoord.xy );
}
]])

assertglshader(shadres)

--print("main shader:")
--print(shadres)

gameoflife.copyshader.prog,shadres = glCreateProgram(

[[
void main(void)
{ 
    gl_Position = gl_Vertex;
    gl_Position.w = 1.0;
}
]],

[[
uniform vec2      iResolution;           // viewport resolution (in pixels)
uniform int       iFrame;                // shader playback frame
uniform vec4      iMouse;                // mouse pixel coords. xy: current (if MLB down), zw: click
uniform sampler2D iChannel0;             // input channel. XX = 2D/Cube

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    float val = texture2D(iChannel0, fragCoord/iResolution.xy).r;
    vec4 color = pow(vec4(cos(val), tan(val), sin(val), 1.0) * 0.5 + 0.5, vec4(0.5));
    
    // code below taken from
    //https://www.shadertoy.com/view/Xsd3DB
    
    vec2 q = fragCoord.xy/iResolution.xy;
    
    vec3 e = vec3(vec2(1.0)/iResolution.xy,0.0);
    float p10 = texture2D(iChannel0, q-e.zy).x;
    float p01 = texture2D(iChannel0, q-e.xz).x;
    float p21 = texture2D(iChannel0, q+e.xz).x;
    float p12 = texture2D(iChannel0, q+e.zy).x;
        
    vec3 grad = normalize(vec3(p21 - p01, p12 - p10, 1.0));
    vec3 lightparam = vec3(0.2, -0.25, 0.7);
    vec3 light = normalize(lightparam);
    float diffuse = dot(grad,light);
    float spec = pow(max(0.,-reflect(light,grad).z),32.0);
    
    fragColor = vec4(0.0, 0.0, 0.0, 1.0);
    
    fragColor = (color * diffuse) + spec;
}

void main()
{
    mainImage(gl_FragColor, gl_FragCoord.xy);
}
]]

--[[
uniform vec2      iResolution;           // viewport resolution (in pixels)
uniform int       iFrame;                // shader playback frame
uniform vec4      iMouse;                // mouse pixel coords. xy: current (if MLB down), zw: click
uniform sampler2D iChannel0;             // input channel

void mainImage_copy( out vec4 fragColor, in vec2 fragCoord ){
    fragColor = texture2D(iChannel0, fragCoord/iResolution.xy);
}

void mainImage_fluid( out vec4 fragColor, in vec2 fragCoord ){
    float val = texture2D(iChannel0, fragCoord/iResolution.xy).r;
    float s = 1.0;
    s = 1.0;
    //vec4 color = pow(vec4(cos(val*s), tan(val*s), sin(val*s), 1.0) * 0.5 + 0.5, vec4(0.5));
    vec4 color;
    vec4 colori = vec4(cos(val*s), tan(val*s), sin(val*s), 1.0);
    for(int i = 0; i < 4; i++)
    {
      color[i] = pow(colori[i]*0.5+0.5, 0.5);
    }
    //color.r = 0.0;
    color.g = 0.0;
    color.b = 0.0;
    color.a = 1.0;
    fragColor = color;
}

void mainImage_test( out vec4 fragColor, in vec2 fragCoord ){
    float val = texture2D(iChannel0, fragCoord/iResolution.xy).r;
    float s = 1.0;
    val = sin(val*s) * 0.5 + 0.5;
    fragColor = vec4(val, 0.0, 0.0, 1.0);
}

void main()
{
    //mainImage_copy(gl_FragColor, gl_FragCoord.xy );
    mainImage_fluid(gl_FragColor, gl_FragCoord.xy );
    //mainImage_test(gl_FragColor, gl_FragCoord.xy );
}
]])

assertglshader(shadres)

--print("copy shader:")
--print(shadres)

local function preparething(new)
  local g = gameoflife
  
  -- GL_NEAREST
  -- GL_LINEAR
  
  if new == true then
    g.fbo1 = makefbo(512,512, GL_LINEAR)
    g.fbo2 = makefbo(512,512, GL_LINEAR)
    g.fbo_curr = g.fbo1
    g.fbo_prev = g.fbo2
    fbotest = makefbo(512,512, GL_NEAREST)
  else
    g.fbo1 = g.fbo1 or makefbo(512,512)
    g.fbo2 = g.fbo2 or makefbo(512,512)
    g.fbo_curr = g.fbo_curr or g.fbo1
    g.fbo_prev = g.fbo_curr or g.fbo2
  end
  
  gather_shader_uniforms(g.mainshader)
  gather_shader_uniforms(g.drawshader)
  gather_shader_uniforms(g.copyshader)
end

--preparething(false)
preparething(true)


function prerender()
  local g = gameoflife
  
  render_to_fbo_with_input(g.fbo_curr,  g.mainshader, g.fbo_prev)
  render_to_fbo_with_input(fbotest,     g.copyshader, g.fbo_curr)

  g.fbo_curr, g.fbo_prev = g.fbo_prev, g.fbo_curr
end

--clearTrace()





