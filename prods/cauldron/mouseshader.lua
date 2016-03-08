-- praxis:
--continue()

shadprog,shadres = glCreateProgram(

-- basic vertex shader
[[
void main(void)
{ 
    // normal MVP transform
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
}
]],

-- This is where its implemented
[[
// From: https://www.shadertoy.com/view/ldjSzd

uniform vec2      iResolution;           // viewport resolution (in pixels)
// uniform float     iGlobalTime;           // shader playback time (in seconds)
// uniform float     iTimeDelta;            // render time (in seconds)
// uniform int       iFrame;                // shader playback frame
// uniform float     iChannelTime[4];       // channel playback time (in seconds)
// uniform vec3      iChannelResolution[4]; // channel resolution (in pixels)
uniform vec4      iMouse;                // mouse pixel coords. xy: current (if MLB down), zw: click
// uniform sampler2D iChannel0;             // input channel. XX = 2D/Cube
// uniform sampler2D iChannel1;             // input channel. XX = 2D/Cube
// uniform sampler2D iChannel2;             // input channel. XX = 2D/Cube
// uniform sampler2D iChannel3;             // input channel. XX = 2D/Cube
// uniform vec4      iDate;                 // (year, month, day, time in seconds)
// uniform float     iSampleRate;           // sound sample rate (i.e., 44100)

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
  vec2 uv = fragCoord.xy / iResolution.xy;
    
    // iMouse's xy values contain the (last) mouse drag position. Simple.
    // iMouse's zw values contain the (last) click position, and are
    // signed negative if the mouse button is up.
    bool iMouseDown = !(iMouse.z < 0.);
    vec2 iMouseClick = iMouse.zw;
    if (iMouseClick.x < 0.) {
        iMouseClick.x *= -1.;
    }
    if (iMouseClick.y < 0.) {
        iMouseClick.y *= -1.;
    }
    
    if (uv.x > 0.5) {
        // Left side of screen shows iMouse.xy (mouse drag location)
        fragColor = vec4(
            iMouse.xy / iResolution.xy,
            0.,1.);
         // fragColor = vec4(1.,0.,0.,1.);
    } else {
        // Right side of screen shows iMouse.zw (as 'normalized' iMouseClick.xy) 
        //fragColor = vec4(0.,0.,1..,1.);
        fragColor = vec4(
            iMouseClick.xy / iResolution.xy,
            0.,1.);
         //fragColor = vec4(0.,1.,0.,1.);
    }
    
    fragColor = vec4(0.,0.,0.,1.);
    
    // Draw mouseDown indicator.
    // Empty circle if up, solid if down.
    float distToScreenCenter = distance(fragCoord.xy, vec2(0.5,0.5) * iResolution.xy);
    if ( distToScreenCenter < 20. && distToScreenCenter > (iMouseDown ? 0. : 18.) ) {
      fragColor = vec4(1.,1.,1.,1.);
    }
    
    // Draw mouseClick and mouse location indicators.
    float distToMouse = distance(fragCoord.xy, iMouse.xy);
    float distToMouseClick = distance(fragCoord.xy, iMouseClick.xy);
    
    // Mouse drag location
    if ( distToMouse < 10. && distToMouse > 8.) {
        fragColor = vec4(1.0,1.0,1.0,1.);
    }
    
    // Mouse click location
    if ( distToMouseClick < 5.) {
        fragColor = vec4(1.0,1.0,1.0,1.);
    }
    
}

void main()
{
    mainImage(gl_FragColor, gl_FragCoord.xy );
}

]])

do
  local t = fbotest
  t.uloc = {}
  local u = t.uloc
  
  t.frame = 0
  
  --u.frame   = glGetUniformLocation(shadprog, "iFrame")
  u.resolution = glGetUniformLocation(shadprog, "iResolution")
  u.mouse   = glGetUniformLocation(shadprog, "iMouse")
  --u.sampler = glGetUniformLocation(shadprog, "iChannel0")
  
  --assertgl()
end
