-- drawshader.lua

gameoflife.drawshader.prog,shadres = glCreateProgram(

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
