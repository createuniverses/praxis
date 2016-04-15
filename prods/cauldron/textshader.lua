-- Beginnings of a text shader
-- Takes a font fbo and texture encoding a string as input

textshader = {}

textshader.docshader    = {}
textshader.fontshader   = {}
textshader.stringshader = {}
textshader.copyshader   = {}



textshader.stringshader.prog,shadres = glCreateProgram(

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

// --------------------------------------------------------------
//
//  This Shader codes a string into a texture.
//  
// --------------------------------------------------------------

//  Created by Bart Verheijen 2016
//  License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.



vec4 myText(vec2 v)
{
    if (v.y < 0.5 || v.y > 22.5 || v.x > 2.5) {
        return vec4(0x202020);
    }
    if (v.y < 1.5) {
        if (v.x < 0.5)  return vec4(0x4c6f72, 0x656d20, 0x697073, 0x756d20);
        if (v.x < 1.5)  return vec4(0x646f6c, 0x6f7220, 0x736974, 0x20616d);
        if (v.x < 2.5)  return vec4(0x65742c, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 2.5) {
        if (v.x < 0.5)  return vec4(0x202063, 0x6f6e73, 0x656374, 0x657475);
        if (v.x < 1.5)  return vec4(0x722061, 0x646970, 0x697363, 0x696e67);
        if (v.x < 2.5)  return vec4(0x202020);
    }
    if (v.y < 3.5) {
        if (v.x < 0.5)  return vec4(0x656c69, 0x742c20, 0x736564, 0x20646f);
        if (v.x < 1.5)  return vec4(0x206569, 0x75736d, 0x6f6420, 0x74656d);
        if (v.x < 2.5)  return vec4(0x706f72, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 4.5) {
        if (v.x < 0.5)  return vec4(0x202069, 0x6e6369, 0x646964, 0x756e74);
        if (v.x < 1.5)  return vec4(0x207574, 0x206c61, 0x626f72, 0x652065);
        if (v.x < 2.5)  return vec4(0x742020, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 5.5) {
        if (v.x < 0.5)  return vec4(0x202020, 0x20646f, 0x6c6f72, 0x65206d);
        if (v.x < 1.5)  return vec4(0x61676e, 0x612061, 0x6c6971, 0x75612e);
        if (v.x < 2.5)  return vec4(0x202020);
    }
    if (v.y < 6.5) {
        if (v.x < 0.5)  return vec4(0x202055, 0x742065, 0x6e696d, 0x206164);
        if (v.x < 1.5)  return vec4(0x206d69, 0x6e696d, 0x207665, 0x6e6961);
        if (v.x < 2.5)  return vec4(0x6d2c20, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 7.5) {
        if (v.x < 0.5)  return vec4(0x207175, 0x697320, 0x6e6f73, 0x747275);
        if (v.x < 1.5)  return vec4(0x642065, 0x786572, 0x636974, 0x617469);
        if (v.x < 2.5)  return vec4(0x6f6e20, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 8.5) {
        if (v.x < 0.5)  return vec4(0x202075, 0x6c6c61, 0x6d636f, 0x206c61);
        if (v.x < 1.5)  return vec4(0x626f72, 0x697320, 0x6e6973, 0x692075);
        if (v.x < 2.5)  return vec4(0x742020, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 9.5) {
        if (v.x < 0.5)  return vec4(0x202020, 0x616c69, 0x717569, 0x702065);
        if (v.x < 1.5)  return vec4(0x782065, 0x612063, 0x6f6d6d, 0x6f646f);
        if (v.x < 2.5)  return vec4(0x202020);
    }
    if (v.y < 10.5) {
        if (v.x < 0.5)  return vec4(0x202020, 0x202020, 0x202020, 0x636f6e);
        if (v.x < 1.5)  return vec4(0x736571, 0x756174, 0x2e2020, 0x202020);
        if (v.x < 2.5)  return vec4(0x202020);
    }
    if (v.y < 13.5) {
        return vec4(0x202020);
    }
    if (v.y < 14.5) {
        if (v.x < 0.5)  return vec4(0x204475, 0x697320, 0x617574, 0x652069);
        if (v.x < 1.5)  return vec4(0x727572, 0x652064, 0x6f6c6f, 0x722069);
        if (v.x < 2.5)  return vec4(0x6e2020, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 15.5) {
        if (v.x < 0.5)  return vec4(0x726570, 0x726568, 0x656e64, 0x657269);
        if (v.x < 1.5)  return vec4(0x742069, 0x6e2076, 0x6f6c75, 0x707461);
        if (v.x < 2.5)  return vec4(0x746520, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 16.5) {
        if (v.x < 0.5)  return vec4(0x207665, 0x6c6974, 0x206573, 0x736520);
        if (v.x < 1.5)  return vec4(0x63696c, 0x6c756d, 0x20646f, 0x6c6f72);
        if (v.x < 2.5)  return vec4(0x652020, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 17.5) {
        if (v.x < 0.5)  return vec4(0x206575, 0x206675, 0x676961, 0x74206e);
        if (v.x < 1.5)  return vec4(0x756c6c, 0x612070, 0x617269, 0x617475);
        if (v.x < 2.5)  return vec4(0x722e20, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 18.5) {
        if (v.x < 0.5)  return vec4(0x202045, 0x786365, 0x707465, 0x757220);
        if (v.x < 1.5)  return vec4(0x73696e, 0x74206f, 0x636361, 0x656361);
        if (v.x < 2.5)  return vec4(0x742020, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 19.5) {
        if (v.x < 0.5)  return vec4(0x202063, 0x757069, 0x646174, 0x617420);
        if (v.x < 1.5)  return vec4(0x6e6f6e, 0x207072, 0x6f6964, 0x656e74);
        if (v.x < 2.5)  return vec4(0x2c2020, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 20.5) {
        if (v.x < 0.5)  return vec4(0x207375, 0x6e7420, 0x696e20, 0x63756c);
        if (v.x < 1.5)  return vec4(0x706120, 0x717569, 0x206f66, 0x666963);
        if (v.x < 2.5)  return vec4(0x696120, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 21.5) {
        if (v.x < 0.5)  return vec4(0x202064, 0x657365, 0x72756e, 0x74206d);
        if (v.x < 1.5)  return vec4(0x6f6c6c, 0x697420, 0x616e69, 0x6d2069);
        if (v.x < 2.5)  return vec4(0x642020, 0x202020, 0x202020, 0x202020);
    }
    if (v.y < 22.5) {
        if (v.x < 0.5)  return vec4(0x202020, 0x202020, 0x202065, 0x737420);
        if (v.x < 1.5)  return vec4(0x6c6162, 0x6f7275, 0x6d2e20, 0x202020);
        if (v.x < 2.5)  return vec4(0x202020);
    }

    return vec4(0x202020);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    //write iResolution into BufA (store it for the next Frame)
    if (fragCoord.x<0.9 && fragCoord.y<0.9) {
        fragColor = vec4(iResolution, 1.0);
        return;
    }

    //read iResolution of the previous Frame
    vec3 iPreviousResolution = texture2D(iChannel0, vec2(0.0)).xyz;


    if (iResolution != iPreviousResolution)
    {
        fragColor = myText(floor(fragCoord));
    }
    else
    {
        fragColor = texture2D(iChannel0, fragCoord / iResolution.xy);
    }
}

void main()
{
    mainImage(gl_FragColor, gl_FragCoord.xy );
}
]])

assertglshader(shadres)

textshader.docshader.prog,shadres = glCreateProgram(

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

// --------------------------------------------------------------
//
//  This is a demonstration for using a font texture in GLSL ES
//
// --------------------------------------------------------------
//
//  Buf A generates a bitmap with 256 characters each 8x12 pixels.
//  To draw an ASCII character the main shader reads 8x12 pixels
//  from the texture.
//  The text string for this demo is encoded in the texture created
//  by Buf B.
//
//  Both Buf A and Buf B don't actually need to be re-calculated
//  for every new Frame.
//  Ideally the font texture should be fed as a texture. But I
//  don't know how to use a custom texture in ShaderToy.
//
//  Many thanks to Flyguy: https://www.shadertoy.com/view/Mt2GWD
//
// --------------------------------------------------------------

//  Created by Bart Verheijen 2016
//  License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.




#define CHAR_SIZE vec2(8, 12)

//#define ZOOM 5.0
float ZOOM = floor(min(iResolution.x,iResolution.y) / 100.0);



/**
 * x [0..8>
 * y [0..12>
 **/
vec4 drawCh(in float character, in float x, in float y)
{
    vec2 coord = floor(vec2(CHAR_SIZE.x*mod(character,32.0) + x, iResolution.y - CHAR_SIZE.y*floor(1.0+character/32.0) + y));
    return texture2D(iChannel0, (coord+vec2(0.5,0.5)) / iResolution.xy);
}

float readChar(in vec2 v)
{
    if (v.y > 0.0) v.y = 0.0; // hack
    float lineNmbr  = mod(-1.0 * v.y, 30.0); // hack
    float chunkNmbr = floor(v.x/CHAR_SIZE.y);
    float chunkPos  = mod(v.x, CHAR_SIZE.y);
    float bytePos   = floor(mod(chunkPos, 3.0));
    
    vec4 chunk = vec4(0);
    if (chunkNmbr > 0.5 || lineNmbr > 0.5) {
        chunk = texture2D(iChannel1, (vec2(chunkNmbr + 0.5, lineNmbr + 0.5)) / iResolution.xy);
    }
    
    float word = 0.0;
    if      (chunkPos<2.5) word = chunk.x;
    else if (chunkPos<5.5) word = chunk.y;
    else if (chunkPos<8.5) word = chunk.z;
    else                   word = chunk.a;

    return mod(floor(word / pow(256.0, 2.0-bytePos)), 256.0);
}


void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 pixel = (fragCoord - vec2(iResolution.x/2.0, 0.0)) / (ZOOM * (1.4 - fragCoord.y/iResolution.y));
    
    fragColor = vec4(0);

    if (pixel.x > -104.0) // hack
    {
        pixel.x  += 104.0; // hack
        pixel.y   = pixel.y - 8.0*iGlobalTime;
        float ch  = readChar(floor(pixel/CHAR_SIZE));
        vec4 color = drawCh(ch, mod(pixel.x, CHAR_SIZE.x), mod(pixel.y, CHAR_SIZE.y));

        //vec2 uv = fragCoord.xy / ZOOM;
        //vec3 col = mix(vec3(0.1), vec3(0.33, 1.0, 0.0), color.g);
        //col *= (1.0 - 1.5*distance(mod(uv,vec2(1.0)),vec2(0.6)))*1.5;
        fragColor = color;//vec4(col, 1.0);
    }
    
    //uncomment this line to see the output of Buf A
    //if (fragCoord.y > iResolution.y-95.0 && fragCoord.x < 256.0) fragColor = texture2D(iChannel0, fragCoord / iResolution.xy);
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
  
  local g = textshader
  
  render_to_fbo_with_input(g.fbo_font_curr, g.fontshader, g.fbo_font_prev)
  render_to_fbo_with_input(g.fbo_text_curr, g.textshader, g.fbo_text_prev)
  render_to_fbo_with_inputs(fbotest,        g.docshader,  g.fbo_font_curr, g.fbo_text_curr )
  
  g.fbo_font_curr, g.fbo_font_prev = g.fbo_font_prev, g.fbo_font_curr
  g.fbo_text_curr, g.fbo_text_prev = g.fbo_text_prev, g.fbo_text_curr
end


