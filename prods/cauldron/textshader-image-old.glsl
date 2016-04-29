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




#define CHAR_SIZE vec2(8.0, 12.0)

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
    float chunkNmbr = floor(v.x/16.0);
    float chunkPos  = mod(v.x, 16.0);
    float bytePos   = floor(mod(chunkPos, 4.0));
    
    vec4 chunk = vec4(0);
    if (chunkNmbr > 0.5 || lineNmbr > 0.5) {
        chunk = texture2D(iChannel1, ((vec2(chunkNmbr + 0.5, lineNmbr + 0.5)) / iResolution.xy));
    }

    //chunk = vec4(0x4c6f72);
    //chunk = vec4(0x4d4e4f);
    //chunk = vec4(0x616263,0x646566,0x676869,0x6a6b20);
    //chunk = vec4(0x400000,0x400000,0x000000,0x000000);
    
    float fword = 0;
    if      (chunkPos<3.5)  fword = chunk.x;
    else if (chunkPos<7.5)  fword = chunk.y;
    else if (chunkPos<11.5) fword = chunk.z;
    else                    fword = chunk.a;
    
    uint iword = floatBitsToUint(fword);
    
    // Since its not being converted to a float, we can use all 4 bytes now, so no need for this.
    //iword = iword & uint(0x00ffffff);
    
    uint ichara = uint(0);
    
    if      (bytePos < 0.5) ichara = uint(iword >> uint(0));
    else if (bytePos < 1.5) ichara = uint(iword >> uint(8));
    else if (bytePos < 2.5) ichara = uint(iword >> uint(16));
    else                    ichara = uint(iword >> uint(24));
    
    ichara = uint(ichara & uint(0x000000ff));
    
    float fchara = float(ichara);
    
    return fchara;
    
    //word = chunk.z;
    //return word;

    //return mod(floor(word / pow(256.0, 2.0-bytePos)), 256.0);
}


void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    //vec2 pixel = (fragCoord - vec2(iResolution.x/2.0, 0.0)) / (ZOOM * (1.4 - fragCoord.y/iResolution.y));
    vec2 pixel = fragCoord;

    fragColor = vec4(0);

    //if (pixel.x > -104.0) // hack
    {
        float fFrame = float(iFrame);
        
        //pixel.x  += 104.0; // hack
        
        pixel.y   = pixel.y - 16.0*fFrame/30.0;
        
        //pixel.y   = pixel.y - 12.0;
        
        if (pixel.y < 0.0)
        {
          float ch  = readChar(floor(pixel/CHAR_SIZE));
          vec4 color = drawCh(ch, mod(pixel.x, CHAR_SIZE.x), mod(pixel.y, CHAR_SIZE.y));
          fragColor = color;
        }
    }
    
    //uncomment this line to see the output of Buf A
    // if (fragCoord.y > iResolution.y-100.0 && fragCoord.x < 256.0) fragColor = texture2D(iChannel1, vec2(0.0, 0.0));
    //if (fragCoord.y > iResolution.y- 95.0 && fragCoord.x < 256.0) fragColor = texture2D(iChannel0, fragCoord / iResolution.xy);
    //if (fragCoord.y > iResolution.y- 95.0 && fragCoord.x < 256.0) fragColor = texture2D(iChannel1, fragCoord / iResolution.xy);
}
