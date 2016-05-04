//  Text rendering in a shader from a string texture
//
//  Adapted from shader program on Shadertoy written by Bart Verheijen 2016
//  https://www.shadertoy.com/view/lsK3D1

#define CHAR_SIZE vec2(8.0, 12.0)

//#define ZOOM 5.0
float ZOOM = floor(min(iResolution.x,iResolution.y) / 100.0);

/**
 * x [0..8>
 * y [0..12>
 **/
vec4 drawCh(in float character, in float x, in float y)
{
    if (character == 0.0)
      return vec4(0.0, 1.0, 0.0, 0.4);

    //if (character > 255.0)
    //  character = mod(character, 256.0);
    //character = 65.0;
      
    vec2 coord = floor(vec2(CHAR_SIZE.x*mod(character,32.0) + x, iResolution.y - CHAR_SIZE.y*floor(0.0+character/32.0) - y));
    return texture2D(iChannel0, (coord+vec2(0.5,0.5)) / iResolution.xy);
}

float readChar(in vec2 v)
{
    float line      = mod(v.y, 30.0);
    float column    = floor(v.x);
    
    //float pixel     = floor(v.x/4.0);
    //float component = mod(v.x, 4.0);
    
    uvec4 chunk = texture(iChannel1, ((vec2(column + 0.5, line + 0.5)) / iResolution.xy));
    
    //uint ichar = 0u;
    //if      (component < 0.5) ichar = chunk.x;
    //else if (component < 1.5) ichar = chunk.y;
    //else if (component < 2.5) ichar = chunk.z;
    //else                      ichar = chunk.a;
    
    uint ichar = chunk.a;
    
    float fchar = float(ichar);
    
    return fchar;
}

vec2 FragCoordToCharPixel_Plain(in vec2 fragCoord)
{
  vec2 pixel = fragCoord;
  pixel.y = iResolution.y - pixel.y;
  return pixel;
}

vec2 FragCoordToCharPixel_Zoom(in vec2 fragCoord)
{
  vec2 pixel = fragCoord;
  //pixel.y = pixel.y + 100.0;
  pixel = (pixel - vec2(iResolution.x/2.0, 0.0)) / (ZOOM * (1.4 - pixel.y/iResolution.y));
  pixel.x = pixel.x + 256.0;
  pixel.y = iResolution.y - pixel.y;
  return pixel;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 pixel = FragCoordToCharPixel_Plain(fragCoord);
    //vec2 pixel = FragCoordToCharPixel_Zoom(fragCoord);
    
    // Default blank color
    fragColor = vec4(1.0, 0.0, 0.0, 0.4);
    
    if (pixel.y > 0.0)
    {
        float fFrame = float(iFrame);
        
        // Uncomment these to have scrolling.
        //float speed = 8.0;
        //pixel.y = pixel.y + speed*fFrame/30.0;
        
        float ch  = readChar(floor(pixel/CHAR_SIZE));

        vec4 color = drawCh(ch, mod(pixel.x, CHAR_SIZE.x), mod(pixel.y, CHAR_SIZE.y));
        fragColor = color;
    }
    
    //if (fragCoord.y > iResolution.y- 95.0 && fragCoord.x < 256.0) fragColor = texture(iChannel0, fragCoord / iResolution.xy);
    //if (fragCoord.y > iResolution.y- 95.0 && fragCoord.x < 256.0) fragColor = texture(iChannel1, fragCoord / iResolution.xy);
}

