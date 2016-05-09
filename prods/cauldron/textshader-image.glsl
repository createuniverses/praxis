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
    vec2 coord = floor(vec2(CHAR_SIZE.x*mod(character,32.0) + x, iResolution.y - CHAR_SIZE.y*floor(0.0+character/32.0) - y));
    vec4 pixel = texture2D(iChannel0, (coord+vec2(0.5,0.5)) / iResolution.xy);
    return pixel;
}

float readChar(in vec2 v)
{
    float line   = floor(v.y);
    float column = floor(v.x);
    vec4 chunk = texture2D(iChannel1, ((vec2(column + 0.5, line + 0.5)) / iResolution.xy));
    float fchar = floor(chunk.r * 255.0 + 0.5);
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
        
        vec2 colrow = floor(pixel/CHAR_SIZE);
        
        float ch  = readChar(colrow);

        vec4 color = drawCh(ch, mod(pixel.x, CHAR_SIZE.x), mod(pixel.y, CHAR_SIZE.y));
        
        if (colrow == iCursorPos)
          color.r = 0.5;
          
        if (colrow == iBlockStart)
          color.b = 1.0;

        if (colrow == iBlockEnd)
          color.b = 1.0;
        
        // On start row and selection on multiple rows
        if (colrow.y        == iSelectionStart.y &&
            iSelectionEnd.y > iSelectionStart.y &&
            colrow.x        >= iSelectionStart.x)
          color.g = 0.5;
          
        // On start row and selection on single row
        if (iSelectionEnd.y == iSelectionStart.y &&
            colrow.y        == iSelectionStart.y &&
            colrow.x        >= iSelectionStart.x &&
            colrow.x        <  iSelectionEnd.x)
          color.g = 0.5;
          
        // On row between begin and end rows
        if (colrow.y > iSelectionStart.y &&
            colrow.y < iSelectionEnd.y)
          color.g = 0.5;
          
        // On end row and selection on multiple rows
        if (colrow.y        == iSelectionEnd.y &&
            iSelectionEnd.y >  iSelectionStart.y &&
            colrow.x        <  iSelectionEnd.x)
          color.g = 0.5;
          
        fragColor = color;
    }
    
    //if (fragCoord.y > iResolution.y- 95.0 && fragCoord.x < 256.0) fragColor = texture(iChannel0, fragCoord / iResolution.xy);
    //if (fragCoord.y > iResolution.y- 95.0 && fragCoord.x < 256.0) fragColor = texture(iChannel1, fragCoord / iResolution.xy);
}


