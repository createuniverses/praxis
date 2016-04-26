
float readChar_uintTexture(in vec2 v)
{
    float lineNmbr  = mod(v.y, 30.0);
    float chunkNmbr = floor(v.x/16.0);
    float chunkPos  = mod(v.x, 16.0);
    float bytePos   = floor(mod(chunkPos, 4.0));
    
    uvec4 chunk = texture(iChannel1, ((vec2(chunkNmbr + 0.5, lineNmbr + 0.5)) / iResolution.xy));

    uint iword = uint(0);
    if      (chunkPos <  3.5)  iword = chunk.x;
    else if (chunkPos <  7.5)  iword = chunk.y;
    else if (chunkPos < 11.5)  iword = chunk.z;
    else                       iword = chunk.a;

    //return float(iword);

    uint ichara = uint(0);
    if      (bytePos < 0.5)    ichara = (iword >> uint( 0));
    else if (bytePos < 1.5)    ichara = (iword >> uint( 8));
    else if (bytePos < 2.5)    ichara = (iword >> uint(16));
    else                       ichara = (iword >> uint(24));
    ichara = uint(ichara & uint(0x000000ff));
    
    float fchara = float(ichara);
    
    return fchara;
}

float readChar_intTexture(in vec2 v)
{
    float lineNmbr  = mod(v.y, 30.0);
    float chunkNmbr = floor(v.x/16.0);
    float chunkPos  = mod(v.x, 16.0);
    float bytePos   = floor(mod(chunkPos, 4.0));
    
    ivec4 chunk = texture(iChannel1, ((vec2(chunkNmbr + 0.5, lineNmbr + 0.5)) / iResolution.xy));

    int iword = int(0);
    if      (chunkPos <  3.5)  iword = chunk.x;
    else if (chunkPos <  7.5)  iword = chunk.y;
    else if (chunkPos < 11.5)  iword = chunk.z;
    else                       iword = chunk.a;

    //return float(iword);

    int ichara = int(0);
    if      (bytePos < 0.5)    ichara = (iword >> int( 0));
    else if (bytePos < 1.5)    ichara = (iword >> int( 8));
    else if (bytePos < 2.5)    ichara = (iword >> int(16));
    else                       ichara = (iword >> int(24));
    ichara = int(ichara & int(0x000000ff));
    
    float fchara = float(ichara);
    
    return fchara;
}

