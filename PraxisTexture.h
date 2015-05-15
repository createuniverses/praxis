#ifndef PRAXISTEXTURE_H
#define PRAXISTEXTURE_H

#include "World.h"
#include "SingleWorldConfiguration.h"
#include "luaInterface.h"

class PraxisTexture
{
public:
    PraxisTexture(int nSize = 128);
    void Clear(unsigned char red, unsigned char green, unsigned char blue);
    void UpdateTexture();

    void Begin();
    void Resume();
    void End();

    void Randomize();

    GLuint nTextureID;
    std::string sRenderFunction;
    int nSize;
    char pixels [1024*1024*3];
};

#endif // PRAXISTEXTURE_H
