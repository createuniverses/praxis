#include "PraxisTexture.h"


PraxisTexture::PraxisTexture(int nSize)
{
    glGenTextures(1,&nTextureID);
    glBindTexture(GL_TEXTURE_2D,nTextureID);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    //glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    //glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    if(nSize <    2)  nSize = 2;
    if(nSize > 1024)  nSize = 1024;

    this->nSize = nSize;

    Clear(0,0,0);

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, nSize, nSize, 0, GL_RGB, GL_UNSIGNED_BYTE, pixels);

    sRenderFunction = "";
}

void PraxisTexture::Clear(unsigned char red, unsigned char green, unsigned char blue)
{
    for(int i = 0; i < nSize * nSize; i++)
    {
        pixels[i*3+0] = red;
        pixels[i*3+1] = green;
        pixels[i*3+2] = blue;
    }
}

void PraxisTexture::Randomize()
{
    for(int i = 0; i < nSize * nSize; i++)
    {
        pixels[i*3+0] = rand() % 255;
        pixels[i*3+1] = rand() % 255;
        pixels[i*3+2] = rand() % 255;
    }
}

void PraxisTexture::UpdateTexture()
{
    // http://www.gamedev.net/page/resources/_/technical/opengl/moving-beyond-opengl-11-for-windows-r1929
    // http://stackoverflow.com/questions/7808146/gltexsubimage2d-extremely-slow-on-intel-video-card
    // http://www.clearchain.com/blog/posts/opengl-gltexsubimage2d-very-slow-a-solution
    // https://developer.palm.com/distribution/viewtopic.php?f=70&t=11066
    // http://www.khronos.org/message_boards/viewtopic.php?f=4&t=1274

    Begin();

    luaCall(sRenderFunction);

    End();
}

void PraxisTexture::Begin()
{
    UseOffscreenContext();

    glViewport(0,0,nSize,nSize);

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT );
}

void PraxisTexture::Resume()
{
    UseOffscreenContext();

    glViewport(0,0,nSize,nSize);

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT );

    glMatrixMode(GL_PROJECTION);
    glPushMatrix();

    glLoadIdentity();

    glOrtho(0,nSize,0,nSize,0,10);

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();

    glLoadIdentity();

    glRasterPos2f(0,0);
    glDrawPixels(nSize, nSize, GL_RGB, GL_UNSIGNED_BYTE, (GLvoid *)pixels);

    glMatrixMode(GL_MODELVIEW);
    glPopMatrix();

    glMatrixMode(GL_PROJECTION);
    glPopMatrix();
}

void PraxisTexture::End()
{
    glReadPixels(
        0, 0, nSize,nSize,
        GL_RGB, GL_UNSIGNED_BYTE, (GLvoid *)pixels);

    UseMainWindowContext();

    glBindTexture(GL_TEXTURE_2D,nTextureID);

    glTexSubImage2D (
        GL_TEXTURE_2D, 0, 0, 0, nSize, nSize,
        GL_RGB, GL_UNSIGNED_BYTE, (void *)pixels);
}
