
// Author:  Greg "fugue" Santucci, 2011
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/


#include "Voxel.h"

#include "ML_Vector.h"

#include "GL/openglut.h"

Voxel::Voxel()
{
    m_nSidesVisible = 0;
    m_bActive = false;
    m_pNext = 0;

    m_nX = -1;
    m_nY = -1;
    m_nZ = -1;

    m_nRed = 50;
    m_nGreen = 50;
    m_nBlue = 150;
    m_nAlpha = 255;
}

void Voxel::SetPosition(int x, int y, int z)
{
    m_nX = x;
    m_nY = y;
    m_nZ = z;
}

void Voxel::SetColor(int red, int green, int blue, int alpha)
{
    m_nRed = red;
    m_nGreen = green;
    m_nBlue = blue;

    m_nAlpha = alpha;
}

void Voxel::Render()
{
    // Make this settable from lua
    float fWidth = 10.0f;

    bool x1 = m_nSidesVisible & Voxel::X2;
    bool x2 = m_nSidesVisible & Voxel::X1;
    bool y1 = m_nSidesVisible & Voxel::Z1;
    bool y2 = m_nSidesVisible & Voxel::Z2;
    bool z1 = m_nSidesVisible & Voxel::Y1;
    bool z2 = m_nSidesVisible & Voxel::Y2;

    float fSize = fWidth * 0.5f;

    mlVector3D vCorner(m_nX*fWidth, m_nZ*fWidth, m_nY*fWidth);

    mlVector3D vCenter = vCorner + mlVector3D(fSize, fSize, fSize);

    glColor4ub(m_nRed, m_nGreen, m_nBlue, m_nAlpha);

    // Speed up with:
    // VBO
    // Display lists

#   define V(a,b,c) glVertex3d( vCenter.x + a fSize, vCenter.y + b fSize, vCenter.z + c fSize );
//#   define N(a,b,c) glNormal3d( a, b, c );
#   define N(a,b,c)
    //glBegin( GL_QUADS );
        if(x1) { N( 1, 0, 0 ); V( +, -, + ); V( +, -, - ); V( +, +, - ); V( +, +, + ); }
        if(y1) { N( 0, 1, 0 ); V( +, +, + ); V( +, +, - ); V( -, +, - ); V( -, +, + ); }
        if(z1) { N( 0, 0, 1 ); V( +, +, + ); V( -, +, + ); V( -, -, + ); V( +, -, + ); }
        if(x2) { N( -1, 0, 0 ); V( -, -, + ); V( -, +, + ); V( -, +, - ); V( -, -, - ); }
        if(y2) { N( 0, -1, 0 ); V( -, -, + ); V( -, -, - ); V( +, -, - ); V( +, -, + ); }
        if(z2) { N( 0, 0, -1 ); V( -, -, - ); V( -, +, - ); V( +, +, - ); V( +, -, - ); }
    //glEnd( );
#   undef V
#   undef N
}
