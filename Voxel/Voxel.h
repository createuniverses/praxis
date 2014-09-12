
// Author:  Greg "fugue" Santucci, 2011
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/


#ifndef VOXEL_H
#define VOXEL_H

class Voxel
{
public:
    Voxel();

    void SetPosition(int x, int y, int z);
    void SetColor(int red, int green, int blue, int alpha);

    void Render();

    enum Side
    {
        NONE = 0x00000000,
        X1   = 0x10000000, // Left       -x    -x
        X2   = 0x20000000, // Right      +x    +x
        Y1   = 0x40000000, // Forward    +y    +z
        Y2   = 0x80000000, // Back       -y    -z
        Z1   = 0x01000000, // Up         +z    +y
        Z2   = 0x02000000  // Down       -z    -y
    };

    int m_nSidesVisible;
    bool m_bActive;

    Voxel * m_pNext;

    int m_nRed;
    int m_nGreen;
    int m_nBlue;
    int m_nAlpha;

    int m_nX;
    int m_nY;
    int m_nZ;
};

#endif // VOXEL_H
