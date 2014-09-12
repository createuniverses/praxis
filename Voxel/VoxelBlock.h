
// Author:  Greg "fugue" Santucci, 2011
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/


#ifndef VOXELBLOCK_H
#define VOXELBLOCK_H

class Voxel;

class VoxelBlock
{
public:
    VoxelBlock();

    int m_nSizeX;
    int m_nSizeY;
    int m_nSizeZ;

    void Render();

    Voxel * GetVoxelAt(int x, int y, int z);
    Voxel * GetSolidVoxelAt(int x, int y, int z);

    void Carve(int x, int y, int z);
    void Add(int x, int y, int z);

    Voxel * m_pVoxels;

    Voxel * m_pFirst;

    void AddToRenderList(Voxel * pVoxel);

    // Use a QHash instead of Lua tables?
};

#endif // VOXELBLOCK_H
