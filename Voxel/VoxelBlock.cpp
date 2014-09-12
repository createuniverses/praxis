
// Author:  Greg "fugue" Santucci, 2011
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/


#include "VoxelBlock.h"

#include "Voxel.h"

#include "ML_Maths.h"

VoxelBlock * g_pVoxelBlock = 0;

VoxelBlock::VoxelBlock()
{
    g_pVoxelBlock = 0;

    m_pFirst = 0;

    m_nSizeX = 100;
    m_nSizeY = 100;
    m_nSizeZ = 100;

    int nNumVoxels = m_nSizeX * m_nSizeY * m_nSizeZ;

    m_pVoxels = new Voxel[nNumVoxels];

    for(int x = 0; x < m_nSizeX; x++)
    {
        for(int y = 0; y < m_nSizeY; y++)
        {
            for(int z = 0; z < m_nSizeZ; z++)
            {
                Voxel * pVoxel = GetVoxelAt(x,y,z);
                pVoxel->SetPosition(x,y,z);

                //                pVoxel->SetColor(
                //                            mlLinearMapping<int, int>(x, 0, m_nSizeX, 0, 255),
                //                            mlLinearMapping<int, int>(y, 0, m_nSizeY, 0, 255),
                //                            mlLinearMapping<int, int>(z, 0, m_nSizeZ, 0, 255),
                //                            255);

                int nRed = 0;
                int nGreen = 0;
                int nBlue = 0;

                if(x%2 == 0)
                    nRed = mlLinearMapping<int, int>(x, 0, m_nSizeX, 0, 127);
                else
                    nRed = mlLinearMapping<int, int>(x, 0, m_nSizeX, 128, 255);

                if(y%2 == 0)
                    nGreen = mlLinearMapping<int, int>(y, 0, m_nSizeY, 0, 127);
                else
                    nGreen = mlLinearMapping<int, int>(y, 0, m_nSizeY, 128, 255);

                if(z%2 == 0)
                    nBlue = mlLinearMapping<int, int>(z, 0, m_nSizeZ, 0, 127);
                else
                    nBlue = mlLinearMapping<int, int>(z, 0, m_nSizeZ, 128, 255);

                pVoxel->SetColor(nRed, nGreen, nBlue, 255);
                Add(x,y,z);
            }
        }
    }

    // All cubes have been "added". Now a render pass (without rendering)
    // needs to occur to fulfil the obligation that a voxel with
    // m_nSidesVisible==0 will definitely not be in the display list.

    // A voxel, once added or carved, cannot be added or carved again
    // until after the housekeeping has occured.

    // All the voxels one voxel away from the cube edge will be removed from
    // the display list after this housekeeping process. These are all the
    // voxels that were "boxed in" after they added themselves to the display
    // list.

    // Clean up - do a render without rendering
    {
        Voxel * pVoxel = m_pFirst;
        Voxel * pPrev  = 0;

        while(pVoxel)
        {
            if(pVoxel->m_nSidesVisible != 0)
            {
                //pVoxel->Render();

                // Previous is good!
                pPrev = pVoxel;
            }
            else
            {
                // Housekeeping, google style!

                if(pPrev == 0)
                {
                    // There hasn't been a good one yet

                    // Update first for future render calls
                    m_pFirst = pVoxel->m_pNext;

                    // Nothing else to do.
                }
                else
                {
                    // There has been a good one

                    // Skip the current voxel for future render calls
                    pPrev->m_pNext = pVoxel->m_pNext;
                }
            }

            // Try the next one.
            pVoxel = pVoxel->m_pNext;
        }
    }
}

void VoxelBlock::Render()
{
    // During the render, voxels with no exposed sides that are still in the
    // display list will be dropped from it.
    //
    // A VERY strong assumption is being made that a voxel with m_nSidesVisible == 0
    // is definitely not in the display list. Running this render functions fulfils
    // this promise.

    Voxel * pVoxel = m_pFirst;
    Voxel * pPrev  = 0;

    while(pVoxel)
    {
        if(pVoxel->m_nSidesVisible != 0)
        {
            pVoxel->Render();

            // Previous is good!
            pPrev = pVoxel;
        }
        else
        {
            // Housekeeping, google style!

            if(pPrev == 0)
            {
                // There hasn't been a good one yet

                // Update first for future render calls
                m_pFirst = pVoxel->m_pNext;

                // Nothing else to do.
            }
            else
            {
                // There has been a good one

                // Skip the current voxel for future render calls
                pPrev->m_pNext = pVoxel->m_pNext;
            }
        }

        // Try the next one.
        pVoxel = pVoxel->m_pNext;
    }
}

void VoxelBlock::Carve(int x, int y, int z)
{
    Voxel * pVoxel = GetVoxelAt(x,y,z);

    if(pVoxel == 0)
        return;

    Voxel * pVoxelLeft   = GetSolidVoxelAt(x-1, y,   z  );
    Voxel * pVoxelRight  = GetSolidVoxelAt(x+1, y,   z  );
    Voxel * pVoxelAhead  = GetSolidVoxelAt(x,   y+1, z  );
    Voxel * pVoxelBehind = GetSolidVoxelAt(x,   y-1, z  );
    Voxel * pVoxelAbove  = GetSolidVoxelAt(x,   y,   z+1);
    Voxel * pVoxelBelow  = GetSolidVoxelAt(x,   y,   z-1);

    pVoxel->m_bActive = false;
    pVoxel->m_nSidesVisible = 0;

    // If there is a solid voxel adjacent, turn on the exposed side.
    // If its not already in the display list, add it.
    // Strong assumption is that this can be queried by reading m_nSidesVisible
    // instead of traversing the entire list.
    // After the render function, voxels with m_nSidesVisible==0 will no
    // longer be in the display list.

#define PROCESS_VOXEL_SIDE(pVox,bit)                       \
    if(pVox)                                               \
    {                                                      \
        if(pVox->m_nSidesVisible == 0)                     \
        {                                                  \
            AddToRenderList(pVox);                         \
        }                                                  \
        pVox->m_nSidesVisible |= Voxel::bit;               \
    }

    PROCESS_VOXEL_SIDE(pVoxelLeft,   X2);
    PROCESS_VOXEL_SIDE(pVoxelRight,  X1);
    PROCESS_VOXEL_SIDE(pVoxelAhead,  Y2);
    PROCESS_VOXEL_SIDE(pVoxelBehind, Y1);
    PROCESS_VOXEL_SIDE(pVoxelAbove,  Z2);
    PROCESS_VOXEL_SIDE(pVoxelBelow,  Z1);

#undef PROCESS_VOXEL_SIDE
}

void VoxelBlock::Add(int x, int y, int z)
{
    Voxel * pVoxel = GetVoxelAt(x,y,z);

    if(pVoxel == 0)
        return;

    Voxel * pVoxelLeft   = GetSolidVoxelAt(x-1, y,   z  );
    Voxel * pVoxelRight  = GetSolidVoxelAt(x+1, y,   z  );
    Voxel * pVoxelAhead  = GetSolidVoxelAt(x,   y+1, z  );
    Voxel * pVoxelBehind = GetSolidVoxelAt(x,   y-1, z  );
    Voxel * pVoxelAbove  = GetSolidVoxelAt(x,   y,   z+1);
    Voxel * pVoxelBelow  = GetSolidVoxelAt(x,   y,   z-1);

    pVoxel->m_bActive = true;

    bool bVoxelInRenderList = (pVoxel->m_nSidesVisible != 0);

    // If there is a solid voxel adjacent, turn off the common side on both voxels.
    // If there is no solid voxel adjacent, turn on the exposed side.

#define PROCESS_VOXEL_SIDE(pVox,ourBit,otherBit)                      \
    if(pVox)                                                          \
    {                                                                 \
        pVoxel->m_nSidesVisible &= ~Voxel::ourBit;                    \
        pVox->m_nSidesVisible &= ~Voxel::otherBit;                    \
    }                                                                 \
    else                                                              \
    {                                                                 \
        pVoxel->m_nSidesVisible |= Voxel::ourBit;                     \
    }

    PROCESS_VOXEL_SIDE(pVoxelLeft,   X1, X2);
    PROCESS_VOXEL_SIDE(pVoxelRight,  X2, X1);
    PROCESS_VOXEL_SIDE(pVoxelAhead,  Y1, Y2);
    PROCESS_VOXEL_SIDE(pVoxelBehind, Y2, Y1);
    PROCESS_VOXEL_SIDE(pVoxelAbove,  Z1, Z2);
    PROCESS_VOXEL_SIDE(pVoxelBelow,  Z2, Z1);

#undef PROCESS_VOXEL_SIDE

    // Only add it if it went from invisible to visible
    if(!bVoxelInRenderList && pVoxel->m_nSidesVisible != 0)
    {
        AddToRenderList(pVoxel);
    }

    // Subsequent "add" operation may box this in and render it invisible
    // (m_nSidesVisible == 0) - its up to the render process to notice this
    // and remove it from the display list if this is the case.
}

Voxel * VoxelBlock::GetVoxelAt(int x, int y, int z)
{
    if(x < 0)
        return 0;
    if(x >= m_nSizeX)
        return 0;

    if(y < 0)
        return 0;
    if(y >= m_nSizeY)
        return 0;

    if(z < 0)
        return 0;
    if(z >= m_nSizeZ)
        return 0;

    int nIndex = (z * m_nSizeX * m_nSizeY) + (y * m_nSizeX) + x;

    Voxel * pVoxel = &m_pVoxels[nIndex];

    return pVoxel;
}

Voxel * VoxelBlock::GetSolidVoxelAt(int x, int y, int z)
{
    Voxel * pVoxel = GetVoxelAt(x,y,z);

    if(pVoxel == 0)
        return 0;

    if(pVoxel->m_bActive == false)
        return 0;

    return pVoxel;
}

void VoxelBlock::AddToRenderList(Voxel * pVoxel)
{
    // A VERY strong assumption: pVoxel isn't already in the display list, and
    // that this can be checked by querying m_nSidesVisible.

    // A VERY strong assumption is being made that a voxel with m_nSidesVisible == 0
    // is definitely not in the display list. Running the render functions fulfils
    // this promise.

    if(m_pFirst)
        pVoxel->m_pNext = m_pFirst;

    m_pFirst = pVoxel;
}
