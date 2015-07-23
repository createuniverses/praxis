
// Author:  Greg "fugue" Santucci, 2011
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/

#include "luaCB.h"

int luaCBMakeVoxelBlock(lua_State * L)
{
    if(g_pVoxelBlock == 0)
        g_pVoxelBlock = new VoxelBlock();

    return 0;
}

int luaCBRenderVoxelBlock(lua_State * L)
{
    if(g_pVoxelBlock)
        g_pVoxelBlock->Render();

    return 0;
}

int luaCBCarveVoxelAt(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=3) luaL_error(L, "3 arguments expected.");

    int x = luaL_checknumber(L, 1);
    int y = luaL_checknumber(L, 2);
    int z = luaL_checknumber(L, 3);

    if(g_pVoxelBlock)
        g_pVoxelBlock->Carve(x,y,z);

    return 0;
}

int luaCBAddVoxelAt(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=3) luaL_error(L, "3 arguments expected.");

    int x = luaL_checknumber(L, 1);
    int y = luaL_checknumber(L, 2);
    int z = luaL_checknumber(L, 3);

    if(g_pVoxelBlock)
        g_pVoxelBlock->Add(x,y,z);

    return 0;
}

int luaCBGetVoxelColour(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=3) luaL_error(L, "3 arguments expected.");

    int x = luaL_checknumber(L, 1);
    int y = luaL_checknumber(L, 2);
    int z = luaL_checknumber(L, 3);

    if(g_pVoxelBlock)
    {
        Voxel * pVoxel = g_pVoxelBlock->GetVoxelAt(x,y,z);

        if(pVoxel)
        {
            lua_pushnumber(L, pVoxel->m_nRed);
            lua_pushnumber(L, pVoxel->m_nGreen);
            lua_pushnumber(L, pVoxel->m_nBlue);
            lua_pushnumber(L, pVoxel->m_nAlpha);

            return 4;
        }
    }

    return 0;
}

int luaCBSetVoxelColour(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=7) luaL_error(L, "7 arguments expected.");

    int x = luaL_checknumber(L, 1);
    int y = luaL_checknumber(L, 2);
    int z = luaL_checknumber(L, 3);

    int red   = luaL_checknumber(L, 4);
    int green = luaL_checknumber(L, 5);
    int blue  = luaL_checknumber(L, 6);
    int alpha = luaL_checknumber(L, 7);

    if(g_pVoxelBlock)
    {
        Voxel * pVoxel = g_pVoxelBlock->GetVoxelAt(x,y,z);

        if(pVoxel)
        {
            pVoxel->m_nRed   = red;
            pVoxel->m_nGreen = green;
            pVoxel->m_nBlue  = blue;
            pVoxel->m_nAlpha = alpha;
        }
    }

    return 0;
}

void luaInitCallbacksVoxel()
{
    lua_register(g_pLuaState, "makeVoxelBlock",        luaCBMakeVoxelBlock);
    lua_register(g_pLuaState, "renderVoxelBlock",      luaCBRenderVoxelBlock);
    lua_register(g_pLuaState, "carveVoxelAt",          luaCBCarveVoxelAt);
    lua_register(g_pLuaState, "addVoxelAt",            luaCBAddVoxelAt);
    lua_register(g_pLuaState, "getVoxelColour",        luaCBGetVoxelColour);
    lua_register(g_pLuaState, "setVoxelColour",        luaCBSetVoxelColour);
}
