
// Author:  Greg "fugue" Santucci, 2011
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/

#include "luaCB.h"

int luaCBGetMp3Time(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    if(g_pMp3Stream)
    {
        float fTimeMSec = FSOUND_Stream_GetTime(g_pMp3Stream);
        float fTimeSec = fTimeMSec * 0.001f;

        lua_pushnumber(L, fTimeSec);
    }
    else
    {
        lua_pushnumber(L, 0.0f);
    }
#else
    lua_pushnumber(L, 0.0f);
#endif

    return 1;
}

int luaCBSetMp3Time(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    if(g_pMp3Stream)
    {
        float fPosition = luaL_checknumber(L, 1);
        int nPosition = fPosition * 1000;

        FSOUND_Stream_SetTime(g_pMp3Stream, nPosition);
    }
#endif
    return 0;
}

int luaCBGetMp3Length(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    if(g_pMp3Stream)
    {
        float fTimeMSec = FSOUND_Stream_GetLengthMs(g_pMp3Stream);
        fTimeMSec *= 0.001f;

        lua_pushnumber(L, fTimeMSec);
    }
    else
    {
        lua_pushnumber(L, 0.0f);
    }
#else
    lua_pushnumber(L, 0.0f);
#endif
    return 1;
}

int luaCBPlayMp3(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    if(g_pMp3Stream)
        g_nMp3Channel = FSOUND_Stream_Play(FSOUND_FREE, g_pMp3Stream);
#endif
    return 0;
}

int luaCBStopMp3(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    if(g_pMp3Stream)
        FSOUND_Stream_Stop(g_pMp3Stream);
#endif
    return 0;
}

int luaCBIsMp3Playing(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    if(g_nMp3Channel == -1)
    {
        lua_pushboolean(L, false);
    }
    else
    {
        bool bIsPlaying = FSOUND_IsPlaying(g_nMp3Channel);
        lua_pushboolean(L, bIsPlaying);
    }
#else
    // Changed from false to true so prods don't quit immediately for Linux build
    lua_pushboolean(L, true);
#endif
    return 1;
}

void luaInitCallbacksMp3()
{
    lua_register(g_pLuaState, "getMp3Time",            luaCBGetMp3Time);
    lua_register(g_pLuaState, "setMp3Time",            luaCBSetMp3Time);
    lua_register(g_pLuaState, "getMp3Length",          luaCBGetMp3Length);
    lua_register(g_pLuaState, "playMp3",               luaCBPlayMp3);
    lua_register(g_pLuaState, "stopMp3",               luaCBStopMp3);
    lua_register(g_pLuaState, "isMp3Playing",          luaCBIsMp3Playing);
}
