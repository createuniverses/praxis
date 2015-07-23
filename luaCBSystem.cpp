
// Author:  Greg "fugue" Santucci, 2011
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/

#include "luaCB.h"

int luaCBLuaCall(lua_State * L)
{
    std::string sText = luaL_checkstring(L, 1);

    luaCall(sText);

    return 0;
}

void cbLuaBreakHook(lua_State *L, lua_Debug *ar);

int luaCBTurnOnDebugHook(lua_State * L)
{
    int nLines = 1000;

    int n = lua_gettop(L);
    if(n == 1)
        nLines = luaL_checknumber(L, 1);

    lua_sethook(L, cbLuaBreakHook, LUA_MASKCOUNT, nLines);

    return 0;
}

int luaCBTurnOffDebugHook(lua_State * L)
{
    lua_sethook(L, 0, LUA_MASKCOUNT, 1000);
    return 0;
}

int luaCBGetFPS(lua_State * L)
{
    extern float g_fFPS;
    lua_pushnumber(L, g_fFPS);
    return 1;
}

int luaCBGetCurrentDir(lua_State * L)
{
    return 0;
}

#ifdef __PRAXIS_LINUX__
#include <unistd.h>
int luaCBSetCurrentDir(lua_State * L)
{
    std::string sDir = luaL_checkstring(L, 1);
    chdir(sDir.c_str());
    return 0;
}
#endif

#ifdef __PRAXIS_WINDOWS__
int luaCBSetCurrentDir(lua_State * L)
{
    std::string sDir = luaL_checkstring(L, 1);
    ::SetCurrentDirectory(sDir.c_str());
    return 0;
}
#endif

void luaInitCallbacksSystem()
{
    lua_register(g_pLuaState, "luaCall",               luaCBLuaCall);

    lua_register(g_pLuaState, "turnOnDebugHook",       luaCBTurnOnDebugHook);
    lua_register(g_pLuaState, "turnOffDebugHook",      luaCBTurnOffDebugHook);

    lua_register(g_pLuaState, "getFPS",                luaCBGetFPS);

    lua_register(g_pLuaState, "getCurrentDir",         luaCBGetCurrentDir);
    lua_register(g_pLuaState, "setCurrentDir",         luaCBSetCurrentDir);
}
