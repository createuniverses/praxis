
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

#ifdef __PRAXIS_LINUX__
#include <unistd.h>
#endif

int luaCBGetCurrentDir(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    // https://msdn.microsoft.com/en-us/library/windows/desktop/aa364934%28v=vs.85%29.aspx
    //DWORD WINAPI GetCurrentDirectory(
    //  _In_  DWORD  nBufferLength,
    //  _Out_ LPTSTR lpBuffer
    //);
    char buf[1024];
    ::GetCurrentDirectory(1024, buf);
    lua_pushstring(L, buf);
#endif
#ifdef __PRAXIS_LINUX__
    const char * dirname = get_current_dir_name();
    lua_pushstring(L, dirname);
#endif
    return 1;
}

#ifdef __PRAXIS_LINUX__
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

int luaCBPrintf(lua_State * L)
{
    std::string s = luaL_checkstring(L, 1);
    printf("%s", s.c_str());
    fflush(stdout);
    return 0;
}

//#include <conio.h>

int luaCBGetChar(lua_State * L)
{
    int c = getchar();
    //int c = getch();
    lua_pushnumber(L, c);
    return 1;
}

int luaCBPlatform(lua_State * L)
{
    std::string s = "undefined";
#ifdef __PRAXIS_WINDOWS__
    s = "windows";
#endif
#ifdef __PRAXIS_LINUX__
    s = "linux";
#endif
    lua_pushstring(L, s.c_str());
    return 1;
}

void luaInitCallbacksSystem()
{
    lua_register(g_pLuaState, "luaCall",               luaCBLuaCall);

    lua_register(g_pLuaState, "turnOnDebugHook",       luaCBTurnOnDebugHook);
    lua_register(g_pLuaState, "turnOffDebugHook",      luaCBTurnOffDebugHook);

    lua_register(g_pLuaState, "getFPS",                luaCBGetFPS);

    lua_register(g_pLuaState, "getCurrentDir",         luaCBGetCurrentDir);
    lua_register(g_pLuaState, "setCurrentDir",         luaCBSetCurrentDir);

    lua_register(g_pLuaState, "printf",                luaCBPrintf);
    lua_register(g_pLuaState, "getchar",               luaCBGetChar);

    lua_register(g_pLuaState, "platform",              luaCBPlatform);
}
