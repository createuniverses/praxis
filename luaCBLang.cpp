
// Author:  Greg "fugue" Santucci, 2011
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/

#include "luaCB.h"

int luaCBLisp(lua_State * L)
{
#ifdef __PRAXIS_WITH_LISP__
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    std::string sCode = luaL_checkstring(L, 1);

    lispCall(sCode);

    std::string sOut = lispGetOutput();
    std::string sErr = lispGetError();

    if(sErr != "")
        sOut = sOut + std::string(" ") + sErr;

    lua_pushstring(L, sOut.c_str());
    return 1;
#else
    lua_pushstring(L, "Lisp not compiled.");
    return 1;
#endif
}

int luaCBForth(lua_State * L)
{
#ifdef __PRAXIS_WITH_FORTH__
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    std::string sCode = luaL_checkstring(L, 1);

    forthCall(sCode);

    std::string sOut = forthGetOutput();
    std::string sErr = forthGetError();

    if(sErr != "")
        sOut = sOut + std::string(" ") + sErr;

    lua_pushstring(L, sOut.c_str());

    return 1;
#else
    lua_pushstring(L, "Forth not compiled.");
    return 1;
#endif
}

int luaCBIoLang(lua_State * L)
{
#ifdef __PRAXIS_WITH_IO__
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    std::string sCode = luaL_checkstring(L, 1);

    ioCallWithReply(sCode);

    lua_pushstring(L, ioGetReply().c_str());
    lua_pushstring(L, ioGetTrace().c_str());

    return 2;
#else
    lua_pushstring(L, "Io not compiled.");
    lua_pushstring(L, "");
    return 2;
#endif

}

int luaCBSetLispTraceVerbosity(lua_State * L)
{
#ifdef __PRAXIS_WITH_LISP__
    extern int g_nLispTraceVerbosity;

    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    int nVerbosity = luaL_checknumber(L, 1);
    g_nLispTraceVerbosity = nVerbosity;
#endif

    return 0;
}

int luaCBGetLispTraceVerbosity(lua_State * L)
{
#ifdef __PRAXIS_WITH_LISP__
    extern int g_nLispTraceVerbosity;

//    int n = lua_gettop(L);
//    if(n!=1) luaL_error(L, "1 argument expected.");

    lua_pushnumber(L, g_nLispTraceVerbosity);
#else
    lua_pushnumber(L, 0);
#endif

    return 1;
}

void luaInitCallbacksLang()
{
    lua_register(g_pLuaState, "lisp",                   luaCBLisp);
    lua_register(g_pLuaState, "forth",                  luaCBForth);
    lua_register(g_pLuaState, "iolang",                 luaCBIoLang);

    lua_register(g_pLuaState, "setLispTraceVerbosity",  luaCBSetLispTraceVerbosity);
    lua_register(g_pLuaState, "getLispTraceVerbosity",  luaCBGetLispTraceVerbosity);
}
