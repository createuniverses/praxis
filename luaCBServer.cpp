
// Author:  Greg "fugue" Santucci, 2015
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/

#include "luaCB.h"

int luaCBStartServer(lua_State * L)
{
    bool bResult = PraxisServer::Start();
    lua_pushboolean(L, bResult);
    return 1;
}

int luaCBAcceptConnection(lua_State * L)
{
    SOCKET s = PraxisServer::Accept();
    lua_pushnumber(L, s);
    return 1;
}

int luaCBReceiveData(lua_State * L)
{
    int n = lua_gettop(L);
    if(n >= 1)
    {
        SOCKET s = luaL_checknumber(L, 1);
        std::string data = PraxisServer::Receive(s);
        lua_pushstring(L, data.c_str());
        return 1;
    }
    else
    {
        std::string data = PraxisServer::Receive();
        lua_pushstring(L, data.c_str());
        return 1;
    }
}

int luaCBSendData(lua_State * L)
{
    std::string sText = luaL_checkstring(L, 1);
    int n = lua_gettop(L);
    if(n >= 2)
    {
        SOCKET s = luaL_checknumber(L, 2);
        PraxisServer::Send(s, sText);
    }
    else
    {
        PraxisServer::Send(sText);
    }
    return 0;
}

int luaCBIsValidSocket(lua_State * L)
{
    SOCKET s = luaL_checknumber(L, 1);
    lua_pushboolean(L, PraxisServer::SocketIsValid(s));
    return 1;
}

int luaCBSetBlocking(lua_State * L)
{
    u_long mode = luaL_checknumber(L, 1);
    int n = lua_gettop(L);
    if(n >= 2)
    {
        SOCKET s = luaL_checknumber(L, 2);
        PraxisServer::SetBlockingOption(s, mode);
    }
    else
    {
        PraxisServer::SetBlockingOption(mode);
    }
    return 0;
}

int luaCBShutdownServer(lua_State * L)
{
    return 0;
}

void luaInitCallbacksServer()
{
    lua_register(g_pLuaState, "svrStart",               luaCBStartServer);
    lua_register(g_pLuaState, "svrAccept",              luaCBAcceptConnection);
    lua_register(g_pLuaState, "svrReceive",             luaCBReceiveData);
    lua_register(g_pLuaState, "svrSend",                luaCBSendData);
    lua_register(g_pLuaState, "svrSetBlocking",         luaCBSetBlocking);
    lua_register(g_pLuaState, "svrIsValidSocket",       luaCBIsValidSocket);
    lua_register(g_pLuaState, "svrShutdown",            luaCBShutdownServer);
}
