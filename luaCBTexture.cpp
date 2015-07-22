
// Author:  Greg "fugue" Santucci, 2011
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/

#include "luaCB.h"

int luaCBTextureNew(lua_State * L)
{
    // return the GL index of the texture and a pointer to the QImage itself.

    PraxisTexture ** c = (PraxisTexture **)lua_newuserdata(L, sizeof(PraxisTexture *));

    *c = new PraxisTexture();

    luaL_getmetatable(L, "LiveCode.texture");
    lua_setmetatable(L, -2);

    return 1;
}

int luaCBTextureGC(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    PraxisTexture * c = *(PraxisTexture **)luaL_checkudata(L, 1, "LiveCode.texture");

    delete c;

    return 0;
}

int luaCBTextureClear(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    PraxisTexture * c = *(PraxisTexture **)luaL_checkudata(L, 1, "LiveCode.texture");

    c->Clear(0,0,0);

    return 0;
}

int luaCBTextureSelect(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    PraxisTexture * c = *(PraxisTexture **)luaL_checkudata(L, 1, "LiveCode.texture");

    glBindTexture(GL_TEXTURE_2D, c->nTextureID);

    return 0;
}

int luaCBTextureUpdate(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    PraxisTexture * c = *(PraxisTexture **)luaL_checkudata(L, 1, "LiveCode.texture");

    c->UpdateTexture();

    return 0;
}

int luaCBTextureBegin(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    PraxisTexture * c = *(PraxisTexture **)luaL_checkudata(L, 1, "LiveCode.texture");

    c->Begin();

    return 0;
}

int luaCBTextureEnd(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    PraxisTexture * c = *(PraxisTexture **)luaL_checkudata(L, 1, "LiveCode.texture");

    c->End();

    return 0;
}

int luaCBTextureSetRenderFunction(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=2) luaL_error(L, "2 arguments expected.");

    PraxisTexture * c = *(PraxisTexture **)luaL_checkudata(L, 1, "LiveCode.texture");

    std::string sRenderFunction = luaL_checkstring(L, 2);

    c->sRenderFunction = sRenderFunction;

    return 0;
}

void luaInitCallbacksTextureLib()
{
    const struct luaL_Reg lua_texturelib [] = {
        {"new",                   luaCBTextureNew},
        {"clear",                 luaCBTextureClear},
        {"select",                luaCBTextureSelect},
        {"update",                luaCBTextureUpdate},
        {"setRenderFn",           luaCBTextureSetRenderFunction},
        {"beginDrawing",          luaCBTextureBegin},
        {"endDrawing",            luaCBTextureEnd},
        {NULL, NULL}
    };

    luaL_register(g_pLuaState, "texture",   lua_texturelib);

    luaL_newmetatable(g_pLuaState, "LiveCode.texture");
    lua_pushstring(g_pLuaState, "__gc");
    lua_pushcfunction(g_pLuaState, luaCBTextureGC);
    lua_settable(g_pLuaState, -3);

    // Lua 5.2:
    //    lua_newtable(g_pLuaState);
    //    luaL_setfuncs (g_pLuaState,lua_texturelib,0);
    //    lua_pushvalue(g_pLuaState,-1);
    //    lua_setglobal(g_pLuaState,"texture");
}
