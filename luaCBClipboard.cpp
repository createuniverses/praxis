
// Author:  Greg "fugue" Santucci, 2011
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/

#include "luaCB.h"

int luaCBSetClipboardText(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    HGLOBAL      hGlobal ;
    PTSTR        pGlobal ;

    std::string sText = luaL_checkstring(L, 1);

    hGlobal = GlobalAlloc (GHND | GMEM_SHARE, sText.length() + 1) ;
    pGlobal = (PTSTR)GlobalLock (hGlobal) ;

    for (int i = 0 ; i < sText.length() ; i++)
        *(pGlobal+i) = sText.at(i);

    GlobalUnlock (hGlobal) ;

    OpenClipboard (g_AppHWND) ;

    EmptyClipboard () ;

    SetClipboardData (CF_TEXT, hGlobal) ;

    CloseClipboard () ;
#endif
    return 0;
}

int luaCBGetClipboardText(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    HGLOBAL      hGlobal ;
    PTSTR        pGlobal ;

    std::string sText;

    OpenClipboard (g_AppHWND) ;
    if (hGlobal = GetClipboardData (CF_TEXT))
    {
         pGlobal = (PTSTR)GlobalLock (hGlobal) ;
         sText = pGlobal;
         GlobalUnlock(hGlobal);
    }
    CloseClipboard () ;

    lua_pushstring(L, sText.c_str());
#endif
#ifdef __PRAXIS_LINUX__
    lua_pushstring(L, "not implemented");
#endif

    return 1;
}

int luaCBClearClipboardText(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    OpenClipboard (g_AppHWND) ;
    EmptyClipboard();
    CloseClipboard();
#endif
    return 0;
}

void luaInitCallbacksClipboard()
{
    lua_register(g_pLuaState, "getClipboardText",      luaCBGetClipboardText);
    lua_register(g_pLuaState, "setClipboardText",      luaCBSetClipboardText);
    lua_register(g_pLuaState, "clearClipboardText",    luaCBClearClipboardText);
}
