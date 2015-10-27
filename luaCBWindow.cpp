
// Author:  Greg "fugue" Santucci, 2011
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/

#include "luaCB.h"

int luaCBGetScreenWidth(lua_State *L)
{
    int nWidth = glutGet(GLUT_SCREEN_WIDTH);
    lua_pushnumber(L, nWidth);
    return 1;
}

int luaCBGetScreenHeight(lua_State *L)
{
    int nHeight = glutGet(GLUT_SCREEN_HEIGHT);
    lua_pushnumber(L, nHeight);
    return 1;
}

int luaCBGetWindowWidth(lua_State *L)
{
    int nWidth = glutGet(GLUT_WINDOW_WIDTH);
    lua_pushnumber(L, nWidth);
    return 1;
}

int luaCBGetWindowHeight(lua_State *L)
{
    int nHeight = glutGet(GLUT_WINDOW_HEIGHT);
    lua_pushnumber(L, nHeight);
    return 1;
}

int luaCBGetMaximizedWindowWidth(lua_State *L)
{
#ifdef __PRAXIS_LINUX__
    int nWidth = glutGet(GLUT_SCREEN_WIDTH);
#endif

#ifdef __PRAXIS_WINDOWS__
    int nWidth = GetSystemMetrics(SM_CXMAXIMIZED);
#endif

    lua_pushnumber(L, nWidth);
    return 1;
}

int luaCBGetMaximizedWindowHeight(lua_State *L)
{
#ifdef __PRAXIS_LINUX__
    int nHeight = glutGet(GLUT_SCREEN_HEIGHT);
#endif

#ifdef __PRAXIS_WINDOWS__
    int nHeight = GetSystemMetrics(SM_CYMAXIMIZED);
#endif

    lua_pushnumber(L, nHeight);
    return 1;
}

int luaCBTurnOnBorders(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    RECT rect;
    GetWindowRect(glutGetWindowHandle(), &rect);

    SetWindowLongPtr(glutGetWindowHandle(), GWL_STYLE, WS_VISIBLE | WS_CLIPSIBLINGS | WS_CLIPCHILDREN | WS_POPUP | WS_OVERLAPPEDWINDOW);

    SetWindowPos(
        glutGetWindowHandle(),
        HWND_TOP,
        rect.left,
        rect.top,
        rect.right  - rect.left,
        rect.bottom - rect.top,
        SWP_NOACTIVATE | SWP_NOOWNERZORDER | SWP_NOSENDCHANGING |
        SWP_NOZORDER | SWP_FRAMECHANGED);

    rect.right  -= GetSystemMetrics( SM_CXSIZEFRAME ) * 2;
    rect.bottom -= GetSystemMetrics( SM_CYSIZEFRAME ) * 2 + GetSystemMetrics( SM_CYCAPTION );

#ifdef SM_CXPADDEDBORDER
    rect.right -= GetSystemMetrics( SM_CXPADDEDBORDER ) * 2;
    rect.bottom -= GetSystemMetrics( SM_CXPADDEDBORDER ) * 2;
#endif

    glutReshapeWindow(rect.right - rect.left, rect.bottom - rect.top);
#endif
    return 0;
}

int luaCBGetPaddedBorder(lua_State * L)
{
#ifdef __PRAXIS_WINDOW__
    lua_pushnumber(L, GetSystemMetrics( SM_CXPADDEDBORDER ));
#endif
#ifdef __PRAXIS_LINUX__
    lua_pushnumber(L, 0);
#endif
    return 1;
}

int luaCBTurnOffBorders(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    RECT rect;
    GetWindowRect(glutGetWindowHandle(), &rect);

    glutReshapeWindow(rect.right - rect.left, rect.bottom - rect.top);

    DWORD flags = WS_VISIBLE | WS_CLIPSIBLINGS | WS_CLIPCHILDREN | WS_POPUP;

    SetWindowLongPtr(glutGetWindowHandle(), GWL_STYLE, flags);

    SetWindowPos(
        glutGetWindowHandle(),
        HWND_TOP,
        rect.left,
        rect.top,
        rect.right  - rect.left,
        rect.bottom - rect.top,
        SWP_NOACTIVATE | SWP_NOOWNERZORDER | SWP_NOSENDCHANGING |
        SWP_NOZORDER | SWP_FRAMECHANGED
    );
#endif
    return 0;
}

int luaCBFullscreenMode(lua_State *L)
{
    glutFullScreen();

    return 0;
}

int luaCBWindowedMode(lua_State *L)
{
    int nLeft   = 100;
    int nTop    = 100;
    int nWidth  = 800;
    int nHeight = 600;

    int n = lua_gettop(L);
    if(n==4)
    {
        nLeft   = luaL_checknumber(L, 1);
        nTop    = luaL_checknumber(L, 2);
        nWidth  = luaL_checknumber(L, 3);
        nHeight = luaL_checknumber(L, 4);
    }
    else if(n==2)
    {
        nLeft   = luaL_checknumber(L, 1);
        nTop    = luaL_checknumber(L, 2);
    }

#ifdef __PRAXIS_WINDOWS__
    DWORD flags = GetWindowLong(glutGetWindowHandle(), GWL_STYLE);
    if(flags & WS_OVERLAPPEDWINDOW)
    {
        nWidth  -= GetSystemMetrics( SM_CXSIZEFRAME ) * 2;
        nHeight -= GetSystemMetrics( SM_CYSIZEFRAME ) * 2 + GetSystemMetrics( SM_CYCAPTION );
    }
#endif

    glutPositionWindow(nLeft,nTop);
    glutReshapeWindow(nWidth,nHeight);

    return 0;
}

int luaCBGetWindowRect(lua_State * L)
{
    int x = glutGet(GLUT_WINDOW_X);
    int y = glutGet(GLUT_WINDOW_Y);
    int w = glutGet(GLUT_WINDOW_WIDTH);
    int h = glutGet(GLUT_WINDOW_HEIGHT);

    lua_pushnumber(L, x);
    lua_pushnumber(L, y);
    lua_pushnumber(L, w);
    lua_pushnumber(L, h);

    return 4;
}

void luaInitCallbacksWindow()
{
    lua_register(g_pLuaState, "getScreenWidth",           luaCBGetScreenWidth);
    lua_register(g_pLuaState, "getScreenHeight",          luaCBGetScreenHeight);
    lua_register(g_pLuaState, "getWindowWidth",           luaCBGetWindowWidth);
    lua_register(g_pLuaState, "getWindowHeight",          luaCBGetWindowHeight);
    lua_register(g_pLuaState, "getMaximizedWindowWidth",  luaCBGetMaximizedWindowWidth);
    lua_register(g_pLuaState, "getMaximizedWindowHeight", luaCBGetMaximizedWindowHeight);

    lua_register(g_pLuaState, "getWindowRect",         luaCBGetWindowRect);

#ifdef __PRAXIS_LINUX__
    luaCall("function getWinScreenWidth() return getMaximizedWindowWidth() - 6 end");
    luaCall("function getWinScreenHeight() return getMaximizedWindowHeight() - 60 end");
#endif

#ifdef __PRAXIS_WINDOWS__
    luaCall("function getWinScreenWidth() return getMaximizedWindowWidth() - 16 end");
    luaCall("function getWinScreenHeight() return getMaximizedWindowHeight() - 16 end");
#endif

    lua_register(g_pLuaState, "fullscreenMode",        luaCBFullscreenMode);
    lua_register(g_pLuaState, "windowedMode",          luaCBWindowedMode);
    lua_register(g_pLuaState, "turnOnBorders",         luaCBTurnOnBorders);
    lua_register(g_pLuaState, "turnOffBorders",        luaCBTurnOffBorders);

    lua_register(g_pLuaState, "dbgGetPaddedBorder",    luaCBGetPaddedBorder);
}
