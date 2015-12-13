
// Author:  Greg "fugue" Santucci, 2011
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/

#include "luaCB.h"

int luaSetFloorGrid(lua_State *L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    int nShow = lua_toboolean(L, 1);

    if(nShow)
        g_pWorld->m_bRenderFloorGrid = true;
    else
        g_pWorld->m_bRenderFloorGrid = false;

    return 0;
}

int luaSetProbesHUD(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    int nShow = lua_toboolean(L, 1);

    if(nShow)
        g_pWorld->m_bRenderProbesHUD = true;
    else
        g_pWorld->m_bRenderProbesHUD = false;

    return 0;
}

int luaSetFloorBox(lua_State *L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    int nShow = lua_toboolean(L, 1);

    if(nShow)
        g_pWorld->m_bRenderFloorPlane = true;
    else
        g_pWorld->m_bRenderFloorPlane = false;

    return 0;
}

int luaSetPickSphere(lua_State *L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    int nShow = lua_toboolean(L, 1);

    if(nShow)
        g_pWorld->m_bRenderMousePickSphere = true;
    else
        g_pWorld->m_bRenderMousePickSphere = false;

    return 0;
}

int luaCBEnableStdMouseCam(lua_State *L)
{
    g_pWorld->EnableStdMouseCam();

    return 0;
}

int luaCBDisableStdMouseCam(lua_State *L)
{
    g_pWorld->DisableStdMouseCam();

    return 0;
}

int luaCBGetMouseCursorPos(lua_State *L)
{
    mlVector3D vPos = g_pWorld->GetMousePickPosition();

    lua_pushnumber(L, vPos.x);
    lua_pushnumber(L, vPos.y);
    lua_pushnumber(L, vPos.z);

    return 3;
}

int luaCBLeftMouseDown(lua_State * L)
{
    lua_pushboolean(L, g_pWorld->LeftMouseDown());
    return 1;
}

int luaCBLeftMouseWentDown(lua_State * L)
{
    lua_pushboolean(L, g_pWorld->LeftMouseWentDown());
    return 1;
}

int luaCBRightMouseDown(lua_State * L)
{
    lua_pushboolean(L, g_pWorld->RightMouseDown());
    return 1;
}

int luaCBRightMouseWentDown(lua_State * L)
{
    lua_pushboolean(L, g_pWorld->RightMouseWentDown());
    return 1;
}

int luaCBGetMaxFramerate(lua_State * L)
{
    //extern float g_fSingleWorldTimeBwFrames;
    //lua_pushnumber(L, 1.0f / g_fSingleWorldTimeBwFrames);
    extern int g_nSingleWorldFramerate;
    lua_pushnumber(L, 1.0f / g_nSingleWorldFramerate);
    return 1;
}

int luaCBSetMaxFramerate(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    extern int g_nSingleWorldFramerate;
    g_nSingleWorldFramerate = luaL_checknumber(L, 1);

    //extern float g_fSingleWorldTimeBwFrames;
    //g_fSingleWorldTimeBwFrames = 1.0f / luaL_checknumber(L, 1);
    return 0;
}

int luaCBShowTrace(lua_State * L)
{
    g_pWorld->ShowOutput();
    return 0;
}

int luaCBHideTrace(lua_State * L)
{
    g_pWorld->HideOutput();
    return 0;
}

int luaCBClearTrace(lua_State * L)
{
    PraxisLog::trace = "";
    return 0;
}

int luaCBGetTraceText(lua_State * L)
{
    lua_pushstring(L, PraxisLog::trace.c_str());
    return 1;
}

int luaCBTraceVisible(lua_State * L)
{
    lua_pushboolean(L, g_pWorld->OutputVisible());
    return 1;
}

int luaCBShowError(lua_State * L)
{
    g_pWorld->ShowError();
    return 0;
}

int luaCBHideError(lua_State * L)
{
    g_pWorld->HideError();
    return 0;
}

int luaCBClearError(lua_State * L)
{
    PraxisLog::error = "";
    return 0;
}

int luaCBGetErrorText(lua_State * L)
{
    lua_pushstring(L, PraxisLog::error.c_str());
    return 1;
}

int luaCBErrorVisible(lua_State * L)
{
    lua_pushboolean(L, g_pWorld->ErrorVisible());
    return 1;
}

int luaCBShowEditor(lua_State * L)
{
    g_pWorld->ShowEditor();
    return 0;
}

int luaCBHideEditor(lua_State * L)
{
    g_pWorld->HideEditor();
    return 0;
}

int luaCBEditorVisible(lua_State * L)
{
    lua_pushboolean(L, g_pWorld->EditorVisible());
    return 1;
}

int luaCBSetKeyRepeat(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    int nOption = lua_toboolean(L, 1);

    if(nOption)
        glutSetKeyRepeat(GLUT_KEY_REPEAT_ON);
    else
        glutSetKeyRepeat(GLUT_KEY_REPEAT_OFF);

    return 0;
}

int luaCBNextEditor(lua_State * L)
{
    g_pWorld->NextEditor();
    return 0;
}

int luaCBPreviousEditor(lua_State * L)
{
    g_pWorld->PreviousEditor();
    return 0;
}

int luaCBIsShiftDown(lua_State * L)
{
    bool bShift  = (glutGetModifiers() & GLUT_ACTIVE_SHIFT);
    lua_pushboolean(L, bShift);
    return 1;
}

int luaCBIsCtrlDown(lua_State * L)
{
    bool bCtrl   = (glutGetModifiers() & GLUT_ACTIVE_CTRL);
    lua_pushboolean(L, bCtrl);
    return 1;
}

int luaCBIsAltDown(lua_State * L)
{
    bool bAlt    = (glutGetModifiers() & GLUT_ACTIVE_ALT);
    lua_pushboolean(L, bAlt);
    return 1;
}

int luaCBShowFPS(lua_State * L)
{
    g_pWorld->m_bRenderFPS = true;
    return 0;
}

int luaCBHideFPS(lua_State * L)
{
    g_pWorld->m_bRenderFPS = false;
    return 0;
}

int luaCBPause(lua_State * L)
{
    g_pWorld->Pause();
    return 0;
}

int luaCBContinue(lua_State * L)
{
    g_pWorld->Continue();
    return 0;
}

int luaCBSetClearColor(lua_State * L)
{
    g_pWorld->m_nClearColorRed   = luaL_checknumber(L, 1);
    g_pWorld->m_nClearColorGreen = luaL_checknumber(L, 2);
    g_pWorld->m_nClearColorBlue  = luaL_checknumber(L, 3);

    if(g_pWorld->m_nClearColorRed < 0)   g_pWorld->m_nClearColorRed = 0;
    if(g_pWorld->m_nClearColorRed > 255) g_pWorld->m_nClearColorRed = 255;
    if(g_pWorld->m_nClearColorGreen < 0)   g_pWorld->m_nClearColorGreen = 0;
    if(g_pWorld->m_nClearColorGreen > 255) g_pWorld->m_nClearColorGreen = 255;
    if(g_pWorld->m_nClearColorBlue < 0)   g_pWorld->m_nClearColorBlue = 0;
    if(g_pWorld->m_nClearColorBlue > 255) g_pWorld->m_nClearColorBlue = 255;

    return 0;
}

int luaCBGetClearColor(lua_State * L)
{
    lua_pushnumber(L, g_pWorld->m_nClearColorRed);
    lua_pushnumber(L, g_pWorld->m_nClearColorGreen);
    lua_pushnumber(L, g_pWorld->m_nClearColorBlue);
    return 3;
}

void luaInitCallbacksWorld()
{
    lua_register(g_pLuaState, "getMaxFramerate",       luaCBGetMaxFramerate);
    lua_register(g_pLuaState, "setMaxFramerate",       luaCBSetMaxFramerate);

    lua_register(g_pLuaState, "enableStdMouseCam",     luaCBEnableStdMouseCam);
    lua_register(g_pLuaState, "disableStdMouseCam",    luaCBDisableStdMouseCam);
    lua_register(g_pLuaState, "getMouseCursorPos",     luaCBGetMouseCursorPos);

    lua_register(g_pLuaState, "getLMBDown",            luaCBLeftMouseDown);
    lua_register(g_pLuaState, "getLMBWentDown",        luaCBLeftMouseWentDown);
    lua_register(g_pLuaState, "getRMBDown",            luaCBRightMouseDown);
    lua_register(g_pLuaState, "getRMBWentDown",        luaCBRightMouseWentDown);

    lua_register(g_pLuaState, "setFloorGrid",          luaSetFloorGrid);
    lua_register(g_pLuaState, "setFloorBox",           luaSetFloorBox);
    lua_register(g_pLuaState, "setPickSphere",         luaSetPickSphere);
    lua_register(g_pLuaState, "setProbesHUD",          luaSetProbesHUD);

    lua_register(g_pLuaState, "showTrace",             luaCBShowTrace);
    lua_register(g_pLuaState, "hideTrace",             luaCBHideTrace);
    lua_register(g_pLuaState, "clearTrace",            luaCBClearTrace);
    lua_register(g_pLuaState, "getTraceText",          luaCBGetTraceText);
    lua_register(g_pLuaState, "traceVisible",          luaCBTraceVisible);

    lua_register(g_pLuaState, "showError",             luaCBShowError);
    lua_register(g_pLuaState, "hideError",             luaCBHideError);
    lua_register(g_pLuaState, "clearError",            luaCBClearError);
    lua_register(g_pLuaState, "getErrorText",          luaCBGetErrorText);
    lua_register(g_pLuaState, "errorVisible",          luaCBErrorVisible);

    lua_register(g_pLuaState, "showEditor",            luaCBShowEditor);
    lua_register(g_pLuaState, "hideEditor",            luaCBHideEditor);
    lua_register(g_pLuaState, "editorVisible",         luaCBEditorVisible);
    lua_register(g_pLuaState, "setKeyRepeat",          luaCBSetKeyRepeat);

    lua_register(g_pLuaState, "nextEditor",            luaCBNextEditor);
    lua_register(g_pLuaState, "previousEditor",        luaCBPreviousEditor);

    lua_register(g_pLuaState, "isAltDown",             luaCBIsAltDown);
    lua_register(g_pLuaState, "isShiftDown",           luaCBIsShiftDown);
    lua_register(g_pLuaState, "isCtrlDown",            luaCBIsCtrlDown);

    lua_register(g_pLuaState, "showFPS",               luaCBShowFPS);
    lua_register(g_pLuaState, "hideFPS",               luaCBHideFPS);

    lua_register(g_pLuaState, "pause",                 luaCBPause);
    lua_register(g_pLuaState, "continue",              luaCBContinue);

    lua_register(g_pLuaState, "setClearColor",         luaCBSetClearColor);
    lua_register(g_pLuaState, "getClearColor",         luaCBGetClearColor);
}
