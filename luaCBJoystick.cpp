
// Author:  Greg "fugue" Santucci, 2011
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/

#include "luaCB.h"

int luaCBGetJoyAxis(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    JOYINFOEX joyInfo;
    //joyInfo.dwSize = 3000;
    joyInfo.dwSize = sizeof(JOYINFOEX);
    joyInfo.dwFlags = JOY_RETURNALL;
    MMRESULT joyresult = joyGetPosEx(JOYSTICKID1, &joyInfo);

    if(joyresult == JOYERR_NOERROR)
    {
        float fScale = 1.0f / 32.768f;

        lua_pushnumber(L, (float)((int)joyInfo.dwXpos - 32768) * fScale);
        lua_pushnumber(L, (float)((int)joyInfo.dwYpos - 32768) * fScale);
        lua_pushnumber(L, (float)((int)joyInfo.dwRpos - 32768) * fScale);
    }
    else
    {
        lua_pushnumber(L, 0);
        lua_pushnumber(L, 0);
        lua_pushnumber(L, 0);
    }
#else
    lua_pushnumber(L, 0);
    lua_pushnumber(L, 0);
    lua_pushnumber(L, 0);
#endif

    return 3;
}

int luaCBGetJoyThrottle(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    JOYINFOEX joyInfo;
    //joyInfo.dwSize = 3000;
    joyInfo.dwSize = sizeof(JOYINFOEX);
    joyInfo.dwFlags = JOY_RETURNALL;
    MMRESULT joyresult = joyGetPosEx(JOYSTICKID1, &joyInfo);

    if(joyresult == JOYERR_NOERROR)
    {
        float fScale = 1.0f / 32.768f;

        lua_pushnumber(L, (float)(joyInfo.dwZpos - 32768) * fScale);
    }
    else
    {
        lua_pushnumber(L, 0);
    }
#else
    lua_pushnumber(L, 0);
#endif

    return 1;
}

void luaInitCallbacksJoystick()
{
    lua_register(g_pLuaState, "getJoyAxis",            luaCBGetJoyAxis);
    lua_register(g_pLuaState, "getJoyThrottle",        luaCBGetJoyThrottle);
}
