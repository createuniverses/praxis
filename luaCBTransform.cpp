
// Author:  Greg "fugue" Santucci, 2015
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/

#include "luaCB.h"

int luaCBTransformNew(lua_State * L)
{
    mlTransform ** t = (mlTransform **)lua_newuserdata(L, sizeof(mlTransform *));

    *t = new mlTransform();

    luaL_getmetatable(L, "LiveCode.transform");
    lua_setmetatable(L, -2);

    return 1;
}

mlTransform g_trnIdentity;

int luaCBTransformIdentity(lua_State * L)
{
    mlTransform ** t = (mlTransform **)lua_newuserdata(L, sizeof(mlTransform *));

    (*t) = &g_trnIdentity;

    luaL_getmetatable(L, "LiveCode.transform");
    lua_setmetatable(L, -2);

    g_trnIdentity = mlTransform();

    return 1;
}

int luaCBTransformCamera(lua_State * L)
{
    mlTransform ** t = (mlTransform **)lua_newuserdata(L, sizeof(mlTransform *));

    // Refer to the existing camera.
    (*t) = g_pWorld->GetCameraTransform();

    luaL_getmetatable(L, "LiveCode.transform");
    lua_setmetatable(L, -2);

    return 1;
}

int luaCBTransformCameraBase(lua_State * L)
{
    mlTransform ** t = (mlTransform **)lua_newuserdata(L, sizeof(mlTransform *));

    // Refer to the existing camera base.
    (*t) = g_pWorld->GetCameraTransformBase();

    luaL_getmetatable(L, "LiveCode.transform");
    lua_setmetatable(L, -2);

    return 1;
}

int luaCBTransformSetTranslation(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=4) luaL_error(L, "4 arguments expected.");

    mlTransform * t = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");

    mlVector3D vTranslation(luaL_checknumber(L, 2), luaL_checknumber(L, 3), luaL_checknumber(L, 4));

    t->SetTranslation(vTranslation);

    return 0;
}

int luaCBTransformGetTranslation(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    mlTransform * t = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");

    mlVector3D vTranslation = t->GetTranslation();

    lua_pushnumber(L, vTranslation.x);
    lua_pushnumber(L, vTranslation.y);
    lua_pushnumber(L, vTranslation.z);

    return 3;
}

int luaCBTransformApplyTranslation(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=4) luaL_error(L, "4 arguments expected.");

    mlTransform * t = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");

    mlVector3D vTranslation(luaL_checknumber(L, 2), luaL_checknumber(L, 3), luaL_checknumber(L, 4));

    t->ApplyTranslation(vTranslation);

    return 0;
}

int luaCBTransformGetScale(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    mlTransform * t = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");

    mlVector3D vScale = t->GetScale();

    lua_pushnumber(L, vScale.x);
    lua_pushnumber(L, vScale.y);
    lua_pushnumber(L, vScale.z);

    return 3;
}

int luaCBTransformSetScale(lua_State * L)
{
    int n = lua_gettop(L);
    if(n == 4)
    {
        mlTransform * t = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");

        mlVector3D vScale(luaL_checknumber(L, 2), luaL_checknumber(L, 3), luaL_checknumber(L, 4));

        t->SetScale(vScale);
    }
    else if(n == 2)
    {
        mlTransform * t = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");

        float fScale = luaL_checknumber(L, 2);

        t->SetScale(fScale);
    }
    else
    {
        luaL_error(L, "4 or 2 arguments expected.");
    }

    return 0;
}

int luaCBTransformApplyScale(lua_State * L)
{
    int n = lua_gettop(L);
    if(n == 4)
    {
        mlTransform * t = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");

        mlVector3D vScale(luaL_checknumber(L, 2), luaL_checknumber(L, 3), luaL_checknumber(L, 4));

        t->ApplyScale(vScale);
    }
    else if(n == 2)
    {
        mlTransform * t = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");

        float fScale = luaL_checknumber(L, 2);

        t->ApplyScale(fScale);
    }
    else
    {
        luaL_error(L, "4 or 2 arguments expected.");
    }

    return 0;
}

int luaCBTransformUp(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    mlTransform * t = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");

    lua_pushnumber(L, t->GetMatrix().J.x);
    lua_pushnumber(L, t->GetMatrix().J.y);
    lua_pushnumber(L, t->GetMatrix().J.z);

    return 3;
}

int luaCBTransformForward(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    mlTransform * t = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");

    lua_pushnumber(L, t->GetMatrix().K.x);
    lua_pushnumber(L, t->GetMatrix().K.y);
    lua_pushnumber(L, t->GetMatrix().K.z);

    return 3;
}

int luaCBTransformSide(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    mlTransform * t = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");

    lua_pushnumber(L, t->GetMatrix().I.x);
    lua_pushnumber(L, t->GetMatrix().I.y);
    lua_pushnumber(L, t->GetMatrix().I.z);

    return 3;
}

int luaCBTransformNormalise(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument (the transform) expected.");
    mlTransform * t = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");
    t->Normalise();
    return 0;
}

int luaCBTransformLookAt(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=4 && n!=7) luaL_error(L, "4 or 7 arguments expected.");

    mlTransform * t = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");

    mlVector3D vFocalPoint(luaL_checknumber(L, 2), luaL_checknumber(L, 3), luaL_checknumber(L, 4));

    mlVector3D vForward = vFocalPoint - t->GetTranslation();
    //mlVector3D vForward = t->GetTranslation() - vFocalPoint;

    vForward.Normalise();

    if(n>4)
    {
        mlVector3D vUp(luaL_checknumber(L, 5), luaL_checknumber(L, 6), luaL_checknumber(L, 7));
        mlQuaternion rotLookAt = mlQuaternionFromDirection(vForward, vUp);
        t->SetRotation(rotLookAt);
    }
    else
    {
        mlQuaternion rotLookAt = mlQuaternionFromDirection(vForward, mlVector3D(0.0f, 1.0f, 0.0f));
        t->SetRotation(rotLookAt);
    }

    return 0;
}

int luaCBTransformRotate(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=3 && n!=5) luaL_error(L, "3 or 5 arguments expected.");

    mlTransform * t = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");

    if(n == 3)
    {
        float yaw   = luaL_checknumber(L, 2);
        float pitch = luaL_checknumber(L, 3);

        t->ApplyRotation(mlQuaternion(mlVector3D(0,1,0), -yaw));

        mlVector3D vSide = t->TransformVector(mlVector3D(1,0,0));

        t->ApplyRotation(mlQuaternion(vSide, pitch));
    }
    else if(n == 5)
    {
        float angle  = luaL_checknumber(L, 2);
        float x      = luaL_checknumber(L, 3);
        float y      = luaL_checknumber(L, 4);
        float z      = luaL_checknumber(L, 5);

        t->ApplyRotation(mlQuaternion(mlVector3D(x,y,z), angle));
    }

    return 0;
}

int luaCBTransformLocalToGlobal(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=4) luaL_error(L, "4 arguments expected.");

    mlTransform * t = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");

    mlVector3D v1(luaL_checknumber(L, 2), luaL_checknumber(L, 3), luaL_checknumber(L, 4));

    mlVector3D v2 = t->TransformPoint(v1);

    lua_pushnumber(L, v2.x);
    lua_pushnumber(L, v2.y);
    lua_pushnumber(L, v2.z);

    return 3;
}

int luaCBTransformGlobalToLocal(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=4) luaL_error(L, "4 arguments expected.");

    mlTransform * t = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");

    mlVector3D v1(luaL_checknumber(L, 2), luaL_checknumber(L, 3), luaL_checknumber(L, 4));

    mlVector3D v2 = t->TransformPointInverse(v1);

    lua_pushnumber(L, v2.x);
    lua_pushnumber(L, v2.y);
    lua_pushnumber(L, v2.z);

    return 3;
}

int luaCBTransformTransformVector(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=4) luaL_error(L, "4 arguments expected.");

    mlTransform * t = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");

    mlVector3D v1(luaL_checknumber(L, 2), luaL_checknumber(L, 3), luaL_checknumber(L, 4));

    mlVector3D v2 = t->TransformVector(v1);

    lua_pushnumber(L, v2.x);
    lua_pushnumber(L, v2.y);
    lua_pushnumber(L, v2.z);

    return 3;
}

int luaCBTransformTransformVectorInverse(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=4) luaL_error(L, "4 arguments expected.");

    mlTransform * t = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");

    mlVector3D v1(luaL_checknumber(L, 2), luaL_checknumber(L, 3), luaL_checknumber(L, 4));

    mlVector3D v2 = t->TransformVectorInverse(v1);

    lua_pushnumber(L, v2.x);
    lua_pushnumber(L, v2.y);
    lua_pushnumber(L, v2.z);

    return 3;
}

int luaCBTransformCopyFrom(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=2) luaL_error(L, "2 arguments expected.");

    mlTransform * t1 = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");
    mlTransform * t2 = *(mlTransform **)luaL_checkudata(L, 2, "LiveCode.transform");

    (*t1) = (*t2);

    return 0;
}

#if 0
int luaCBTransformSet(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=17) luaL_error(L, "17 arguments expected.");

    // 17 arguments.
    // One for the transform, 16 for the numbers.

    mlTransform * t = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");

    mlMatrix3x4 m;
    m.I = mlVector3D(luaL_checknumber(L, 2), luaL_checknumber(L, 3), luaL_checknumber(L, 4));
    // etc.

    //t->SetMatrix(m);
    return 0;
}
#endif

int luaCBTransformTransform(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=2) luaL_error(L, "2 arguments expected.");

    mlTransform * t1 = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");
    mlTransform * t2 = *(mlTransform **)luaL_checkudata(L, 2, "LiveCode.transform");

    t1->TransformSelf(*t2);

    return 0;
}

void luaInitCallbacksTransformLib()
{
    const struct luaL_Reg lua_transformlib [] = {
        {"new",                    luaCBTransformNew},
        {"identity",               luaCBTransformIdentity},
        {"camera",                 luaCBTransformCamera},
        {"cameraBase",             luaCBTransformCameraBase},
        {"getTranslation",         luaCBTransformGetTranslation},
        {"setTranslation",         luaCBTransformSetTranslation},
        {"applyTranslation",       luaCBTransformApplyTranslation},
        {"getScale",               luaCBTransformGetScale},
        {"setScale",               luaCBTransformSetScale},
        {"applyScale",             luaCBTransformApplyScale},
        {"forward",                luaCBTransformForward},
        {"up",                     luaCBTransformUp},
        {"side",                   luaCBTransformSide},
        {"lookAt",                 luaCBTransformLookAt},
        {"transform",              luaCBTransformTransform},
        {"translate",              luaCBTransformApplyTranslation},
        {"scale",                  luaCBTransformApplyScale},
        {"rotate",                 luaCBTransformRotate},
        {"localToGlobal",          luaCBTransformLocalToGlobal},
        {"globalToLocal",          luaCBTransformGlobalToLocal},
        {"transformPoint",         luaCBTransformLocalToGlobal},
        {"transformPointInverse",  luaCBTransformGlobalToLocal},
        {"transformVector",        luaCBTransformTransformVector},
        {"transformVectorInverse", luaCBTransformTransformVectorInverse},
        {"copy",                   luaCBTransformCopyFrom},
        //{"set",                    luaCBTransformSet},
        {"normalise",              luaCBTransformNormalise},
        {NULL, NULL}
    };

    // Lua 5.1:
    //luaL_register(g_pLuaState, "transform", lua_transformlib);

    // Lua 5.2:
    lua_newtable(g_pLuaState);
    luaL_setfuncs (g_pLuaState,lua_transformlib,0);
    lua_pushvalue(g_pLuaState,-1);
    lua_setglobal(g_pLuaState,"transform");

    luaL_newmetatable(g_pLuaState, "LiveCode.transform");

    lua_pushstring(g_pLuaState, "__index");
    lua_pushvalue(g_pLuaState, -3);           /* pushes transform */
    lua_settable(g_pLuaState, -3);            /* metatable.__index = transform */
}
