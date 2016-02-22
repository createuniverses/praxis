
// Author:  Greg "fugue" Santucci, 2011
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/

#include "luaCB.h"

void luaInitCallbacksOpenGL();
void luaInitCallbacksEditor();
void luaInitCallbacksServer();
void luaInitCallbacksMp3();
void luaInitCallbacksWindow();
void luaInitCallbacksClipboard();
void luaInitCallbacksVoxel();
void luaInitCallbacksSynth();
void luaInitCallbacksMidi();
void luaInitCallbacksWorld();
void luaInitCallbacksLang();
void luaInitCallbacksJoystick();
void luaInitCallbacksSystem();

void luaInitCallbacksTransformLib();
void luaInitCallbacksTextureLib();

void luaInitCallbacks()
{
//    g_luaFont = QFont("Bitstream Vera Sans Mono", 12);

    luaCall("function init() end");
    luaCall("function update() end");
    luaCall("function render() end");
    luaCall("function prerender() end");
    luaCall("function postrender() end");

    luaCall("function render() "
              "for i=0,100,5 do "
                "drawLine(i,0,0,  0,0,100-i )"
              "end "
            "end");

    luaCall("function rendercursor() end");

    luaCall("function print2(...) "
              "local arg = {...} "
              "for i,v in ipairs(arg) do "
                "if type(v) ~= \"table\" then insertBufferText(v .. \"\\n\") end "
              "end "
            "end");

    luaCall("function LMBDown(x,y) end");
    luaCall("function LMBUp(x,y) end");
    luaCall("function RMBDown(x,y) end");
    luaCall("function RMBUp(x,y) end");
    luaCall("function OnMouseMove(dx,dy,px,py) end");

    luaCall("function keyDown(k) end");
    luaCall("function keyUp(k) end");

    luaCall("function logLiveCode(sCode) end");

    luaCall("function f1Pressed() "
              "luaCall(getBufferText()) "
            "end");
    luaCall("function f2Pressed() "
            "newBuffer(getBufferName() .. \" - command\") "
            "end");
    luaCall("function f3Pressed() "
              "local filename = getSelectedText() "
              "newBuffer() "
              "loadBuffer(filename) "
            "end");
    luaCall("function f4Pressed() "
            "end");
    luaCall("function f5Pressed() end");
    luaCall("function f6Pressed() end");
    luaCall("function f7Pressed() end");
    luaCall("function f8Pressed() end");
    luaCall("function f9Pressed() end");
    luaCall("function f10Pressed() end");
    luaCall("function f11Pressed() end");
    luaCall("function f12Pressed() end");

    // luaCall("function midiUpdate() print(\"midiUpdate() called\") end");
    luaCall("function midiUpdate() end");

    luaCall("function getProdMp3Name() return \"\" end");

    // Lua 5.1:
    //luaCall("table.unpack = unpack");
    //luaCall("load = loadstring");

    // Lua 5.2:
    luaCall("unpack = table.unpack");
    luaCall("loadstring = load");

    luaInitCallbacksOpenGL();
    luaInitCallbacksEditor();
    luaInitCallbacksServer();
    luaInitCallbacksMp3();
    luaInitCallbacksWindow();
    luaInitCallbacksClipboard();
    luaInitCallbacksVoxel();
    luaInitCallbacksSynth();
    luaInitCallbacksMidi();
    luaInitCallbacksWorld();
    luaInitCallbacksLang();
    luaInitCallbacksJoystick();
    luaInitCallbacksSystem();

    luaInitCallbacksTransformLib();
    luaInitCallbacksTextureLib();

    // Clear the stack with a lua_pop call here
    lua_pop(g_pLuaState, lua_gettop(g_pLuaState));
}
