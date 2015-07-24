
// Author:  Greg "fugue" Santucci, 2011
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/

#include "luaCB.h"

int g_nMidiUpdateInterval = 20;

#ifdef __PRAXIS_WINDOWS__
// may need to start a thread to play midi
// so that it is played accurately.
extern int PlayMidiTest();
#endif

int luaCBPlayMidi(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    PlayMidiTest();
#endif
    return 0;
}

#ifdef __PRAXIS_WINDOWS__
extern void StartMidi();
extern void StopMidi();
extern void MidiNoteOn(unsigned char nNote, unsigned char nVolume);
extern void MidiNoteOff(unsigned char nNote);
extern void SelectMidiInstrument(unsigned char nInstrument);
void CALLBACK midiPraxisTimerFunc (UINT, UINT, DWORD, DWORD, DWORD);
extern UINT uTimerRes;
#endif

int luaCBMidiStart(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    StartMidi();
    g_nMidiUpdateInterval = max((UINT) uTimerRes, (UINT) g_nMidiUpdateInterval);
    timeSetEvent(g_nMidiUpdateInterval, uTimerRes, midiPraxisTimerFunc, 0, TIME_ONESHOT);
#endif

#ifdef __PRAXIS_LINUX__
    // 0 is midi thru
    // 1 is Timidity
    int nPortNum = 1;

    if(lua_gettop(L) >= 1)
        nPortNum = luaL_checknumber(L, 1);

    if(nPortNum >= 0 && nPortNum < g_pWorld->m_midiout->getPortCount())
        g_pWorld->m_midiout->openPort(nPortNum);
#endif

    return 0;
}

int luaCBMidiStop(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    StopMidi();
#endif

#ifdef __PRAXIS_LINUX__
    g_pWorld->m_midiout->closePort();
    g_pWorld->m_midiin->closePort();
#endif

    return 0;
}

int luaCBMidiNoteOn(lua_State * L)
{
    int n = lua_gettop(L);

    unsigned char nNote   = luaL_checknumber(L, 1);
    unsigned char nVolume = 127;
    if(n==2)
        nVolume = luaL_checknumber(L, 2);

#ifdef __PRAXIS_WINDOWS__
    MidiNoteOn(nNote, nVolume);
#endif

#ifdef __PRAXIS_LINUX__
    std::vector<unsigned char> msg;
    msg.push_back(0x90);
    msg.push_back(nNote);
    msg.push_back(nVolume);
    g_pWorld->m_midiout->sendMessage(&msg);
#endif

    return 0;
}

int luaCBMidiNoteOff(lua_State * L)
{
    unsigned char nNote = luaL_checknumber(L, 1);

#ifdef __PRAXIS_WINDOWS__
    MidiNoteOff(nNote);
#endif

#ifdef __PRAXIS_LINUX__
    std::vector<unsigned char> msg;
    msg.push_back(0x90);
    msg.push_back(nNote);
    msg.push_back(0);
    g_pWorld->m_midiout->sendMessage(&msg);
#endif

    return 0;
}

int luaCBMidiOpenInputPort(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
#endif

#ifdef __PRAXIS_LINUX__
    // 0 is midi thru
    // 1 is Timidity
    int nPortNum = 1;

    if(lua_gettop(L) >= 1)
        nPortNum = luaL_checknumber(L, 1);

    if(nPortNum >= 0 && nPortNum < g_pWorld->m_midiin->getPortCount())
        g_pWorld->m_midiin->openPort(nPortNum);
#endif

    return 0;
}

int luaCBMidiGetInputPortCount(lua_State * L)
{
    int nCount = g_pWorld->m_midiin->getPortCount();
    lua_pushnumber(L, nCount);
    return 1;
}

int luaCBMidiGetInputPortName(lua_State * L)
{
    int nPortNumber = luaL_checknumber(L, 1);
    std::string sName = g_pWorld->m_midiin->getPortName(nPortNumber);
    lua_pushstring(L, sName.c_str());
    return 1;
}

int luaCBMidiInputPortMessage(lua_State * L)
{
    std::vector<unsigned char> message;
    g_pWorld->m_midiin->getMessage(&message);
    for(int i = 0; i < message.size(); i++)
    {
        lua_pushnumber(L, message[i]);
    }
    return message.size();
}

int luaCBMidiSelectInstrument(lua_State * L)
{
    unsigned char nInstrument = luaL_checknumber(L, 1);

#ifdef __PRAXIS_WINDOWS__
    SelectMidiInstrument(nInstrument);
#endif

#ifdef __PRAXIS_LINUX__
    std::vector<unsigned char> msg;
    msg.push_back(0x0C0);
    msg.push_back(nInstrument);
    g_pWorld->m_midiout->sendMessage(&msg);
#endif

    return 0;
}

#ifdef __PRAXIS_WINDOWS__
void CALLBACK midiPraxisTimerFunc (UINT, UINT, DWORD, DWORD, DWORD)
{
    if(g_bLuaRunning)
        luaCall("midiUpdate()");

    timeSetEvent(g_nMidiUpdateInterval, uTimerRes, midiPraxisTimerFunc, 0, TIME_ONESHOT);
}

#endif

int luaCBMidiGetInterval(lua_State * L)
{
    lua_pushnumber(L, g_nMidiUpdateInterval);
    return 1;
}

int luaCBMidiSetInterval(lua_State * L)
{
    int nInterval = luaL_checknumber(L, 1);
    g_nMidiUpdateInterval = nInterval;
#ifdef __PRAXIS_WINDOWS__
    g_nMidiUpdateInterval = max((UINT) uTimerRes, (UINT) g_nMidiUpdateInterval);
#endif
    lua_pushnumber(L, g_nMidiUpdateInterval);
    return 1;
}

// Leave it in for now
int luaCBMidiLaunchNextEvent(lua_State * L)
{
    return 0;
}

int luaCBMidiGetPortCount(lua_State * L)
{
    int nCount = g_pWorld->m_midiout->getPortCount();
    lua_pushnumber(L, nCount);
    return 1;
}

int luaCBMidiGetPortName(lua_State * L)
{
    int nPortNumber = luaL_checknumber(L, 1);
    std::string sName = g_pWorld->m_midiout->getPortName(nPortNumber);
    lua_pushstring(L, sName.c_str());
    return 1;
}

void luaInitCallbacksMidi()
{
    lua_register(g_pLuaState, "playMidi",               luaCBPlayMidi);
    lua_register(g_pLuaState, "midiStart",              luaCBMidiStart);
    lua_register(g_pLuaState, "midiStop",               luaCBMidiStop);
    lua_register(g_pLuaState, "midiNoteOn",             luaCBMidiNoteOn);
    lua_register(g_pLuaState, "midiNoteOff",            luaCBMidiNoteOff);
    lua_register(g_pLuaState, "midiSelectInstrument",   luaCBMidiSelectInstrument);
    lua_register(g_pLuaState, "midiLaunchNextEvent",    luaCBMidiLaunchNextEvent);

    lua_register(g_pLuaState, "midiGetInterval",        luaCBMidiGetInterval);
    lua_register(g_pLuaState, "midiSetInterval",        luaCBMidiSetInterval);

    lua_register(g_pLuaState, "midiGetPortCount",       luaCBMidiGetPortCount);
    lua_register(g_pLuaState, "midiGetPortName",        luaCBMidiGetPortName);

    lua_register(g_pLuaState, "midiOpenInputPort",      luaCBMidiOpenInputPort);
    lua_register(g_pLuaState, "midiGetInputPortCount",  luaCBMidiGetInputPortCount);
    lua_register(g_pLuaState, "midiGetInputPortName",   luaCBMidiGetInputPortName);
    lua_register(g_pLuaState, "midiInputPortMessage",   luaCBMidiInputPortMessage);
}
