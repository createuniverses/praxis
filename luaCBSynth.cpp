
// Author:  Greg "fugue" Santucci, 2011
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/

#include "luaCB.h"

extern void StartPlayingSound();
extern void StopPlayingSound();

int luaCBPlaySound(lua_State * L)
{
    StartPlayingSound();
    return 0;
}

int luaCBStopSound(lua_State * L)
{
    StopPlayingSound();
    return 0;
}

int luaCBSoundPlaying(lua_State * L)
{
#ifdef __PRAXIS_LINUX__
    extern bool g_bSDLAudioPlaying;
    lua_pushboolean(L, g_bSDLAudioPlaying);
#else
    bool bSoundPlaying = false;
    lua_pushboolean(L, bSoundPlaying);
#endif
    return 1;
}

extern const int SAMPLE_RATE;
extern const int SAMPLES_PER_REQUEST;

int luaCBGetSampleRate(lua_State * L)
{
    lua_pushnumber(L, SAMPLE_RATE);
    return 1;
}

int luaCBGetSamplesPerRequest(lua_State * L)
{
    lua_pushnumber(L, SAMPLES_PER_REQUEST);
    return 1;
}

extern short ReadLiveBufferSample();
extern void WriteLiveBufferSample(short nSample);
extern int nLiveBufferReadPosition;
extern int nLiveBufferWritePosition;
extern int nLiveBufferTotalReads;
extern int nLiveBufferTotalWrites;

int luaCBReadLiveBufferSample(lua_State * L)
{
    short nSample = ReadLiveBufferSample();
    lua_pushnumber(L, nSample);
    return 1;
}

int luaCBWriteLiveBufferSample(lua_State * L)
{
    short nSample = luaL_checknumber(L, 1);
    WriteLiveBufferSample(nSample);

    return 0;
}

int luaCBGetLiveBufferMarkers(lua_State * L)
{
    lua_pushnumber(L, nLiveBufferReadPosition);
    lua_pushnumber(L, nLiveBufferWritePosition);
    lua_pushnumber(L, nLiveBufferTotalReads);
    lua_pushnumber(L, nLiveBufferTotalWrites);

    return 4;
}

void luaInitCallbacksSynth()
{
    lua_register(g_pLuaState, "playSound",              luaCBPlaySound);
    lua_register(g_pLuaState, "stopSound",              luaCBStopSound);
    lua_register(g_pLuaState, "soundPlaying",           luaCBSoundPlaying);

    lua_register(g_pLuaState, "getSampleRate",          luaCBGetSampleRate);
    lua_register(g_pLuaState, "getSamplesPerRequest",   luaCBGetSamplesPerRequest);

    lua_register(g_pLuaState, "readSample",             luaCBReadLiveBufferSample);
    lua_register(g_pLuaState, "writeSample",            luaCBWriteLiveBufferSample);

    lua_register(g_pLuaState, "getSampleMarkers",       luaCBGetLiveBufferMarkers);
}
