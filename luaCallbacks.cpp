
// Author:  Greg "fugue" Santucci, 2011
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/

#include "luaCB.h"

int luaCBGetMp3Time(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    if(g_pMp3Stream)
    {
        float fTimeMSec = FSOUND_Stream_GetTime(g_pMp3Stream);
        float fTimeSec = fTimeMSec * 0.001f;

        lua_pushnumber(L, fTimeSec);
    }
    else
    {
        lua_pushnumber(L, 0.0f);
    }
#else
    lua_pushnumber(L, 0.0f);
#endif

    return 1;
}

int luaCBSetMp3Time(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    if(g_pMp3Stream)
    {
        float fPosition = luaL_checknumber(L, 1);
        int nPosition = fPosition * 1000;

        FSOUND_Stream_SetTime(g_pMp3Stream, nPosition);
    }
#endif
    return 0;
}

int luaCBGetMp3Length(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    if(g_pMp3Stream)
    {
        float fTimeMSec = FSOUND_Stream_GetLengthMs(g_pMp3Stream);
        fTimeMSec *= 0.001f;

        lua_pushnumber(L, fTimeMSec);
    }
    else
    {
        lua_pushnumber(L, 0.0f);
    }
#else
    lua_pushnumber(L, 0.0f);
#endif
    return 1;
}

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
    glutReshapeWindow(rect.right - rect.left, rect.bottom - rect.top);
#endif
    return 0;
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

int luaCBPlayMp3(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    if(g_pMp3Stream)
        g_nMp3Channel = FSOUND_Stream_Play(FSOUND_FREE, g_pMp3Stream);
#endif
    return 0;
}

int luaCBStopMp3(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    if(g_pMp3Stream)
        FSOUND_Stream_Stop(g_pMp3Stream);
#endif
    return 0;
}

int luaCBIsMp3Playing(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    if(g_nMp3Channel == -1)
    {
        lua_pushboolean(L, false);
    }
    else
    {
        bool bIsPlaying = FSOUND_IsPlaying(g_nMp3Channel);
        lua_pushboolean(L, bIsPlaying);
    }
#else
    lua_pushboolean(L, false);
#endif
    return 1;
}

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

int luaCBMakeVoxelBlock(lua_State * L)
{
    if(g_pVoxelBlock == 0)
        g_pVoxelBlock = new VoxelBlock();

    return 0;
}

int luaCBRenderVoxelBlock(lua_State * L)
{
    if(g_pVoxelBlock)
        g_pVoxelBlock->Render();

    return 0;
}

int luaCBCarveVoxelAt(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=3) luaL_error(L, "3 arguments expected.");

    int x = luaL_checknumber(L, 1);
    int y = luaL_checknumber(L, 2);
    int z = luaL_checknumber(L, 3);

    if(g_pVoxelBlock)
        g_pVoxelBlock->Carve(x,y,z);

    return 0;
}

int luaCBAddVoxelAt(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=3) luaL_error(L, "3 arguments expected.");

    int x = luaL_checknumber(L, 1);
    int y = luaL_checknumber(L, 2);
    int z = luaL_checknumber(L, 3);

    if(g_pVoxelBlock)
        g_pVoxelBlock->Add(x,y,z);

    return 0;
}

int luaCBGetVoxelColour(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=3) luaL_error(L, "3 arguments expected.");

    int x = luaL_checknumber(L, 1);
    int y = luaL_checknumber(L, 2);
    int z = luaL_checknumber(L, 3);

    if(g_pVoxelBlock)
    {
        Voxel * pVoxel = g_pVoxelBlock->GetVoxelAt(x,y,z);

        if(pVoxel)
        {
            lua_pushnumber(L, pVoxel->m_nRed);
            lua_pushnumber(L, pVoxel->m_nGreen);
            lua_pushnumber(L, pVoxel->m_nBlue);
            lua_pushnumber(L, pVoxel->m_nAlpha);

            return 4;
        }
    }

    return 0;
}

int luaCBSetVoxelColour(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=7) luaL_error(L, "7 arguments expected.");

    int x = luaL_checknumber(L, 1);
    int y = luaL_checknumber(L, 2);
    int z = luaL_checknumber(L, 3);

    int red   = luaL_checknumber(L, 4);
    int green = luaL_checknumber(L, 5);
    int blue  = luaL_checknumber(L, 6);
    int alpha = luaL_checknumber(L, 7);

    if(g_pVoxelBlock)
    {
        Voxel * pVoxel = g_pVoxelBlock->GetVoxelAt(x,y,z);

        if(pVoxel)
        {
            pVoxel->m_nRed   = red;
            pVoxel->m_nGreen = green;
            pVoxel->m_nBlue  = blue;
            pVoxel->m_nAlpha = alpha;
        }
    }

    return 0;
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

extern void StartPlayingSound();
extern void StopPlayingSound();

#ifdef __PRAXIS_WINDOWS__
// may need to start a thread to play midi
// so that it is played accurately.
extern int PlayMidiTest();
#endif

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

int luaCBLisp(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    std::string sCode = luaL_checkstring(L, 1);

    lispCall(sCode);

    std::string sOut = lispGetOutput();
    std::string sErr = lispGetError();

    if(sErr != "")
        sOut = sOut + std::string(" ") + sErr;

    lua_pushstring(L, sOut.c_str());
    return 1;
}

int luaCBForth(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    std::string sCode = luaL_checkstring(L, 1);

    forthCall(sCode);

    std::string sOut = forthGetOutput();
    std::string sErr = forthGetError();

    if(sErr != "")
        sOut = sOut + std::string(" ") + sErr;

    lua_pushstring(L, sOut.c_str());

    return 1;
}

int luaCBIoLang(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    std::string sCode = luaL_checkstring(L, 1);

    ioCallWithReply(sCode);

    lua_pushstring(L, ioGetReply().c_str());
    lua_pushstring(L, ioGetTrace().c_str());

    return 2;
}

int luaCBSetLispTraceVerbosity(lua_State * L)
{
    extern int g_nLispTraceVerbosity;

    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    int nVerbosity = luaL_checknumber(L, 1);
    g_nLispTraceVerbosity = nVerbosity;

    return 0;
}

int luaCBGetLispTraceVerbosity(lua_State * L)
{
    extern int g_nLispTraceVerbosity;

//    int n = lua_gettop(L);
//    if(n!=1) luaL_error(L, "1 argument expected.");

    lua_pushnumber(L, g_nLispTraceVerbosity);

    return 1;
}

int luaCBGetCurrentDir(lua_State * L)
{
    return 0;
}

#ifdef __PRAXIS_LINUX__
#include <unistd.h>
int luaCBSetCurrentDir(lua_State * L)
{
    std::string sDir = luaL_checkstring(L, 1);
    chdir(sDir.c_str());
    return 0;
}
#endif

#ifdef __PRAXIS_WINDOWS__
int luaCBSetCurrentDir(lua_State * L)
{
    std::string sDir = luaL_checkstring(L, 1);
    ::SetCurrentDirectory(sDir.c_str());
    return 0;
}
#endif

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

int luaCBShowFPS(lua_State * L)
{
    return 0;
}

int luaCBHideFPS(lua_State * L)
{
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

int luaCBLuaCall(lua_State * L)
{
    std::string sText = luaL_checkstring(L, 1);

    luaCall(sText);

    return 0;
}

void cbLuaBreakHook(lua_State *L, lua_Debug *ar);

int luaCBTurnOnDebugHook(lua_State * L)
{
    int nLines = 1000;

    int n = lua_gettop(L);
    if(n == 1)
        nLines = luaL_checknumber(L, 1);

    lua_sethook(L, cbLuaBreakHook, LUA_MASKCOUNT, nLines);

    return 0;
}

int luaCBTurnOffDebugHook(lua_State * L)
{
    lua_sethook(L, 0, LUA_MASKCOUNT, 1000);
    return 0;
}

int luaCBGetFPS(lua_State * L)
{
    extern float g_fFPS;
    lua_pushnumber(L, g_fFPS);
    return 1;
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

int luaCBTurnOffNativeEditorControl(lua_State * L)
{
    GLEditor::m_bNativeControl = false;
    return 0;
}

int luaCBTurnOnNativeEditorControl(lua_State * L)
{
    GLEditor::m_bNativeControl = true;
    return 0;
}

void luaInitCallbacksGL();
void luaInitCallbacksEditor();
void luaInitCallbacksServer();
void luaInitCallbacksTransformLib();
void luaInitCallbacksTextureLib();

void luaInitCallbacks()
{
//    g_luaFont = QFont("Bitstream Vera Sans Mono", 12);

    luaCall("function init() end");
    luaCall("function update() end");
    luaCall("function render() end");
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

    luaCall("function edKeyDown(k) end");
    luaCall("function edKeyUp(k) end");

    luaCall("function returnPressed() edInsertNewline() end");

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

    luaCall("table.unpack = unpack");
    luaCall("load = loadstring");

    lua_register(g_pLuaState, "getJoyAxis",            luaCBGetJoyAxis);
    lua_register(g_pLuaState, "getJoyThrottle",        luaCBGetJoyThrottle);

    lua_register(g_pLuaState, "luaCall",               luaCBLuaCall);

    luaInitCallbacksGL();
    luaInitCallbacksEditor();
    luaInitCallbacksServer();

    luaInitCallbacksTransformLib();
    luaInitCallbacksTextureLib();

    lua_register(g_pLuaState, "getClipboardText",      luaCBGetClipboardText);
    lua_register(g_pLuaState, "setClipboardText",      luaCBSetClipboardText);
    lua_register(g_pLuaState, "clearClipboardText",    luaCBClearClipboardText);


    lua_register(g_pLuaState, "getMp3Time",            luaCBGetMp3Time);
    lua_register(g_pLuaState, "setMp3Time",            luaCBSetMp3Time);
    lua_register(g_pLuaState, "getMp3Length",          luaCBGetMp3Length);
    lua_register(g_pLuaState, "playMp3",               luaCBPlayMp3);
    lua_register(g_pLuaState, "stopMp3",               luaCBStopMp3);
    lua_register(g_pLuaState, "isMp3Playing",          luaCBIsMp3Playing);

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

    lua_register(g_pLuaState, "getMaxFramerate",       luaCBGetMaxFramerate);
    lua_register(g_pLuaState, "setMaxFramerate",       luaCBSetMaxFramerate);

    lua_register(g_pLuaState, "enableStdMouseCam",     luaCBEnableStdMouseCam);
    lua_register(g_pLuaState, "disableStdMouseCam",    luaCBDisableStdMouseCam);
    lua_register(g_pLuaState, "getMouseCursorPos",     luaCBGetMouseCursorPos);

    lua_register(g_pLuaState, "getLMBDown",            luaCBLeftMouseDown);
    lua_register(g_pLuaState, "getLMBWentDown",        luaCBLeftMouseWentDown);
    lua_register(g_pLuaState, "getRMBDown",            luaCBRightMouseDown);
    lua_register(g_pLuaState, "getRMBWentDown",        luaCBRightMouseWentDown);

    lua_register(g_pLuaState, "makeVoxelBlock",        luaCBMakeVoxelBlock);
    lua_register(g_pLuaState, "renderVoxelBlock",      luaCBRenderVoxelBlock);
    lua_register(g_pLuaState, "carveVoxelAt",          luaCBCarveVoxelAt);
    lua_register(g_pLuaState, "addVoxelAt",            luaCBAddVoxelAt);
    lua_register(g_pLuaState, "getVoxelColour",        luaCBGetVoxelColour);
    lua_register(g_pLuaState, "setVoxelColour",        luaCBSetVoxelColour);

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

    lua_register(g_pLuaState, "showFPS",               luaCBShowFPS);
    lua_register(g_pLuaState, "hideFPS",               luaCBHideFPS);

    lua_register(g_pLuaState, "pause",                 luaCBPause);
    lua_register(g_pLuaState, "continue",              luaCBContinue);

    lua_register(g_pLuaState, "playSound",              luaCBPlaySound);
    lua_register(g_pLuaState, "stopSound",              luaCBStopSound);
    lua_register(g_pLuaState, "soundPlaying",           luaCBSoundPlaying);

    lua_register(g_pLuaState, "getSampleRate",          luaCBGetSampleRate);
    lua_register(g_pLuaState, "getSamplesPerRequest",   luaCBGetSamplesPerRequest);

    lua_register(g_pLuaState, "readSample",             luaCBReadLiveBufferSample);
    lua_register(g_pLuaState, "writeSample",            luaCBWriteLiveBufferSample);

    lua_register(g_pLuaState, "getSampleMarkers",       luaCBGetLiveBufferMarkers);

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

    lua_register(g_pLuaState, "turnOnDebugHook",        luaCBTurnOnDebugHook);
    lua_register(g_pLuaState, "turnOffDebugHook",       luaCBTurnOffDebugHook);

    lua_register(g_pLuaState, "getFPS",                 luaCBGetFPS);

    lua_register(g_pLuaState, "lisp",                   luaCBLisp);
    lua_register(g_pLuaState, "forth",                  luaCBForth);
    lua_register(g_pLuaState, "iolang",                 luaCBIoLang);

    lua_register(g_pLuaState, "setLispTraceVerbosity",  luaCBSetLispTraceVerbosity);
    lua_register(g_pLuaState, "getLispTraceVerbosity",  luaCBGetLispTraceVerbosity);

    lua_register(g_pLuaState, "getCurrentDir",          luaCBGetCurrentDir);
    lua_register(g_pLuaState, "setCurrentDir",          luaCBSetCurrentDir);

    lua_register(g_pLuaState, "isAltDown",              luaCBIsAltDown);
    lua_register(g_pLuaState, "isShiftDown",            luaCBIsShiftDown);
    lua_register(g_pLuaState, "isCtrlDown",             luaCBIsCtrlDown);

    // Clear the stack with a lua_pop call here
    lua_pop(g_pLuaState, lua_gettop(g_pLuaState));
}
