#ifndef LUACB_H
#define LUACB_H

#pragma warning(disable:4311)
#pragma warning(disable:4244)
#pragma warning(disable:4273)

#include <stdio.h>
#include <string.h>

#include <string>

#include <sys/stat.h>

#ifdef __PRAXIS_WINDOWS__
#include <windows.h>
#include <mmsystem.h>
#include <winuser.h>
#include <winsock.h>
#include "MMSystem.h"
#include "fmod.h"
#include "fmod_errors.h"
#endif

#ifdef __PRAXIS_LINUX__
#include <X11/Xlib.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#endif

#include "luaCallbacks.h"
#include "luaInterface.h"

#include "World.h"

#include "SingleWorldConfiguration.h"

#include "VoxelBlock.h"
#include "Voxel.h"

#include "PraxisTexture.h"
#include "PraxisServer.h"
#include "PraxisLog.h"


#include "lispInterface.h"
#include "forthInterface.h"
#include "ioInterface.h"


extern "C"
{
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
}

extern lua_State *      g_pLuaState;

#ifdef __PRAXIS_WINDOWS__
extern FSOUND_STREAM *  g_pMp3Stream;
extern int              g_nMp3Channel;

extern "C" {
extern HWND g_AppHWND;
}
#endif

#ifdef __PRAXIS_LINUX__
extern "C" {
extern Display * g_pAppDisplay;
extern Window g_pAppWindow;
}
#endif

extern VoxelBlock *     g_pVoxelBlock;

extern World *          g_pWorld;

extern bool g_bLuaRunning;

extern int g_nMidiUpdateInterval;

#endif // LUACB_H
