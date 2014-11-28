
// Author:  Greg "fugue" Santucci, 2011
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/

#ifdef __PRAXIS_WINDOWS__
#include <windows.h>
#endif

#ifdef __PRAXIS_LINUX__
#include <unistd.h>
#include <pthread.h>
#endif

#include <time.h>


#include "luaInterface.h"

#include "World.h"

#include "SingleWorldConfiguration.h"

#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>

World::World()
{
    m_nRenderCount = 0;
    m_nUpdateCount = 0;

    //m_fFieldOfView = 45.0f;
    m_fFieldOfView = 90.0f;

    // m_trnCamera.SetTranslation(mlVector3D(162,66,35));
    // LookAt(mlVector3D(144,60,45));
    m_trnCamera.SetTranslation(mlVector3D(0,10,0));
    LookAt(mlVector3D(0,0,100));

    m_vMouse2DPosition = mlVector3D(300, 300);
    m_vMouse2DPositionDelta = mlVector3D();
    m_vMousePickPosition = mlVector3D();
    m_fMousePickDepth = 0.5f;
    m_vMouseFloorPickPosition = mlVector3D();
    m_fMouseFloorPickDepth = 0.5f;

    m_bLeftMouseDown = false;
    m_bLeftMouseWentDown = false;
    m_bRightMouseDown = false;
    m_bRightMouseWentDown = false;

    m_bMouseMovesCamera = true;
    m_bUsePositionPreservingOrbitCamera = true;
    //m_bUsePositionPreservingOrbitCamera = false;

    m_bUpdatePickPosition = true;
    m_bRenderMousePickSphere = true;
	m_bRenderFloorGrid = true;
    m_bRenderFloorPlane = true;
    m_bRenderProbesHUD = false;

    m_bDoubleClick = false;

    for(int i = 0; i < 4;  i++) viewport[i]    = 0;
    for(int i = 0; i < 16; i++) projMatrix[i]  = 0.0;
    for(int i = 0; i < 16; i++) modelMatrix[i] = 0.0;

    m_nCurrentBuffer = -1;

    NewEditor();

    m_bRenderOutput = true;
    m_bRenderError = true;
    m_bRenderEditor = true;

    m_bRunning = true;

    // MIDI
#ifdef __PRAXIS_LINUX__
    m_midiout = new RtMidiOutAlsa("praxis");
    m_midiin  = new RtMidiInAlsa("praxis");
#endif
#ifdef __PRAXIS_WINDOWS__
    m_midiout = new RtMidiOutWinMM("praxis");
    m_midiin  = 0;
#endif

}

World::~World()
{
}

mlTransform * World::GetCameraTransform()
{
	return &m_trnCamera;
}

mlTransform * World::GetCameraTransformBase()
{
    return &m_trnCameraBase;
}

void World::Init()
{
    luaCall("dofile(\"prod.lua\")");
}

#ifdef __PRAXIS_WINDOWS__
int worldGetWin32Modifiers( void )
{
    return
        ( ( ( GetKeyState( VK_LSHIFT   ) < 0 ) ||
            ( GetKeyState( VK_RSHIFT   ) < 0 )) ? GLUT_ACTIVE_SHIFT : 0 ) |
        ( ( ( GetKeyState( VK_LCONTROL ) < 0 ) ||
            ( GetKeyState( VK_RCONTROL ) < 0 )) ? GLUT_ACTIVE_CTRL  : 0 ) |
        ( ( ( GetKeyState( VK_LMENU    ) < 0 ) ||
            ( GetKeyState( VK_RMENU    ) < 0 )) ? GLUT_ACTIVE_ALT   : 0 );
}
#endif

void World::Update()
{
//    if(worldGetWin32Modifiers() & GLUT_ACTIVE_CTRL)
//        m_bMouseMovesCamera = false;
//    else
//        m_bMouseMovesCamera = true;

    if(m_bMouseMovesCamera)
    {
        if(m_bLeftMouseDown)
        {
            if(m_bDoubleClick)
            {
                if(m_bUsePositionPreservingOrbitCamera)
                    PositionPreservingOrbitCamera(m_vMousePickPosition, 0.0f, 0.0f);
                else
                    OrientationPreservingOrbitCamera(m_vMousePickPosition, 0.0f, 0.0f);
            }
        }
    }

    m_bLeftMouseWentDown = false;
    m_bRightMouseWentDown = false;

    if(m_bRunning)
    {
        if(!luaCall("update()"))
        {
            m_bRunning = false;
        }
    }

    GetEditor()->Update();

#ifdef __PRAXIS_WINDOWS__

    // Move this default behaviour as well as that of World::Render into lua?

    // Polling the date modified property of a file can cause sporadic stalls
    // Polling the clipboard should be cheap and quick
    // Better than implementing a TCP facility
    // Poll the clipboard, check if it is text and that it has a praxis: prefix.
    // If it does, then run the script following the prefix.
    // Empty the clipboard so it doesn't get run again.

    // I have a clear task ahead of me. So do it!
    // Step 1: Refer to Win32 book to see example of how to access the clipboard.
    // The clipboard is Chapter 12 in Petzold.

    luaCall("if string.sub(getClipboardText(), 1, 10) == \"-- praxis:\" then "
               "local sCmd = string.sub(getClipboardText(), 11) "
               "clearClipboardText() "
               "luaCall(sCmd) "
            "end");
#endif

#ifdef __PRAXIS_LINUX__
    extern pthread_mutex_t g_inputthreadmutex;
    extern std::string g_sInputThreadString;
    extern bool g_bInputThreadNewString;

    pthread_mutex_lock (&g_inputthreadmutex);
    if(g_bInputThreadNewString)
    {
        luaCall(g_sInputThreadString);
        g_bInputThreadNewString = false;
    }
    pthread_mutex_unlock (&g_inputthreadmutex);
#endif




    m_nUpdateCount++;
}

static void qt_save_gl_state()
{
    glPushClientAttrib(GL_CLIENT_ALL_ATTRIB_BITS);
    glPushAttrib(GL_ALL_ATTRIB_BITS);
    glMatrixMode(GL_TEXTURE);
    glPushMatrix();
    glLoadIdentity();
    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();

    glShadeModel(GL_FLAT);
    glDisable(GL_CULL_FACE);
    glDisable(GL_LIGHTING);
    glDisable(GL_STENCIL_TEST);
    glDisable(GL_DEPTH_TEST);
    glEnable(GL_BLEND);
    glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
}

static void qt_restore_gl_state()
{
    glMatrixMode(GL_TEXTURE);
    glPopMatrix();
    glMatrixMode(GL_PROJECTION);
    glPopMatrix();
    glMatrixMode(GL_MODELVIEW);
    glPopMatrix();
    glPopAttrib();
    glPopClientAttrib();
}

#ifndef GL_MULTISAMPLE
#define GL_MULTISAMPLE  0x809D
#endif


int g_nLastFrameTime = 0;
float g_fFPS = 0.0f;

void World::Render()
{
    UseMainWindowContext();

    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LESS);

    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    // glEnable(GL_CULL_FACE);
    glDisable(GL_CULL_FACE);

    // Turned this off for the Vivo Smart Tab
    // Turn it back on again if needed.
    //glEnable(GL_POLYGON_OFFSET_FILL);
    //glPolygonOffset (1., 1.);

    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glMatrixMode(GL_PROJECTION);
    glPushMatrix();

    BuildGLMatrices();

    RenderFloorPlane();
    if(m_bUpdatePickPosition && !m_bDoubleClick)
        RefreshFloorPickPosition();

    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    // turned off the lighting code for now
    // too much to tune
    // 3d ck didn't have lighting
    // just a simple scheme for coloring the different faces
    // of the object primitives so that they were distinct

    //glEnable(GL_LIGHTING);
    //glEnable(GL_LIGHT0);
    //glEnable(GL_LIGHT1);
    //glEnable(GL_MULTISAMPLE);
    glEnable(GL_COLOR_MATERIAL);

    if(m_bRenderFloorGrid)
        RenderFloorGrid();

    //static GLfloat lightPosition[4] = { 0.5, 5.0, 7.0, 1.0 };
    //static GLfloat lightPosition1[4] = {  30, 10,  30, 1.0 };
    //static GLfloat lightPosition2[4] = {   0, 20, 400, 1.0 };
    //glLightfv(GL_LIGHT0, GL_POSITION, lightPosition2);
    //glLightfv(GL_LIGHT1, GL_POSITION, lightPosition2);
    // x = 400, y = -300 + i * 60, z = 20
    //glLightfv(GL_LIGHT0, GL_POSITION, lightPosition);
    //glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);

    glColor4ub(255,255,255,255);

    if(m_bRunning)
    {
        if(!luaCall("render()"))
        {
            m_bRunning = false;
        }
    }

    // Restore a sane render state in case render left it in a bad condition
    glDisable(GL_STENCIL_TEST);
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LESS);
    glColorMask(1,1,1,1);

    // If there is an error, then stop the automatic lua calls.

    UseMainWindowContext();

    glColor4ub(0,255,0,255); // green

    // Always render the 3D version, since its not in the way.
    // Add an option for this later though.
    if(m_bRenderOutput)
        DrawText3DStroked(mlVector3D(-50,0,0  ), SelectEndLines(luaGetOutput(),100));

    if(m_bRenderError)
        DrawText3DStroked(mlVector3D(-50,0,200), SelectEndLines(luaGetError(),100));

    if(m_bRenderFloorPlane)
        RenderFloorPlane();

    if(m_bUpdatePickPosition && !m_bDoubleClick)
		RefreshPickPosition();

    if(m_bRenderMousePickSphere)
        RenderMousePickSphere();


    if(m_bRenderProbesHUD)
        RenderProbes();

    if(m_bRunning)
    {
        if(!luaCall("postrender()"))
        {
            m_bRunning = false;
        }
    }

    // Restore a sane render state in case postrender left it in a bad condition
    glDisable(GL_STENCIL_TEST);
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LESS);
    glColorMask(1,1,1,1);

    glDisable(GL_LIGHTING);
    glDisable(GL_LIGHT0);
    glDisable(GL_LIGHT1);
    glDisable(GL_MULTISAMPLE);

    glMatrixMode(GL_PROJECTION);
    glPopMatrix();

    if(m_bRenderEditor)
    {
        qt_save_gl_state();
        GetEditor()->Render();
        qt_restore_gl_state();

        DrawText2D(mlVector3D(5,2), std::string("Buffer: ") + GetEditor()->GetName());
    }

    bool bRenderFPS = true;

    if(bRenderFPS)
    {
#ifdef __PRAXIS_WINDOWS__
        int nCurrentTime = ::timeGetTime();
#endif
#ifdef __PRAXIS_LINUX__
        int nCurrentTime = glutGet(GLUT_ELAPSED_TIME);
#endif
        if(m_nRenderCount % 10 == 0)
        {
            int nTimeDelta = nCurrentTime - g_nLastFrameTime;
            g_nLastFrameTime = nCurrentTime;
            g_fFPS = 1000.0f / (float)nTimeDelta;
            g_fFPS *= 10.0f;
        }

        //qt_save_gl_state();
        //GetEditor()->Render();
        //qt_restore_gl_state();

        // Render a graph
        // A widget
        // A camera aligned widget
        // A widget for the 2D world
        // Just a camera aligned widget will do.

        stringstream ss; ss << std::setprecision(0) << std::fixed << "FPS: " << g_fFPS;
        DrawText2D(mlVector3D(90,95), ss.str());
    }


    // Also, FPS
    // Get current time
    // Compare with previous time
    // Calculate and display FPS
    // ????
    // PROFIT

    if(m_bRenderOutput)
        DrawText2D(mlVector3D(80,90),  SelectEndLines(luaGetOutput(),20));

    if(m_bRenderError)
        DrawText2D(mlVector3D(10,90),  SelectEndLines(luaGetError(),20));

    m_nRenderCount++;
}

void World::UpdateMousePos(int nX, int nY)
{
    // Viewport already set in Reshape()
    int nViewportHeight = viewport[3];

    mlVector3D vOldPosition = m_vMouse2DPosition;
    mlVector3D vNewPosition = mlVector3D(nX, nViewportHeight - nY);

    m_vMouse2DPosition = vNewPosition;
    m_vMouse2DPositionDelta = vNewPosition - vOldPosition;
}

void World::OnMouseMove(int nX, int nY)
{
    UpdateMousePos(nX, nY);

    // lets try this - nope
    // RefreshPickPosition();

    // Need to change this so it calls the callback during either render or update
    // so that the mouse pick position is up to date

    stringstream ss;
    ss << "OnMouseMove(" << m_vMouse2DPositionDelta.x << "," << m_vMouse2DPositionDelta.y << "," << nX << "," << nY << ")";
    luaCall(ss.str());

    if(m_bMouseMovesCamera)
    {
        if(m_bRightMouseDown)
        {
            PanCamera(m_vMouse2DPositionDelta.x, m_vMouse2DPositionDelta.y);
        }

        if(m_bLeftMouseDown)
        {
            // This is a magic number which maps pixels of mouse motion to radians of rotation
            float fRotationSpeed = 0.008f;

            float fHeadingChange = fRotationSpeed * m_vMouse2DPositionDelta.x;
            float fPitchChange   = fRotationSpeed * m_vMouse2DPositionDelta.y;

            if(m_bDoubleClick)
            {
                if(m_bUsePositionPreservingOrbitCamera)
                    PositionPreservingOrbitCamera(m_vMousePickPosition, fHeadingChange * 0.5, fPitchChange * 0.5);
                else
                    OrientationPreservingOrbitCamera(m_vMousePickPosition, fHeadingChange * 0.5, fPitchChange * 0.5);
            }
            else
            {
                RotateCamera(fHeadingChange, fPitchChange);
            }
        }
    }
}

void World::OnDoubleClick()
{
    m_bDoubleClick = true;
    m_bLeftMouseDown = true;
}

int g_nLastClickTime = 0;

void World::OnLButtonDown(int nX, int nY)
{
    // Need to implement double click detection a different way for Linux
    int nDblClickTime = 500;
#ifdef __PRAXIS_WINDOWS__
    nDblClickTime = ::GetDoubleClickTime();
#endif

    int nCurrClickTime = glutGet(GLUT_ELAPSED_TIME);
    int nTimeBwClicks = nCurrClickTime - g_nLastClickTime;
    g_nLastClickTime = nCurrClickTime;
    if(nTimeBwClicks < nDblClickTime)
    {
        OnDoubleClick();
        return;
    }

    stringstream ss;
    ss << "LMBDown(" << nX << "," << nY << ")";
    luaCall(ss.str());

    m_bLeftMouseDown = true;
	m_bLeftMouseWentDown = true;

	if(m_bMouseMovesCamera)
        m_bUpdatePickPosition = false;
}

void World::OnLButtonUp(int nX, int nY)
{
    stringstream ss;
    ss << "LMBUp(" << nX << "," << nY << ")";
    luaCall(ss.str());

    m_bLeftMouseDown = false;
    m_bDoubleClick = false;

    m_bUpdatePickPosition = true;
}

void World::OnRButtonDown(int nX, int nY)
{
    stringstream ss;
    ss << "RMBDown(" << nX << "," << nY << ")";
    luaCall(ss.str());

    m_bRightMouseDown = true;
    m_bRightMouseWentDown = true;

    if(m_bMouseMovesCamera)
        m_bUpdatePickPosition = false;
}

void World::OnRButtonUp(int nX, int nY)
{
    stringstream ss;
    ss << "RMBUp(" << nX << "," << nY << ")";
    luaCall(ss.str());

    m_bRightMouseDown = false;

    m_bUpdatePickPosition = true;
}

void World::OnKeyDown(unsigned char nKey, int nX, int nY)
{
    // When the buffer is visible, sent the keypresses to it.
    // When the buffer isn't, call the lua function.

    if(!m_bRenderEditor)
    {
        string temp(" "); temp[0] = nKey;
        stringstream ss;
        ss << "keyDown(\"" << temp.c_str() << "\")";
        luaCall(ss.str());

        return;
    }


    // This is where to handle pressing enter for a command buffer.
    // Do the command, then close
    // Changed my mind. I only want it so I can press enter to submit the command.
    // Use a prompt? Check for a prompt to decide whether to respond to enter pressed?
    // The print command. An interactive repl. Printing to the current buffer nicely.

    // Maybe a "parent buffer" is sufficient. Lets just use that for now.
    // Named buffers? Named buffers sounds good.
    // Can refer to them by name.
    // newBuffer("testout")
    // Simplifies saving. Just use the buffer name.
    // For safety, if file exists, save numbered backup before overwriting.

    // std::cout << "OnKeyDown: " << (int)nKey << ", " << "Mods: " << glutGetModifiers() << std::endl;

    bool bCtrlEnter = false;
    bool bShiftEnter = false;
#ifdef __PRAXIS_LINUX__
    bCtrlEnter  = (glutGetModifiers() & GLUT_ACTIVE_CTRL  && nKey == '\r');
    bShiftEnter = (glutGetModifiers() & GLUT_ACTIVE_SHIFT && nKey == '\r');
#endif
#ifdef __PRAXIS_WINDOWS__
    bCtrlEnter  = (nKey == '\n'); // Instead of '\r' presumably
    bShiftEnter = (glutGetModifiers() & GLUT_ACTIVE_SHIFT && nKey == '\r');
#endif

//    if(glutGetModifiers() & GLUT_ACTIVE_SHIFT)
//        std::cout << "GLUT_ACTIVE_SHIFT : ON"  << std::endl;
//    else
//        std::cout << "GLUT_ACTIVE_SHIFT : OFF" << std::endl;

//    if(glutGetModifiers() & GLUT_ACTIVE_CTRL)
//        std::cout << "GLUT_ACTIVE_CTRL  : ON"  << std::endl;
//    else
//        std::cout << "GLUT_ACTIVE_CTRL  : OFF" << std::endl;


//    std::cout << "bCtrlEnter  = " << (bCtrlEnter  ? "true" : "false") << std::endl;
//    std::cout << "bShiftEnter = " << (bShiftEnter ? "true" : "false") << std::endl;

    if(bCtrlEnter)
    {
        std::string sLine = GetEditor()->GetCurrentLineText();
        std::string sLua  = GetEditor()->GetLuaBlock();

        if(sLua != "")
            luaCall(sLua);
        else
            luaCall(sLine);
    }
    else if(bShiftEnter)
    {
        std::string sLine = GetEditor()->GetCurrentLineText();
        std::string sLua  = GetEditor()->GetLuaBlock();

        if(sLua != "")
            GetEditor()->m_Position = GetEditor()->m_LuaBlockHighlight[1];
        else
            GetEditor()->m_Position = GetEditor()->LineEnd(GetEditor()->m_Position);

        // Insert a line feed.
        // Should go to the end of the block or line before inserting line feed.
        GetEditor()->InsertNewline();
        GetEditor()->Update();

        if(sLua != "")
            luaCall(sLua);
        else
            luaCall(sLine);
    }
    else
    {
        // If its just the Control key or other modifier key, don't call this.
        // Its preventing Ctrl-C and Ctrl-V from working in Linux.
        if(nKey != 0)
            GetEditor()->Handle(nKey, 0);

        //std::cout << "Key = " << (int)nKey << std::endl;
    }
}

void World::OnKeyUp(unsigned char nKey, int nX, int nY)
{
    if(!m_bRenderEditor)
    {
        string temp(" "); temp[0] = nKey;
        stringstream ss;
        ss << "keyUp(\"" << temp.c_str() << "\")";
        luaCall(ss.str());
    }

    //std::cout <<"OnKeyUp" << endl;
}

void World::OnKeyDownSpecial(unsigned char nKey, int nX, int nY)
{
    // Have this call the lisp and forth functions as well?
    // Or just Lua for now?
    // Or just Lua, which calls other language stuff?

    //std::cout << "OnKeyDownSpecial: " << (int)nKey << ", " << "Mods: " << glutGetModifiers() << std::endl;

    if(nKey == GLUT_KEY_F1)  luaCall("f1Pressed()");
    if(nKey == GLUT_KEY_F2)  luaCall("f2Pressed()");
    if(nKey == GLUT_KEY_F3)  luaCall("f3Pressed()");
    if(nKey == GLUT_KEY_F4)  luaCall("f4Pressed()");
    if(nKey == GLUT_KEY_F5)  luaCall("f5Pressed()");
    if(nKey == GLUT_KEY_F6)  luaCall("f6Pressed()");
    if(nKey == GLUT_KEY_F7)  luaCall("f7Pressed()");
    if(nKey == GLUT_KEY_F8)  luaCall("f8Pressed()");
    if(nKey == GLUT_KEY_F9)  luaCall("f9Pressed()");
    if(nKey == GLUT_KEY_F10) luaCall("f10Pressed()");
    if(nKey == GLUT_KEY_F11) luaCall("f11Pressed()");
    if(nKey == GLUT_KEY_F12) luaCall("f12Pressed()");

    if(nKey == GLUT_KEY_TAB)
    {
        // std::cout << "tab from special" << endl;
        if(m_buffers.size() > 0)
        {
            if(glutGetModifiers() & GLUT_ACTIVE_CTRL)
            {
                if(glutGetModifiers() & GLUT_ACTIVE_SHIFT)
                    PreviousEditor();
                else
                    NextEditor();

                // early out here so that tab isn't registered as an editor key
                return;
            }
        }
    }

    // REPL buffer for one line commands
    // one line commands can be:
    // edit("function name")
    // which will open a new buffer just for editing that function
    // still need to render the trace and error strings
    // the last n lines of them.

    // don't handle if tab used to switch buffers.
    GetEditor()->Handle(0, nKey);

    // Function keys and other shortcuts for:
    // switching buffers
    // loading and saving
    // executing lua code
    // go to definition

    // Ctrl-Tab and Ctrl-Shift-Tab for buffer selection

}

void World::NewEditor()
{
    GLEditor * pNewBuffer = new GLEditor();
    if(m_buffers.size() <= 0)
        pNewBuffer->SetName("initial");
    else
        pNewBuffer->SetParentName(GetEditor()->GetName());

    //pNewBuffer->SetText("-- scratch pad!!");
    m_buffers.push_back(pNewBuffer);
    m_nCurrentBuffer = m_buffers.size() - 1;

    glGetIntegerv(GL_VIEWPORT, viewport);
    int nViewportWidth = viewport[2];
    int nViewportHeight = viewport[3];
    pNewBuffer->Reshape(nViewportWidth, nViewportHeight);
}

void World::CloseEditor()
{
    if(m_buffers.size() > 1 && m_nCurrentBuffer >= 0 && m_nCurrentBuffer < m_buffers.size())
    {
        m_buffers.erase(m_buffers.begin() + m_nCurrentBuffer);
        m_nCurrentBuffer--;
        if(m_nCurrentBuffer < 0)
            m_nCurrentBuffer = 0;
    }
}

void World::SwitchToEditor(int n)
{
    if(n < 0 || n >= m_buffers.size())
        return;

    m_nCurrentBuffer = n;
}

void World::SwitchToEditor(const std::string & sName)
{
    for(int i = 0; i < m_buffers.size(); i++)
    {
        GLEditor * pBuffer = m_buffers[i];
        if(pBuffer->GetName() == sName)
        {
            m_nCurrentBuffer = i;
            return;
        }
    }
}

void World::NextEditor()
{
    if(m_buffers.size() <= 0)
        return;

    if(m_nCurrentBuffer == -1)
        m_nCurrentBuffer = 0;
    else
        m_nCurrentBuffer = (m_nCurrentBuffer + 1) % m_buffers.size();
}

void World::PreviousEditor()
{
    if(m_buffers.size() <= 0)
        return;

    if(m_nCurrentBuffer == -1)
        m_nCurrentBuffer = 0;
    else
        m_nCurrentBuffer = (m_nCurrentBuffer + m_buffers.size() - 1) % m_buffers.size();
}

void World::OnKeyUpSpecial(unsigned char nKey, int nX, int nY)
{
}

void World::Reshape(int nWidth, int nHeight)
{
    glViewport(0,0,nWidth,nHeight);

    for(int i = 0; i < m_buffers.size(); i++)
        m_buffers[i]->Reshape(nWidth, nHeight);

//    GetEditor()->Reshape(nWidth, nHeight);
}

void World::OnWheelDown(int nX, int nY)
{
    float fStepSize = 5.0f;

    mlVector3D vCamera = GetCameraTransform()->GetTranslation();
    float fDistance = (m_vMousePickPosition - vCamera).Magnitude();
    fStepSize = fDistance * 0.05f;

    fStepSize = mlClamp(fStepSize, -20.0f, 20.0f);

    mlVector3D vBackward = vCamera - m_vMousePickPosition;
    vBackward.Normalise();
    vBackward *= fStepSize;

    m_trnCamera.ApplyTranslation(vBackward);
}

void World::OnWheelUp(int nX, int nY)
{
    float fStepSize = 5.0f;

    mlVector3D vCamera = GetCameraTransform()->GetTranslation();
    float fDistance = (m_vMousePickPosition - vCamera).Magnitude();
    fStepSize = fDistance * 0.05f;

    fStepSize = mlClamp(fStepSize, -20.0f, 20.0f);

    mlVector3D vForward = vCamera - m_vMousePickPosition;
    vForward.Normalise();
    vForward *= -fStepSize;

    m_trnCamera.ApplyTranslation(vForward);
}

void World::BuildGLMatrices()
{
    glGetIntegerv(GL_VIEWPORT, viewport);

    // Projection Matrix
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();

    int nViewportWidth = viewport[2];
    int nViewportHeight = viewport[3];

    GLdouble fAspect = GLdouble(nViewportWidth) / GLdouble(nViewportHeight);

    //gluPerspective(m_fFieldOfView, fAspect, 0.1f, 20000.0f);
    //gluPerspective(m_fFieldOfView, fAspect, 0.01f, 1000.0f);
    //gluPerspective(m_fFieldOfView, fAspect, 0.1f, 1000.0f);
    gluPerspective(m_fFieldOfView, fAspect, 1.0f, 1000.0f);
    // gluPerspective makes a "-Z is in" matrix. I prefer +Z.
    glScalef(1,1,-1);

    // Modelview Matrix
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    mlTransform transform = m_trnCamera;

    transform.Invert();

    mlMatrix4x4 matCameraTransform(transform.GetMatrix());

    mlFloat * pCameraTransform = reinterpret_cast<mlFloat*>(&matCameraTransform);

    glLoadMatrixf(pCameraTransform);

    glGetDoublev(GL_PROJECTION_MATRIX, projMatrix);
    glGetDoublev(GL_MODELVIEW_MATRIX, modelMatrix);
}

void World::RenderFloorPlane()
{
    float fSize = 500.0f;

    // The textured plane underneath the grid - omit for now.
#if 0
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );

    glEnable(GL_TEXTURE_2D);

    // glBindTexture(GL_TEXTURE_2D, m_nBgTexIndex);

    // http://www.gamedev.net/page/resources/_/technical/opengl/moving-beyond-opengl-11-for-windows-r1929
    // http://stackoverflow.com/questions/7808146/gltexsubimage2d-extremely-slow-on-intel-video-card
    // http://www.clearchain.com/blog/posts/opengl-gltexsubimage2d-very-slow-a-solution
    // https://developer.palm.com/distribution/viewtopic.php?f=70&t=11066
    // http://www.khronos.org/message_boards/viewtopic.php?f=4&t=1274
    
    // check textureplay for what to put here.

    //glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0,
    //                m_texBg.width(),
    //                m_texBg.height(),
    //                GL_BGRA_EXT, GL_UNSIGNED_BYTE,
    //                //GL_RGBA, GL_UNSIGNED_BYTE,
    //                m_texBg.bits());

    //glColor4f(0.3f, 0.3f, 0.3f, 0.0f);
    //glColor3f(0.15f, 0.15f, 0.15f);
    glColor3f(1.0f, 1.0f, 1.0f);
    //GLfloat faceColor[4] = { 1.0f, 1.0f, 1.0f, 1.0f };
    //glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, faceColor);
    //glColor3f(0.5f, 0.5f, 0.5f);
    glBegin(GL_QUADS);

    //glTexCoord2f(1.0f, 0.0f);
    glTexCoord2f(0.0f, 0.0f);
    glNormal3f(0.0f, 1.0f, 0.0f);
    glVertex3f(-fSize, -10.0f, -fSize);
    //glTexCoord2f(0.0f, 0.0f);
    glTexCoord2f(1.0f, 0.0f);
    glNormal3f(0.0f, 1.0f, 0.0f);
    glVertex3f( fSize, -10.0f, -fSize);
    //glTexCoord2f(0.0f, 1.0f);
    glTexCoord2f(1.0f, 1.0f);
    glNormal3f(0.0f, 1.0f, 0.0f);
    glVertex3f( fSize, -10.0f,  fSize);
    //glTexCoord2f(1.0f, 1.0f);
    glTexCoord2f(0.0f, 1.0f);
    glNormal3f(0.0f, 1.0f, 0.0f);
    glVertex3f(-fSize, -10.0f,  fSize);
    glEnd();

    glDisable(GL_TEXTURE_2D);
#endif

    glColor4f(0.3f, 0.3f, 0.3f, 0.0f);
    //GLfloat faceColor2[4] = { 0.3f, 0.3f, 0.3f, 0.0f };
    //glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, faceColor2);

    //glColor3f(0.15f, 0.15f, 0.15f);
    glBegin(GL_QUADS);

    glNormal3f(0.0f, 1.0f, 0.0f);
    glVertex3f(-fSize, 0.0f, -fSize);
    glNormal3f(0.0f, 1.0f, 0.0f);
    glVertex3f( fSize, 0.0f, -fSize);
    glNormal3f(0.0f, 1.0f, 0.0f);
    glVertex3f( fSize, 0.0f,  fSize);
    glNormal3f(0.0f, 1.0f, 0.0f);
    glVertex3f(-fSize, 0.0f,  fSize);

    glEnd();
}

void World::RenderFloorGrid()
{
    float fSize = 500.0f;
    float fWidth = 10.0f;
    int nNumLines = (fSize * 2.0f) / fWidth;

    //glColor3f(0.6f, 0.6f, 0.1f);
    glColor3f(0.6f, 0.1f, 0.6f);
    //glColor3f(0.1f, 0.3f, 0.7f);

    glBegin(GL_LINES);

    for(int i = 0; i <= nNumLines; i++)
    {
        glVertex3f(-fSize + i * fWidth, 0, -fSize);
        glVertex3f(-fSize + i * fWidth, 0,  fSize);
    }

    for(int i = 0; i <= nNumLines; i++)
    {
        glVertex3f(-fSize, 0, -fSize + i * fWidth);
        glVertex3f( fSize, 0, -fSize + i * fWidth);
    }

    glEnd();

    // Need something more like a HUD showing the coordinates currently being looked at.

//    glColor3f(0.0f, 1.0f, 0.0f);

//    for(int i = 0; i <= nNumLines; i = i + 2)
//    {
//        mlVector3D pos(-fSize + i * fWidth, 0, -fSize - 25);
//        stringstream ss;
//        ss << pos.x;
//        DrawText3DStroked(pos, ss.str());
//    }

//    for(int i = 0; i <= nNumLines; i = i + 2)
//    {
//        mlVector3D pos(-fSize - fWidth, 0, -fSize + i * fWidth);
//        stringstream ss;
//        ss << pos.z;
//        DrawText3DStroked(pos, ss.str());
//    }
}

void World::RenderMousePickSphere()
{
    // Convert screen radius to 3D radius

    mlVector3D vPoint = m_vMousePickPosition;
    mlVector3D vPointBase = m_trnCameraBase.TransformPointInverse(vPoint);
    bool bEq = (vPoint - vPointBase).Magnitude() < 1.0f;

    int nRadius = 15;

    {
    GLdouble winX, winY, winZ;
    GLdouble objX, objY, objZ;

    objX = vPoint.x;
    objY = vPoint.y;
    objZ = vPoint.z;

    gluProject(objX, objY, objZ, modelMatrix, projMatrix, viewport, &winX, &winY, &winZ);

    winX += nRadius;

    gluUnProject(winX, winY, winZ, modelMatrix, projMatrix, viewport, &objX, &objY, &objZ);

    mlVector3D vSurfacePosition(objX, objY, objZ);

    float fRadius = (vSurfacePosition - vPoint).Magnitude();

    // Now render

    glLineWidth(1.0f);

    glDisable(GL_TEXTURE_2D);

    glMatrixMode(GL_MODELVIEW);

    glPushMatrix();

    glTranslatef(vPoint.x, vPoint.y, vPoint.z);

    //glPushMatrix();

    mlTransform transform = m_trnCameraBase;
    transform.SetTranslation(mlVector3DZero);

    mlMatrix4x4 mat(transform.GetMatrix());

    mlFloat * pMat = reinterpret_cast<mlFloat*>(&mat);

    glMultMatrixf(pMat);

    glColor4ub(
        0,
        255,
        0,
        255);

    //GLfloat faceColor[4] = { 0.0f, 1.0f, 0.0f, 1.0f };
    //glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, faceColor);

    glutWireSphere(fRadius, 15, 15);

    float fTextScale = 0.1f;
    float fAxisScale = 3.0f;

    glDisable(GL_DEPTH_TEST);

    glBegin(GL_LINES);

    glColor4ub(255,0,0, 255);
    glVertex3f(0,0,0);
    glVertex3f(fRadius * fAxisScale, 0,0);

    glColor4ub(0,0,255, 255);
    glVertex3f(0,0,0);
    glVertex3f(0, fRadius * fAxisScale, 0);

    glColor4ub(0,255,0, 255);
    glVertex3f(0,0,0);
    glVertex3f(0,0, fRadius * fAxisScale);

    glEnd();

    // Next step: change this to use the base coordinate system instead of this hardcoded one.

    {
        stringstream ss;
        if(bEq)
            ss << std::setprecision(2) << std::fixed << "x=" << vPoint.x;
        else
            ss << std::setprecision(2) << std::fixed << "x=" << vPointBase.x << std::endl << "  " << vPoint.x;
        glColor4ub(255,0,0, 255);
        glPushMatrix();
        glTranslatef(fRadius * fAxisScale + fRadius * fTextScale * 0.1f, 0, 0);
        glScalef(fRadius * fTextScale, fRadius * fTextScale, fRadius * fTextScale);
        DrawText3DStroked(mlVector3DZero, ss.str());
        glPopMatrix();
    }

    {
        stringstream ss;
        if(bEq)
            ss << std::setprecision(2) << std::fixed << "y=" << vPoint.y;
        else
            ss << std::setprecision(2) << std::fixed << "y=" << vPointBase.y << std::endl << "  " << vPoint.y;
        glColor4ub(0,0,255, 255);
        glPushMatrix();
        glTranslatef(fRadius * fTextScale * 0.1f, fRadius * fAxisScale, 0);
        glScalef(fRadius * fTextScale, fRadius * fTextScale, fRadius * fTextScale);
        DrawText3DStroked(mlVector3DZero, ss.str());
        glPopMatrix();
    }

    {
        stringstream ss;
        if(bEq)
            ss << std::setprecision(2) << std::fixed << "z=" << vPoint.z;
        else
            ss << std::setprecision(2) << std::fixed << "z=" << vPointBase.z << std::endl << "  " << vPoint.z;
        glColor4ub(0,255,0, 255);
        glPushMatrix();
        glTranslatef(fRadius * fTextScale * 2.0f, 0, fRadius * fAxisScale);
        glScalef(fRadius * fTextScale, fRadius * fTextScale, fRadius * fTextScale);
        DrawText3DStroked(mlVector3DZero, ss.str());
        glPopMatrix();
    }

    glPopMatrix();

    {
        glPushMatrix();

        // Use the camera transform moved to the pick position.

        mlTransform transform = m_trnCamera;
        transform.SetTranslation(vPoint);

        mlMatrix4x4 mat(transform.GetMatrix());

        mlFloat * pMat = reinterpret_cast<mlFloat*>(&mat);

        glMultMatrixf(pMat);

        // Rotations required because DrawText3DStroked draws text along the Z axis
        glRotatef(-90.0f, 1.0, 0.0f, 0.0f);

        // Move text beneath the pick sphere
        glTranslatef(0,0,fRadius * -2.0f);

        // Use fRadius to scale it so it remains a constant size
        glScalef(fRadius * fTextScale, fRadius * fTextScale, fRadius * fTextScale);

        glColor4ub(255,255,0, 255);

        stringstream ss;
        if(bEq)
            ss << std::setprecision(2) << std::fixed << "(" << vPoint.x << "," << vPoint.y << "," << vPoint.z << ")";
        else
            ss << std::setprecision(2) << std::fixed << "(" << vPointBase.x << "," << vPointBase.y << "," << vPointBase.z << ")" << std::endl
                                                     << "(" << vPoint.x     << "," << vPoint.y     << "," << vPoint.z     << ")";

        DrawText3DStroked(mlVector3DZero, ss.str());

        glPopMatrix();
    }

    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LESS);

    glColor4ub(0,255,0, 255);

    //glPopMatrix();

    }

    vPoint = m_vMouseFloorPickPosition;
    nRadius = 15;

    {
    GLdouble winX, winY, winZ;
    GLdouble objX, objY, objZ;

    objX = vPoint.x;
    objY = vPoint.y;
    objZ = vPoint.z;

    gluProject(objX, objY, objZ, modelMatrix, projMatrix, viewport, &winX, &winY, &winZ);

    winX += nRadius;

    gluUnProject(winX, winY, winZ, modelMatrix, projMatrix, viewport, &objX, &objY, &objZ);

    mlVector3D vSurfacePosition(objX, objY, objZ);

    float fRadius = (vSurfacePosition - vPoint).Magnitude();

    // Now render

    glLineWidth(1.0f);

    glDisable(GL_TEXTURE_2D);

    glMatrixMode(GL_MODELVIEW);

    glPushMatrix();

    glTranslatef(vPoint.x, vPoint.y, vPoint.z);

    glColor4ub(
        0,
        255,
        0,
        255);

    //GLfloat faceColor[4] = { 0.0f, 1.0f, 0.0f, 1.0f };
    //glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, faceColor);

    glutWireSphere(fRadius, 15, 15);

    glPopMatrix();
    }
}

void World::RefreshPickPosition()
{
    GLdouble winX, winY, winZ;
    GLdouble objX, objY, objZ;

    // Using m_vMouse2DPosition, get m_fMousePickDepth
    m_fMousePickDepth = 0.0f;
    glReadPixels(m_vMouse2DPosition.x, m_vMouse2DPosition.y, 1,1, GL_DEPTH_COMPONENT, GL_FLOAT, &m_fMousePickDepth);
    if(m_fMousePickDepth == 1.0f)
        m_fMousePickDepth = 0.1f;

    // Using m_vMouse2DPosition and m_fMousePickDepth, get m_vMousePickPosition
    winX = m_vMouse2DPosition.x;
    winY = m_vMouse2DPosition.y;
    winZ = m_fMousePickDepth;
    gluUnProject(winX, winY, winZ, modelMatrix, projMatrix, viewport, &objX, &objY, &objZ);
    m_vMousePickPosition = mlVector3D(objX, objY, objZ);
}

void World::RefreshFloorPickPosition()
{
    GLdouble winX, winY, winZ;
    GLdouble objX, objY, objZ;

    // Using m_vMouse2DPosition, get m_fMousePickDepth
    m_fMouseFloorPickDepth = 0.0f;
    glReadPixels(m_vMouse2DPosition.x, m_vMouse2DPosition.y, 1,1, GL_DEPTH_COMPONENT, GL_FLOAT, &m_fMouseFloorPickDepth);
    if(m_fMouseFloorPickDepth == 1.0f)
        m_fMouseFloorPickDepth = 0.1f;

    // Using m_vMouse2DPosition and m_fMousePickDepth, get m_vMousePickPosition
    winX = m_vMouse2DPosition.x;
    winY = m_vMouse2DPosition.y;
    winZ = m_fMouseFloorPickDepth;
    gluUnProject(winX, winY, winZ, modelMatrix, projMatrix, viewport, &objX, &objY, &objZ);
    m_vMouseFloorPickPosition = mlVector3D(objX, objY, objZ);
}

// Want to be able to add probes from lua.

bool World::Pick(mlVector3D & pos2d, mlVector3D & pos3d)
{
    glReadPixels(pos2d.x, pos2d.y, 1,1, GL_DEPTH_COMPONENT, GL_FLOAT, &pos2d.z);

    if(pos2d.z != 1.0f)
    {
        GLdouble objX, objY, objZ;
        gluUnProject(pos2d.x, pos2d.y, pos2d.z, modelMatrix, projMatrix, viewport, &objX, &objY, &objZ);
        pos3d = mlVector3D(objX, objY, objZ);
        return true;
    }

    return false;
}

void World::RenderProbes()
{
    int nViewportWidth = viewport[2];
    int nViewportHeight = viewport[3];

    float fOffset = 100.0f;

    for(float x = fOffset; x < nViewportWidth; x += 100.0f)
    {
        mlVector3D pos2d(x, fOffset, 0.0f);
        mlVector3D pos3d;
        bool bValid = Pick(pos2d, pos3d);
        if(bValid)
        {
            stringstream ss; ss << std::setprecision(2) << std::fixed << pos3d.x << "\n" << pos3d.z;
            DrawText2DUnmapped(pos2d, ss.str());
        }
        else
        {
            DrawText2DUnmapped(pos2d, "---");
        }
    }

    for(float y = fOffset; y < nViewportHeight; y += 100.0f)
    {
        mlVector3D pos2d(fOffset, y, 0.0f);
        mlVector3D pos3d;
        bool bValid = Pick(pos2d, pos3d);
        if(bValid)
        {
            stringstream ss; ss << std::setprecision(2) << std::fixed << pos3d.x << "\n" << pos3d.z;
            DrawText2DUnmapped(pos2d, ss.str());
        }
        else
        {
            DrawText2DUnmapped(pos2d, "---");
        }
    }
}

void World::PanCamera(float fX, float fY)
{
    GLdouble winX, winY, winZ;
    GLdouble objX, objY, objZ;

    mlVector3D vMouseNearA = m_vMouse2DPosition;
    mlVector3D vMouseNearB = m_vMouse2DPosition + mlVector3D(fX, fY);

    winX = vMouseNearA.x;
    winY = vMouseNearA.y;
    winZ = m_fMousePickDepth;
    gluUnProject( winX, winY, winZ, modelMatrix, projMatrix, viewport, &objX, &objY, &objZ);
    mlVector3D vMouseFarA = mlVector3D(objX, objY, objZ);

    winX = vMouseNearB.x;
    winY = vMouseNearB.y;
    winZ = m_fMousePickDepth;
    gluUnProject( winX, winY, winZ, modelMatrix, projMatrix, viewport, &objX, &objY, &objZ);
    mlVector3D vMouseFarB = mlVector3D(objX, objY, objZ);

    mlVector3D vMovement = vMouseFarA - vMouseFarB;
    m_trnCamera.ApplyTranslation(vMovement);
}

void World::RotateCamera(float fHeading, float fPitch)
{
    m_trnCamera.ApplyRotation(mlQuaternion(GetBaseUp(), fHeading));

    //mlVector3D vSide = m_trnCamera.TransformVector(GetBaseSide());
    mlVector3D vSide = m_trnCamera.TransformVector(mlVector3D(1,0,0));

    m_trnCamera.ApplyRotation(mlQuaternion(vSide, -fPitch));
}

void World::PositionPreservingOrbitCamera(mlVector3D & vCenter, float fHeading, float fPitch)
{
    // Constants
    mlVector3D vGlobalUp     = GetBaseUp();
    mlVector3D vGlobalDown   = vGlobalUp * -1.0f;

    mlVector3D vLocalSide    = mlVector3D(1,0,0);
    mlVector3D vLocalUp      = mlVector3D(0,1,0);
    mlVector3D vLocalForward = mlVector3D(0,0,1);

    // The current camera orientation
    mlQuaternion rotCurrent = m_trnCamera.GetRotation();

    // Make a quaternion to represent vector to the focal point (Y is up)
    mlQuaternion rotToPoint;
    mlVector3D vToPoint;
    float fRadius = 0.0f;
    {
        vToPoint = vCenter - m_trnCamera.GetTranslation();

        fRadius = vToPoint.Magnitude();

        vToPoint.Normalise();

        rotToPoint = mlQuaternionFromDirection(vToPoint, vGlobalUp);
        rotToPoint.Normalise();
    }

    // The difference between the current orientation
    // and the look-at orientation
    // This will be referred to after moving, because this difference
    // is what needs to be reduced.
    mlQuaternion rotDiff;
    {
        mlQuaternion rotToPointInv = rotToPoint;
        rotToPointInv.Invert();

        rotDiff = rotToPointInv * rotCurrent;
    }

    // Rotate the quaternion according to the specified heading and pitch change.
    // (Y is up)
    // This is for the purposes of pushing back to produce the new position
    // It is the current orientation rotated
    mlQuaternion rotToPointChanged = rotToPoint;
    mlVector3D vToPointAfterMove;
    {
        rotToPointChanged = rotToPointChanged * mlQuaternion(vLocalUp,    fHeading);
        rotToPointChanged = rotToPointChanged * mlQuaternion(vLocalSide, -fPitch);

        rotToPointChanged.Normalise();

        vToPointAfterMove = rotToPointChanged.TransformVector(vLocalForward);
    }

    // If the proposed angle is too acute, do nothing
    //if(false)
    {
        float fNewAngleToDown = vGlobalDown.AngleToVector(vToPointAfterMove);
        float fNewAngleToUp = vGlobalUp.AngleToVector(vToPointAfterMove);
        float fCurrentAngleToDown = vGlobalDown.AngleToVector(vToPoint);
        float fCurrentAngleToUp = vGlobalUp.AngleToVector(vToPoint);

        float fPitchAngleLimit = 10.0f;

        if(  mlFabs(mlRadiansToDegrees(fNewAngleToDown)) < fPitchAngleLimit &&
             mlFabs(fNewAngleToDown) < mlFabs(fCurrentAngleToDown))
        {
            return;
        }

        if(  mlFabs(mlRadiansToDegrees(fNewAngleToUp)) < fPitchAngleLimit &&
             mlFabs(fNewAngleToUp) < mlFabs(fCurrentAngleToUp))
        {
            return;
        }
    }

    // Move the camera to the center, maintaining the current rotation
    m_trnCamera.SetTranslation(vCenter);

    // Now the rotation quaternion is ready, and may be used to make the push back vector
    mlVector3D vPushBack = rotToPointChanged.TransformVector(vLocalForward * fRadius);

    // Push back the camera. Note that the rotation still hasn't altered.
    // The "topoint" rotation was used to make the push back vector (just a reminder)
    m_trnCamera.SetTranslation(vCenter - vPushBack);

    // Now we are free to modify the rotation of the camera
    // We can either make it point to the point,
    // or lerp it to that rotation.
    {
        mlVector3D vNewToPoint = vCenter - m_trnCamera.GetTranslation();
        vNewToPoint.Normalise();

        mlQuaternion rotNewToPoint = mlQuaternionFromDirection(vNewToPoint, vGlobalUp);

        // Apply this difference to the new look at orientation.
        // (The look at orientation for the new position)
        // This is the starting point for the lerping.
        // This is the same "orientation distance" away from
        // looking at the center as we were at the start.
        // Lerp from here, so that this distance gets smaller.
        mlQuaternion rotNewCurrent = rotNewToPoint * rotDiff;

        mlQuaternion rotResult = mlQuaternionInterpolate(0.2f, rotNewCurrent, rotNewToPoint);

        m_trnCamera.SetRotation(rotResult);
    }

    m_trnCamera.Normalise();
}

void World::OrientationPreservingOrbitCamera(mlVector3D & vCenter, float fHeading, float fPitch)
{
    mlVector3D vToPoint = m_trnCamera.TransformPointInverse(vCenter);

    m_trnCamera.SetTranslation(vCenter);

    RotateCamera(fHeading, fPitch);

    vToPoint.x *= 0.9f;
    vToPoint.y *= 0.9f;

    mlVector3D vPushBack = m_trnCamera.TransformVector(vToPoint);

    m_trnCamera.SetTranslation(vCenter - vPushBack);

    m_trnCamera.Normalise();
}

void World::LookAt(mlVector3D vFocalPoint)
{
	mlVector3D vForward = vFocalPoint - m_trnCamera.GetTranslation();

	vForward.Normalise();

	mlQuaternion rotLookAt = mlQuaternionFromDirection(vForward, mlVector3D(0.0f, 1.0f, 0.0f));

	m_trnCamera.SetRotation(rotLookAt);
}

void World::LookDown()
{
	mlMatrix3x3 mLookDown;
	mLookDown.I = mlVector3D(0.0f, 1.0f, 0.0f);
	mLookDown.J = mlVector3D(0.0f, 0.0f, -1.0f);
	mLookDown.K = mlVector3D(1.0f, 0.0f, 0.0f);

	mlQuaternion qRot = mlQuaternionFromRotationMatrix(mLookDown);

	m_trnCamera.SetRotation(qRot);
}

// Utility functions

void World::DrawText3D(mlVector3D vPos, const std::string & sText)
{
    glRasterPos3d(vPos.x, vPos.y, vPos.z);
    glutBitmapString(GLUT_BITMAP_HELVETICA_18, (unsigned char *)sText.c_str());
}

void World::DrawText3DStroked(mlVector3D vPos, const std::string & sText)
{
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();

    glTranslatef(vPos.x, vPos.y, vPos.z);
    //glScalef(0.015f, 1.0f, 0.01f); // height, _, width
    //glScalef(0.03f, 1.0f, 0.02f); // height, _, width
    //glScalef(0.04f, 1.0f, 0.03f); // height, _, width
    glScalef(0.06f, 0.06f, 0.04f); // height, _, width
    //glRotatef(-90.0f, 1.0f, 0.0f, 0.0f);
    //glRotatef(-90.0f, 0.0f, 0.0f, 1.0f);
    //glRotatef(90.0f, 0.0f, 1.0f, 0.0f);
    glRotatef(90.0f, 1.0f, 0.0f, 0.0f);

    glutStrokeString(GLUT_STROKE_MONO_ROMAN, (unsigned char *)sText.c_str());

    glMatrixMode(GL_MODELVIEW);
    glPopMatrix();
}

void World::DrawText2DUnmapped(mlVector3D vPos, const std::string & sText)
{
    GLint viewport[4];
    glGetIntegerv(GL_VIEWPORT, viewport);
    int nViewportWidth = viewport[2];
    int nViewportHeight = viewport[3];

    DrawText2D(vPos, sText, nViewportWidth, nViewportHeight);
}


void World::DrawText2D(mlVector3D vPos, const std::string & sText, float fWidth, float fHeight)
{
    glMatrixMode(GL_PROJECTION);
    glPushMatrix();

    glLoadIdentity();

    //glOrtho(-50,50,-37.5,37.5,0,1);
    glOrtho(0,fWidth,0,fHeight,0,1);

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();

    glLoadIdentity();

    glRasterPos2d(vPos.x, vPos.y);
    glutBitmapStringLeftAligned(GLUT_BITMAP_HELVETICA_18, (unsigned char *)sText.c_str());
    //glutBitmapStringCenterAligned(GLUT_BITMAP_HELVETICA_18, (unsigned char *)sText.c_str());
    //glutBitmapStringRightAligned(GLUT_BITMAP_HELVETICA_18, (unsigned char *)sText.c_str());

    //glutBitmapString(GLUT_BITMAP_HELVETICA_18, (unsigned char *)sText.c_str());
    // Get the new raster position
    // Do the center calculation based on this.

    glMatrixMode(GL_MODELVIEW);
    glPopMatrix();

    glMatrixMode(GL_PROJECTION);
    glPopMatrix();
}

std::string World::SelectBeginLines(const std::string & sText, int nNumLines)
{
    std::stringstream ss;
    ss << sText;

    std::string sOut;
    for(int i = 0; i < nNumLines; i++)
    {
        std::string sLine;
        std::getline(ss, sLine);
        sOut = sOut + sLine;
    }

    return sOut;
}

std::string World::SelectEndLines(const std::string & sText, int nNumLines)
{
    if(nNumLines <= 0)
        return "";

    if(sText.length() < 2)
        return "";

    string::size_type crpos = string::npos;

    if(sText.at(sText.length()-1) == '\n')
        crpos = sText.rfind("\n",sText.length()-2);
    else
        crpos = sText.rfind("\n");

    if(crpos == string::npos)
        return sText;

    for(int i = 1; i < nNumLines; i++)
    {
        crpos = sText.rfind("\n", crpos-1);

        if(crpos == string::npos)
            return sText;
    }

    return sText.substr(crpos);
}
