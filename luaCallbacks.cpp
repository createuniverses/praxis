
// Author:  Greg "fugue" Santucci, 2011
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/


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

#include "lispInterface.h"

#include "forthInterface.h"

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

extern VoxelBlock *     g_pVoxelBlock;

extern World *          g_pWorld;

extern bool g_bLuaRunning;

int g_nMidiUpdateInterval = 20;


int luaCBDrawLine(lua_State *L)
{
    int n = lua_gettop(L);

    float x1 = 0.0f;
    float y1 = 0.0f;
    float z1 = 0.0f;
    float x2 = 0.0f;
    float y2 = 0.0f;
    float z2 = 0.0f;

    if(n == 4)
    {
        x1 = luaL_checknumber(L, 1);
        y1 = luaL_checknumber(L, 2);

        x2 = luaL_checknumber(L, 3);
        y2 = luaL_checknumber(L, 4);
    }
    else if(n == 6)
    {
        x1 = luaL_checknumber(L, 1);
        y1 = luaL_checknumber(L, 2);
        z1 = luaL_checknumber(L, 3);

        x2 = luaL_checknumber(L, 4);
        y2 = luaL_checknumber(L, 5);
        z2 = luaL_checknumber(L, 6);
    }
    else
    {
        luaL_error(L, "Either 4 or 6 parameters allowed for drawLine.");
    }

    glBegin(GL_LINES);

    glVertex3f(x1,y1,z1);
    glVertex3f(x2,y2,z2);

    glEnd();

    return 0;
}

int luaCBGetCameraPosition(lua_State *L)
{
    mlVector3D vCamPos = g_pWorld->GetCameraTransform()->GetTranslation();

    lua_pushnumber(L, vCamPos.x);
    lua_pushnumber(L, vCamPos.y);
    lua_pushnumber(L, vCamPos.z);

    return 3;
}

int luaCBGetCameraForward(lua_State *L)
{
    mlVector3D vCamFwd = g_pWorld->GetCameraTransform()->GetMatrix().K;

    lua_pushnumber(L, vCamFwd.x);
    lua_pushnumber(L, vCamFwd.y);
    lua_pushnumber(L, vCamFwd.z);

    return 3;
}

int luaCBSetCameraPosition(lua_State *L)
{
    int n = lua_gettop(L);
    if(n!=3) luaL_error(L, "3 arguments expected.");

    mlVector3D vCamPos;

    vCamPos.x = luaL_checknumber(L, 1);
    vCamPos.y = luaL_checknumber(L, 2);
    vCamPos.z = luaL_checknumber(L, 3);

    g_pWorld->GetCameraTransform()->SetTranslation(vCamPos);

    return 0;
}

int luaCBRotateCamera(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=2) luaL_error(L, "2 arguments expected.");

    float fHeading = luaL_checknumber(L, 1);
    float fPitch   = luaL_checknumber(L, 2);

    g_pWorld->RotateCamera(fHeading, fPitch);

    return 0;
}

int luaCBLookAt(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=3) luaL_error(L, "3 arguments expected.");

    mlVector3D vCamTarget;

    vCamTarget.x = luaL_checknumber(L, 1);
    vCamTarget.y = luaL_checknumber(L, 2);
    vCamTarget.z = luaL_checknumber(L, 3);

    g_pWorld->LookAt(vCamTarget);

    return 0;
}

int luaCBOrbitCamPP(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=5) luaL_error(L, "5 arguments expected.");

    mlVector3D vCamTarget;

    vCamTarget.x = luaL_checknumber(L, 1);
    vCamTarget.y = luaL_checknumber(L, 2);
    vCamTarget.z = luaL_checknumber(L, 3);

    float fHeading = luaL_checknumber(L, 4);
    float fPitch   = luaL_checknumber(L, 5);

    g_pWorld->PositionPreservingOrbitCamera(vCamTarget, fHeading, fPitch);

    return 0;
}

int luaCBOrbitCamOP(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=5) luaL_error(L, "5 arguments expected.");

    mlVector3D vCamTarget;

    vCamTarget.x = luaL_checknumber(L, 1);
    vCamTarget.y = luaL_checknumber(L, 2);
    vCamTarget.z = luaL_checknumber(L, 3);

    float fHeading = luaL_checknumber(L, 4);
    float fPitch   = luaL_checknumber(L, 5);

    g_pWorld->OrientationPreservingOrbitCamera(vCamTarget, fHeading, fPitch);

    return 0;
}

int luaCBDrawCube(lua_State * L)
{
    int n = lua_gettop(L);

    mlVector3D vCorner;

    bool x1 = true;
    bool x2 = true;
    bool y1 = true;
    bool y2 = true;
    bool z1 = true;
    bool z2 = true;

    float fWidth = 1.0f;

    if(n==4)
    {
        vCorner.x = luaL_checknumber(L, 1);
        vCorner.y = luaL_checknumber(L, 2);
        vCorner.z = luaL_checknumber(L, 3);

        fWidth = luaL_checknumber(L, 4);
    }
    else if(n == 10)
    {
        vCorner.x = luaL_checknumber(L, 1);
        vCorner.y = luaL_checknumber(L, 2);
        vCorner.z = luaL_checknumber(L, 3);

        fWidth = luaL_checknumber(L, 4);

        x1 = lua_toboolean(L, 5);
        x2 = lua_toboolean(L, 6);
        y1 = lua_toboolean(L, 7);
        y2 = lua_toboolean(L, 8);
        z1 = lua_toboolean(L, 9);
        z2 = lua_toboolean(L, 10);
    }
    else
    {
        luaL_error(L, "Either 4 or 10 arguments expected.");
    }

    float fSize = fWidth * 0.5f;

    mlVector3D vCenter = vCorner + mlVector3D(fSize, fSize, fSize);

#   define V(a,b,c) glVertex3d( vCenter.x + a fSize, vCenter.y + b fSize, vCenter.z + c fSize );
//#   define N(a,b,c) glNormal3d( a, b, c );
#   define N(a,b,c)
    //glBegin( GL_QUADS );
        if(x2) { N( 1, 0, 0 ); V( +, -, + ); V( +, -, - ); V( +, +, - ); V( +, +, + ); }
        if(y2) { N( 0, 1, 0 ); V( +, +, + ); V( +, +, - ); V( -, +, - ); V( -, +, + ); }
        if(z2) { N( 0, 0, 1 ); V( +, +, + ); V( -, +, + ); V( -, -, + ); V( +, -, + ); }
        if(x1) { N( -1, 0, 0 ); V( -, -, + ); V( -, +, + ); V( -, +, - ); V( -, -, - ); }
        if(y1) { N( 0, -1, 0 ); V( -, -, + ); V( -, -, - ); V( +, -, - ); V( +, -, + ); }
        if(z1) { N( 0, 0, -1 ); V( -, -, - ); V( -, +, - ); V( +, +, - ); V( +, -, - ); }
    //glEnd( );
#   undef V
#   undef N

        // Extrude a line to make a square
        // Extrude a square to make a cube.

    return 0;
}

int luaCBBeginTrianglesGL(lua_State * L)
{
    glBegin(GL_TRIANGLES);

    return 0;
}

int luaCBBeginQuadsGL(lua_State * L)
{
    glBegin(GL_QUADS);

    return 0;
}

int luaCBBeginLinesGL(lua_State * L)
{
    glBegin(GL_LINES);

    return 0;
}

int luaCBEndGL(lua_State *L)
{
    glEnd();

    return 0;
}

int luaCBVectorGL(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=3) luaL_error(L, "3 arguments expected.");

    mlVector3D v;

    v.x = luaL_checknumber(L, 1);
    v.y = luaL_checknumber(L, 2);
    v.z = luaL_checknumber(L, 3);

    glVertex3f(v.x, v.y, v.z);

    return 0;
}

int luaCBTexGL(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=2) luaL_error(L, "2 arguments expected.");

    float s = luaL_checknumber(L, 1);
    float t = luaL_checknumber(L, 2);

    glTexCoord2f(s,t);

    return 0;
}

int luaCBColorGL(lua_State * L)
{
    int n = lua_gettop(L);

    if(n == 3)
    {
        int red   = luaL_checknumber(L, 1);
        int green = luaL_checknumber(L, 2);
        int blue  = luaL_checknumber(L, 3);
        int alpha = 255;

        glColor4ub(red, green, blue, alpha);
    }
    else if(n == 4)
    {
        int red   = luaL_checknumber(L, 1);
        int green = luaL_checknumber(L, 2);
        int blue  = luaL_checknumber(L, 3);
        int alpha = luaL_checknumber(L, 4);

        glColor4ub(red, green, blue, alpha);
    }
    else
    {
        luaL_error(L, "Either 3 or 4 arguments expected.");
    }

    return 0;
}

int luaCBSetLightPosGL(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=3) luaL_error(L, "3 arguments expected.");

    mlVector3D v;

    v.x = luaL_checknumber(L, 1);
    v.y = luaL_checknumber(L, 2);
    v.z = luaL_checknumber(L, 3);

    GLfloat lightPosition[4] = { v.x, v.y, v.z, 1.0 };
    glLightfv(GL_LIGHT0, GL_POSITION, lightPosition);

    GLfloat lightColor[4] = { 5.0f, 5.0f, 5.0f, 1.0 };
    glLightfv(GL_LIGHT0, GL_DIFFUSE, lightColor);

    return 0;
}

int luaCBNormalGL(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=3) luaL_error(L, "3 arguments expected.");

    mlVector3D v;

    v.x = luaL_checknumber(L, 1);
    v.y = luaL_checknumber(L, 2);
    v.z = luaL_checknumber(L, 3);

    glNormal3f(v.x, v.y, v.z);

    return 0;
}

int luaCBEnableLighting(lua_State * L)
{
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);
    glEnable(GL_LIGHT1);
    //glEnable(GL_MULTISAMPLE);

    return 0;
}

int luaCBDisableLighting(lua_State * L)
{
    glDisable(GL_LIGHTING);
    glDisable(GL_LIGHT0);
    glDisable(GL_LIGHT1);
    //glDisable(GL_MULTISAMPLE);

    return 0;
}

int luaCBEnableTexturing(lua_State * L)
{
    glEnable(GL_TEXTURE_2D);

    return 0;
}

int luaCBDisableTexturing(lua_State * L)
{
    glDisable(GL_TEXTURE_2D);

    return 0;
}

int luaCBEnablePolygonOffset(lua_State * L)
{
    glEnable(GL_POLYGON_OFFSET_FILL);
    return 0;
}

int luaCBDisablePolygonOffset(lua_State * L)
{
    glDisable(GL_POLYGON_OFFSET_FILL);
    return 0;
}

int luaCBSetPolygonOffset(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=2) luaL_error(L, "2 arguments expected.");

    float p1 = luaL_checknumber(L, 1);
    float p2 = luaL_checknumber(L, 2);

    glPolygonOffset (p1, p2);
    return 0;
}

int luaCBGetColorGL(lua_State * L)
{
    GLfloat col[4];

    glGetFloatv(GL_CURRENT_COLOR, col);

    lua_pushnumber(L, col[0] * 255.0);
    lua_pushnumber(L, col[1] * 255.0);
    lua_pushnumber(L, col[2] * 255.0);
    lua_pushnumber(L, col[3] * 255.0);

    return 4;
}

int luaCBGlutWireSphere(lua_State * L)
{
    float radius = luaL_checknumber(L, 1);
    float slices = 15;
    float stacks = 15;
    if(lua_gettop(L) > 1)
    {
        float slices = luaL_checknumber(L, 2);
        float stacks = luaL_checknumber(L, 3);
    }
    glutWireSphere(radius, slices, stacks);
    return 0;
}

int luaCBGlutSolidSphere(lua_State * L)
{
    float radius = luaL_checknumber(L, 1);
    float slices = 15;
    float stacks = 15;
    if(lua_gettop(L) > 1)
    {
        float slices = luaL_checknumber(L, 2);
        float stacks = luaL_checknumber(L, 3);
    }
    glutSolidSphere(radius, slices, stacks);
    return 0;
}

int luaCBGlutWireCube(lua_State * L)
{
    float size = luaL_checknumber(L, 1);
    glutWireCube(size);
    return 0;
}

int luaCBGlutSolidCube(lua_State * L)
{
    float size = luaL_checknumber(L, 1);
    glutSolidCube(size);
    return 0;
}

int luaCBGlutStrokeString(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    std::string sText = luaL_checkstring(L, 1);

    glutStrokeString(GLUT_STROKE_MONO_ROMAN, (unsigned char *)sText.c_str());

    return 0;
}

int luaCBGlutBitmapString(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 arguments expected.");

    std::string sText = luaL_checkstring(L, 1);

    glutBitmapString(GLUT_BITMAP_HELVETICA_18, (unsigned char *)sText.c_str());

    return 0;
}

int luaCBGLRasterPos(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=3) luaL_error(L, "3 arguments expected.");

    float x = luaL_checknumber(L, 1);
    float y = luaL_checknumber(L, 2);
    float z = luaL_checknumber(L, 3);

    glRasterPos3d(x,y,z);

    return 0;
}

int luaCBGLMatrixModeProjection(lua_State * L)
{
    glMatrixMode(GL_PROJECTION);
    return 0;
}

int luaCBGLMatrixModeModelView(lua_State * L)
{
    glMatrixMode(GL_MODELVIEW);
    return 0;
}

int luaCBGLPushMatrix(lua_State * L)
{
    glPushMatrix();
    return 0;
}

int luaCBGLPopMatrix(lua_State * L)
{
    glPopMatrix();
    return 0;
}

int luaCBGLScale(lua_State * L)
{
    float x = luaL_checknumber(L, 1);
    float y = luaL_checknumber(L, 2);
    float z = luaL_checknumber(L, 3);
    glScalef(x,y,z);
    return 0;
}

int luaCBGLTranslate(lua_State * L)
{
    float x = luaL_checknumber(L, 1);
    float y = luaL_checknumber(L, 2);
    float z = luaL_checknumber(L, 3);
    glTranslatef(x,y,z);
    return 0;
}

int luaCBGLRotate(lua_State * L)
{
    float a = luaL_checknumber(L, 1);
    float x = luaL_checknumber(L, 2);
    float y = luaL_checknumber(L, 3);
    float z = luaL_checknumber(L, 4);
    glRotatef(a,x,y,z);
    return 0;
}

int luaCBGLApplyTransform(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 arguments expected.");

    mlTransform * t = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");

    glMatrixMode(GL_MODELVIEW);

//    mlTransform transform = m_trnCamera;
//    transform.Invert();

    mlMatrix4x4 mat(t->GetMatrix());

    mlFloat * pMat = reinterpret_cast<mlFloat*>(&mat);

    glMultMatrixf(pMat);

    return 0;
}

int luaCBGLLoadIdentity(lua_State * L)
{
    glLoadIdentity();
    return 0;
}

int luaCBUseMainWindowContext(lua_State *L)
{
    UseMainWindowContext();
    return 0;
}

int luaCBUseOffscreenContext(lua_State * L)
{
    UseOffscreenContext();
    return 0;
}

int luaCBGLPerspective(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=4) luaL_error(L, "4 arguments expected.");

    float fFieldOfView = luaL_checknumber(L, 1);
    float fAspectRatio = luaL_checknumber(L, 2);
    float fNearClip    = luaL_checknumber(L, 3);
    float fFarClip     = luaL_checknumber(L, 4);

    gluPerspective(fFieldOfView, fAspectRatio, fNearClip, fFarClip);

    return 0;
}

int luaCBGLOrtho(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=6) luaL_error(L, "6 arguments expected.");

    float fLeft        = luaL_checknumber(L, 1);
    float fRight       = luaL_checknumber(L, 2);
    float fBottom      = luaL_checknumber(L, 3);
    float fTop         = luaL_checknumber(L, 4);
    float fNearClip    = luaL_checknumber(L, 5);
    float fFarClip     = luaL_checknumber(L, 6);

    glOrtho(fLeft, fRight, fBottom, fTop, fNearClip, fFarClip);

    return 0;
}

int luaCBGLViewport(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=4) luaL_error(L, "4 arguments expected.");

    float x = luaL_checknumber(L, 1);
    float y = luaL_checknumber(L, 2);
    float w = luaL_checknumber(L, 3);
    float h = luaL_checknumber(L, 4);

    glViewport(x,y,w,h);

    return 0;
}

int luaCBEdGetRenderMode(lua_State * L)
{
    lua_pushnumber(L, GLEditor::m_nRenderMode);
    return 1;
}

int luaCBEdSetRenderMode(lua_State * L)
{
    GLEditor::m_nRenderMode = luaL_checknumber(L, 1);

    if(GLEditor::m_nRenderMode < 0 || GLEditor::m_nRenderMode > 2)
        GLEditor::m_nRenderMode = 0;

    return 0;
}

int luaCBEdGetFlashRate(lua_State * L)
{
    lua_pushnumber(L, GLEditor::m_fFlashRate);
    return 1;
}

int luaCBEdSetFlashRate(lua_State * L)
{
    GLEditor::m_fFlashRate = luaL_checknumber(L, 1);
    return 0;
}

int luaCBEdGetTopPosition(lua_State * L)
{
    lua_pushnumber(L, g_pWorld->GetEditor()->GetFirstVisiblePosition());
    return 1;
}

int luaCBEdGetBottomPosition(lua_State * L)
{
    lua_pushnumber(L, g_pWorld->GetEditor()->GetLastVisiblePosition());
    return 1;
}

int luaCBEdGetVisibleLines(lua_State * L)
{
    lua_pushnumber(L, g_pWorld->GetEditor()->m_VisibleLines);
    return 1;
}

int luaCBEdSetVisibleLines(lua_State * L)
{
    int nVisibleLines = luaL_checknumber(L, 1);
    g_pWorld->GetEditor()->m_VisibleLines = nVisibleLines;
    return 0;
}

int luaCBEdGetPosition(lua_State * L)
{
    lua_pushnumber(L, g_pWorld->GetEditor()->m_Position);
    return 1;
}

int luaCBEdSetPosition(lua_State * L)
{
    int nPosition = luaL_checknumber(L, 1);
    g_pWorld->GetEditor()->m_Position = nPosition;
    return 0;
}

int luaCBEdInsertNewline(lua_State *L)
{
    g_pWorld->GetEditor()->InsertNewline();
    return 0;
}

int luaCBEdGetHighlightAnchor(lua_State * L)
{
    lua_pushnumber(L, g_pWorld->GetEditor()->m_HighlightAnchor);
    return 1;
}

int luaCBEdSetHighlightAnchor(lua_State * L)
{
    int nAnchor = luaL_checknumber(L, 1);
    g_pWorld->GetEditor()->m_HighlightAnchor = nAnchor;
    g_pWorld->GetEditor()->m_Selection = true;
    return 0;
}

int luaCBEdGetLeftPosition(lua_State * L)
{
    lua_pushnumber(L, g_pWorld->GetEditor()->m_LeftTextPosition);
    return 1;
}

int luaCBGLClear(lua_State * L)
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT );
    return 0;
}

int luaCBGLBuildStencil(lua_State * L)
{
    bool bInverted = luaL_checkinteger(L, 1);
    //GLenum eGLError = glGetError();

    glEnable(GL_STENCIL_TEST);

    if(bInverted)
        glClearStencil(1);
    else
        glClearStencil(0);

    glClear(GL_STENCIL_BUFFER_BIT);

    glStencilFunc(GL_NEVER, 1, 0xFF);
    glStencilOp(GL_REPLACE, GL_KEEP, GL_KEEP);  // draw 1s on test fail (always)

    glColorMask(0,0,0,0);
    glStencilFunc(GL_ALWAYS, 1, 1);

    if(bInverted)
        glStencilOp(GL_KEEP, GL_KEEP, GL_ZERO);
    else
        glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);

    glDisable(GL_DEPTH_TEST);

    return 0;
}

int luaCBGLDrawWithinStencil(lua_State * L)
{
    glColorMask(1,1,1,1);								// Set Color Mask to TRUE, TRUE, TRUE, TRUE
    glStencilFunc(GL_EQUAL, 1, 1);       				// We Draw Only Where The Stencil Is 1
                                                        // (I.E. Where The stencil Was Drawn)
    glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);				// Don't Change The Stencil Buffer

    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LESS);

    return 0;
}

int luaCBGLRemoveStencil(lua_State * L)
{
    glDisable(GL_STENCIL_TEST);							// We Don't Need The Stencil Buffer Any More (Disable)
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LESS);

    return 0;
}

int luaCBGLResetStencil(lua_State * L)
{
    glClearStencil(1);
    glClear(GL_STENCIL_BUFFER_BIT);
    glColorMask(1,1,1,1);
    glDisable(GL_STENCIL_TEST);							// We Don't Need The Stencil Buffer Any More (Disable)
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LESS);
    return 0;
}

int luaCBGLShowStencil(lua_State * L)
{
    return 0;
}

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

int luaCBSetBufferVisColumns(lua_State * L)
{
    int arg = luaL_checknumber(L,1);
    g_pWorld->GetEditor()->m_VisibleColumns = arg;

    return 0;
}

int luaCBSetBufferVisLines(lua_State * L)
{
    int arg = luaL_checknumber(L,1);
    g_pWorld->GetEditor()->m_VisibleLines = arg;

    return 0;
}

int luaCBSetBufferPosY(lua_State * L)
{
    int arg = luaL_checknumber(L,1);
    g_pWorld->GetEditor()->m_PosY = arg;

    return 0;
}

int luaCBGetBufferBB(lua_State * L)
{
    float minX,minY,maxX,maxY;
    g_pWorld->GetEditor()->GetBB(minX,minY,maxX,maxY);
    lua_pushnumber(L, minX);
    lua_pushnumber(L, minY);
    lua_pushnumber(L, maxX);
    lua_pushnumber(L, maxY);
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

int luaCBQtQuit(lua_State * L)
{
    //QCoreApplication::quit();

    return 0;
}

//static void qt_save_gl_state()
//{
//    glPushClientAttrib(GL_CLIENT_ALL_ATTRIB_BITS);
//    glPushAttrib(GL_ALL_ATTRIB_BITS);
//    glMatrixMode(GL_TEXTURE);
//    glPushMatrix();
//    glLoadIdentity();
//    glMatrixMode(GL_PROJECTION);
//    glPushMatrix();
//    glMatrixMode(GL_MODELVIEW);
//    glPushMatrix();

//    glShadeModel(GL_FLAT);
//    glDisable(GL_CULL_FACE);
//    glDisable(GL_LIGHTING);
//    glDisable(GL_STENCIL_TEST);
//    glDisable(GL_DEPTH_TEST);
//    glEnable(GL_BLEND);
//    glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
//}

//static void qt_restore_gl_state()
//{
//    glMatrixMode(GL_TEXTURE);
//    glPopMatrix();
//    glMatrixMode(GL_PROJECTION);
//    glPopMatrix();
//    glMatrixMode(GL_MODELVIEW);
//    glPopMatrix();
//    glPopAttrib();
//    glPopClientAttrib();
//}

static inline void transform_point(GLdouble out[4], const GLdouble m[16], const GLdouble in[4])
{
#define M(row,col)  m[col*4+row]
    out[0] =
        M(0, 0) * in[0] + M(0, 1) * in[1] + M(0, 2) * in[2] + M(0, 3) * in[3];
    out[1] =
        M(1, 0) * in[0] + M(1, 1) * in[1] + M(1, 2) * in[2] + M(1, 3) * in[3];
    out[2] =
        M(2, 0) * in[0] + M(2, 1) * in[1] + M(2, 2) * in[2] + M(2, 3) * in[3];
    out[3] =
        M(3, 0) * in[0] + M(3, 1) * in[1] + M(3, 2) * in[2] + M(3, 3) * in[3];
#undef M
}

static inline GLint qgluProject(GLdouble objx, GLdouble objy, GLdouble objz,
           const GLdouble model[16], const GLdouble proj[16],
           const GLint viewport[4],
           GLdouble * winx, GLdouble * winy, GLdouble * winz)
{
   GLdouble in[4], out[4];

   in[0] = objx;
   in[1] = objy;
   in[2] = objz;
   in[3] = 1.0;
   transform_point(out, model, in);
   transform_point(in, proj, out);

   if (in[3] == 0.0)
      return GL_FALSE;

   in[0] /= in[3];
   in[1] /= in[3];
   in[2] /= in[3];

   *winx = viewport[0] + (1 + in[0]) * viewport[2] / 2;
   *winy = viewport[1] + (1 + in[1]) * viewport[3] / 2;

   *winz = (1 + in[2]) / 2;
   return GL_TRUE;
}

int luaCBDrawText3D(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=4) luaL_error(L, "4 arguments expected.");

    std::string sString = luaL_checkstring(L, 1);

    float x = luaL_checknumber(L, 2);
    float y = luaL_checknumber(L, 3);
    float z = luaL_checknumber(L, 4);

    World::DrawText3D(mlVector3D(x,y,z), sString);

    return 0;
}

int luaCBDrawText3DStroked(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=4) luaL_error(L, "4 arguments expected.");

    std::string sString = luaL_checkstring(L, 1);

    float x = luaL_checknumber(L, 2);
    float y = luaL_checknumber(L, 3);
    float z = luaL_checknumber(L, 4);

    World::DrawText3DStroked(mlVector3D(x,y,z), sString);

    return 0;
}

int luaCBDrawText2D(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=3) luaL_error(L, "3 arguments expected.");

    std::string sString = luaL_checkstring(L, 1);

    float x = luaL_checknumber(L, 2);
    float y = luaL_checknumber(L, 3);

    World::DrawText2D(mlVector3D(x,y), sString);

    return 0;
}

int luaCBDrawText2DCentered(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=3) luaL_error(L, "3 arguments expected.");

    std::string sString = luaL_checkstring(L, 1);

    float x = luaL_checknumber(L, 2);
    float y = luaL_checknumber(L, 3);

    World::DrawText2D(mlVector3D(x,y), sString);

    return 0;
}

class LiveCodeTexture
{
public:
    LiveCodeTexture()
    {
        glGenTextures(1,&nTextureID);
        glBindTexture(GL_TEXTURE_2D,nTextureID);
        glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        //glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        //glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER, GL_LINEAR);

        //nSize = 512;
        nSize = 128;

        for(int i = 0; i < nSize * nSize * 3; i++)
            pixels[i] = rand() % 200;

        glTexImage2D(GL_TEXTURE_2D, 0, 3, nSize, nSize, 0, GL_RGB, GL_UNSIGNED_BYTE, pixels);
        //glTexImage2D(GL_TEXTURE_2D, 0, 4, nSize, nSize, 0, GL_RGBA, GL_UNSIGNED_BYTE, pixels);

        sRenderFunction = "";
    }

    void Clear()
    {
        for(int i = 0; i < nSize * nSize * 3; i++)
            pixels[i] = rand() % 200;
    }

    void UpdateTexture()
    {
        // http://www.gamedev.net/page/resources/_/technical/opengl/moving-beyond-opengl-11-for-windows-r1929
        // http://stackoverflow.com/questions/7808146/gltexsubimage2d-extremely-slow-on-intel-video-card
        // http://www.clearchain.com/blog/posts/opengl-gltexsubimage2d-very-slow-a-solution
        // https://developer.palm.com/distribution/viewtopic.php?f=70&t=11066
        // http://www.khronos.org/message_boards/viewtopic.php?f=4&t=1274

        Begin();

        luaCall(sRenderFunction);

        End();
    }

    void Begin()
    {
        UseOffscreenContext();

        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT );

        glViewport(0,0,nSize,nSize);

//        for(int i = 0; i < nSize * nSize * 3; i++)
//            pixels[i] = rand() % 200;

        {
            glMatrixMode(GL_PROJECTION);
            glPushMatrix();

            glLoadIdentity();

            glOrtho(0,100,0,100,0,10);

            glMatrixMode(GL_MODELVIEW);
            glPushMatrix();

            glLoadIdentity();

            glTranslatef(0,0,-5);

            glRasterPos3d(0,0,-4);
            glDrawPixels(nSize, nSize, GL_RGB, GL_UNSIGNED_BYTE, (GLvoid *)pixels);
            //glutBitmapString(GLUT_BITMAP_HELVETICA_18, (const unsigned char *)"ahoha!");

            glMatrixMode(GL_MODELVIEW);
            glPopMatrix();

            glMatrixMode(GL_PROJECTION);
            glPopMatrix();
        }
    }

    void End()
    {
        glReadPixels(
            0, 0, nSize,nSize,
            GL_RGB, GL_UNSIGNED_BYTE, (GLvoid *)pixels);

        UseMainWindowContext();

        glBindTexture(GL_TEXTURE_2D,nTextureID);

        glTexSubImage2D (
            GL_TEXTURE_2D, 0, 0, 0, nSize, nSize,
            GL_RGB, GL_UNSIGNED_BYTE, (void *)pixels);
    }

    GLuint nTextureID;
    std::string sRenderFunction;
    int nSize;
    char pixels [512*512*3];
    //char pixels [1024*1024*3];
};

int luaCBTextureNew(lua_State * L)
{
    // return the GL index of the texture and a pointer to the QImage itself.

    LiveCodeTexture ** c = (LiveCodeTexture **)lua_newuserdata(L, sizeof(LiveCodeTexture *));

    *c = new LiveCodeTexture();

    luaL_getmetatable(L, "LiveCode.texture");
    lua_setmetatable(L, -2);

    return 1;
}

int luaCBTextureGC(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    LiveCodeTexture * c = *(LiveCodeTexture **)luaL_checkudata(L, 1, "LiveCode.texture");

    delete c;

    return 0;
}

int luaCBTextureClear(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    LiveCodeTexture * c = *(LiveCodeTexture **)luaL_checkudata(L, 1, "LiveCode.texture");

    c->Clear();

    return 0;
}

int luaCBTextureSelect(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    LiveCodeTexture * c = *(LiveCodeTexture **)luaL_checkudata(L, 1, "LiveCode.texture");

    glBindTexture(GL_TEXTURE_2D, c->nTextureID);

    return 0;
}

int luaCBTextureUpdate(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    LiveCodeTexture * c = *(LiveCodeTexture **)luaL_checkudata(L, 1, "LiveCode.texture");

    c->UpdateTexture();

    return 0;
}

int luaCBTextureBegin(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    LiveCodeTexture * c = *(LiveCodeTexture **)luaL_checkudata(L, 1, "LiveCode.texture");

    c->Begin();

    return 0;
}

int luaCBTextureEnd(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    LiveCodeTexture * c = *(LiveCodeTexture **)luaL_checkudata(L, 1, "LiveCode.texture");

    c->End();

    return 0;
}

int luaCBTextureSetRenderFunction(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=2) luaL_error(L, "2 arguments expected.");

    LiveCodeTexture * c = *(LiveCodeTexture **)luaL_checkudata(L, 1, "LiveCode.texture");

    std::string sRenderFunction = luaL_checkstring(L, 2);

    c->sRenderFunction = sRenderFunction;

    return 0;
}

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

int luaCBTransformLookAt(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=4) luaL_error(L, "4 arguments expected.");

    mlTransform * t = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");

    mlVector3D vFocalPoint(luaL_checknumber(L, 2), luaL_checknumber(L, 3), luaL_checknumber(L, 4));

    mlVector3D vForward = vFocalPoint - t->GetTranslation();
    //mlVector3D vForward = t->GetTranslation() - vFocalPoint;

    vForward.Normalise();

    mlQuaternion rotLookAt = mlQuaternionFromDirection(vForward, mlVector3D(0.0f, 1.0f, 0.0f));

    t->SetRotation(rotLookAt);

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

int luaCBTransformCopyFrom(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=2) luaL_error(L, "2 arguments expected.");

    mlTransform * t1 = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");
    mlTransform * t2 = *(mlTransform **)luaL_checkudata(L, 2, "LiveCode.transform");

    (*t1) = (*t2);

    return 0;
}

int luaCBTransformTransform(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=2) luaL_error(L, "2 arguments expected.");

    mlTransform * t1 = *(mlTransform **)luaL_checkudata(L, 1, "LiveCode.transform");
    mlTransform * t2 = *(mlTransform **)luaL_checkudata(L, 2, "LiveCode.transform");

    t1->TransformSelf(*t2);

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
    luaClearOutput();
    return 0;
}

int luaCBGetTraceText(lua_State * L)
{
    lua_pushstring(L, luaGetOutput().c_str());
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
    luaClearError();
    return 0;
}

int luaCBGetErrorText(lua_State * L)
{
    lua_pushstring(L, luaGetError().c_str());
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

int luaCBNewBuffer(lua_State * L)
{
    g_pWorld->NewEditor();

    int n = lua_gettop(L);
    if(n>=1)
    {
        std::string sName = luaL_checkstring(L, 1);
        g_pWorld->GetEditor()->SetName(sName);
    }

    return 0;
}

int luaCBCloseBuffer(lua_State * L)
{
    g_pWorld->CloseEditor();

    return 0;
}

int luaCBSetBufferText(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    std::string sText = luaL_checkstring(L, 1);

    g_pWorld->GetEditor()->SetText(sText);

    return 0;
}

int luaCBInsertBufferText(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    std::string sText = luaL_checkstring(L, 1);

    g_pWorld->GetEditor()->InsertText(sText);

    return 0;
}

int luaCBGetBufferText(lua_State * L)
{
    lua_pushstring(L, g_pWorld->GetEditor()->GetAllText().c_str());

    return 1;
}

int luaCBLoadBuffer(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    std::string sFilename = luaL_checkstring(L, 1);

    //g_pWorld->NewEditor();
    g_pWorld->GetEditor()->Load(sFilename);

    return 0;
}

int luaCBReadFile(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    std::string sFilename = luaL_checkstring(L, 1);

    std::string sFileText = GLEditor::ReadFileToString(sFilename);

    lua_pushstring(L, sFileText.c_str());
    return 1;
}

int luaCBSaveBuffer(lua_State *L)
{
    int n = lua_gettop(L);
    if(n>=1)
    {
        std::string sFilename = luaL_checkstring(L, 1);
        g_pWorld->GetEditor()->SaveAs(sFilename);
    }
    else
    {
        g_pWorld->GetEditor()->Save();
    }

    return 0;
}

int luaCBGetBufferName(lua_State * L)
{
    lua_pushstring(L, g_pWorld->GetEditor()->GetName().c_str());
    return 1;
}

int luaCBSetBufferName(lua_State * L)
{
    std::string sName = luaL_checkstring(L, 1);
    g_pWorld->GetEditor()->SetName(sName);

    return 0;
}

int luaCBGetParentBufferName(lua_State * L)
{
    lua_pushstring(L, g_pWorld->GetEditor()->GetParentName().c_str());
    return 1;
}

int luaCBSwitchToBuffer(lua_State * L)
{
    std::string sName = luaL_checkstring(L, 1);
    g_pWorld->SwitchToEditor(sName);
    return 0;
}

int luaCBGotoBufferStart(lua_State * L)
{
    g_pWorld->GetEditor()->MoveCursorToStart();

    return 0;
}

int luaCBGotoBufferEnd(lua_State * L)
{
    g_pWorld->GetEditor()->MoveCursorToEnd();

    return 0;
}

int luaCBParseParentheses(lua_State * L)
{
    int bias = luaL_checknumber(L, 1);
    GLEditor::IndexPair parens = g_pWorld->GetEditor()->ParseParentheses(bias);
    lua_pushnumber(L, parens.n1);
    lua_pushnumber(L, parens.n2);


    // Temporary, this should be moved to a separate lua function
    g_pWorld->GetEditor()->m_ParenthesesHighlight[0] = parens.n1;
    g_pWorld->GetEditor()->m_ParenthesesHighlight[1] = parens.n2;
    return 2;
}

int luaCBEdSetHighlight(lua_State * L)
{
    int n1 = luaL_checknumber(L, 1);
    int n2 = luaL_checknumber(L, 2);
    g_pWorld->GetEditor()->m_ParenthesesHighlight[0] = n1;
    g_pWorld->GetEditor()->m_ParenthesesHighlight[1] = n2;
    return 0;
}

int luaCBSetSearchText(lua_State * L)
{
    return 0;
}

int luaCBGetSearchText(lua_State * L)
{
    return 0;
}

int luaCBFindNext(lua_State * L)
{
    return 0;
}

int luaCBFindPrevious(lua_State * L)
{
    return 0;
}

int luaCBGetEditorSelectedText(lua_State * L)
{
    lua_pushstring(L, g_pWorld->GetEditor()->GetText().c_str());

    return 1;
}

int luaCBGetEditorSExpr(lua_State * L)
{
    lua_pushstring(L, g_pWorld->GetEditor()->GetSExpr().c_str());

    return 1;
}

int luaCBGetEditorLineStart(lua_State * L)
{
    lua_pushnumber(L, g_pWorld->GetEditor()->LineStart(g_pWorld->GetEditor()->GetPosition()));
    return 1;
}

int luaCBGetEditorLineEnd(lua_State * L)
{
    lua_pushnumber(L, g_pWorld->GetEditor()->LineEnd(g_pWorld->GetEditor()->GetPosition()));
    return 1;
}

int luaCBGetEditorLineText(lua_State * L)
{
    lua_pushstring(L, g_pWorld->GetEditor()->GetCurrentLineText().c_str());
    return 1;
}

int luaCBGotoEditorLine(lua_State * L)
{
    return 0;
}

int luaCBGetEditorLine(lua_State * L)
{
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

int luaCBSelectLines(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=3) luaL_error(L, "3 arguments expected.");

    std::string sText = luaL_checkstring(L, 1);
    //int nNumLines = luaL_checknumber(L, 2);
    int nFirstLine = luaL_checknumber(L, 2);
    int nLastLine  = luaL_checknumber(L, 3);
    //std::string sOutput = World::SelectBeginLines(sText, nNumLines);
    //std::string sOutput = World::SelectEndLines(sText, nNumLines);
    std::string sOutput = GLEditor::ReadLinesFromString(sText, nFirstLine, nLastLine);
    lua_pushstring(L, sOutput.c_str());
    return 1;
}

int luaCBReadFileToString(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    std::string sFilename = luaL_checkstring(L, 1);
    //int nFirstLine = luaL_checknumber(L, 2);
    //int nLastLine  = luaL_checknumber(L, 3);

    std::string sText = GLEditor::ReadFileToString(sFilename);
    //std::string sLines = GLEditor::ReadLinesFromString(sText, nFirstLine, nLastLine);

    //lua_pushstring(L, sLines.c_str());
    lua_pushstring(L, sText.c_str());

    return 1;
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

#ifdef __PRAXIS_LINUX__
typedef int SOCKET;
int INVALID_SOCKET = -1;
int SOCKET_ERROR = -1;
#endif

SOCKET ListeningSocket;

SOCKET SetUpListener(const char* pcAddress, int nPort)
{
    u_long nInterfaceAddr = inet_addr(pcAddress);
    if (nInterfaceAddr != INADDR_NONE) {
        SOCKET sd = socket(AF_INET, SOCK_STREAM, 0);
        if (sd != INVALID_SOCKET) {
            sockaddr_in sinInterface;
            sinInterface.sin_family = AF_INET;
            sinInterface.sin_addr.s_addr = nInterfaceAddr;
            sinInterface.sin_port = nPort;
            if (bind(sd, (sockaddr*)&sinInterface,
                    sizeof(sockaddr_in)) != SOCKET_ERROR) {
                listen(sd, 1);
                return sd;
            }
        }
    }

    return INVALID_SOCKET;
}

int luaCBStartServer(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    // Start Winsock up
    WSAData wsaData;
    int nCode;
    if ((nCode = WSAStartup(MAKEWORD(1, 1), &wsaData)) != 0) {
//        cerr << "WSAStartup() returned error code " << nCode << "." <<
//                endl;
//        return 255;
        return 0;
    }
#endif

    // Begin listening for connections
    cout << "Establishing the listener..." << endl;
    ListeningSocket = INVALID_SOCKET;
    u_long nInterfaceAddr = inet_addr("127.0.0.1");
    if (nInterfaceAddr != INADDR_NONE) {
        ListeningSocket = socket(AF_INET, SOCK_STREAM, 0);
        if (ListeningSocket != INVALID_SOCKET) {

            sockaddr_in sinInterface;
            sinInterface.sin_family = AF_INET;
            sinInterface.sin_addr.s_addr = nInterfaceAddr;
            sinInterface.sin_port = htons(4242);
            if (bind(ListeningSocket, (sockaddr*)&sinInterface,
                    sizeof(sockaddr_in)) != SOCKET_ERROR) {
                listen(ListeningSocket, 1);
            }
        }
    }

    if (ListeningSocket == INVALID_SOCKET) {
//        cout << endl << WSAGetLastErrorMessage("establish listener") <<
//                endl;
//        return 3;
        return 0;
    }

    u_long iMode = 1;
    ioctl(ListeningSocket, FIONBIO, &iMode);
    //ioctlsocket(ListeningSocket, FIONBIO, &iMode);

    return 0;
}

int luaCBAcceptConnection(lua_State * L)
{
    cout << "Waiting for a connection..." << flush;
    sockaddr_in sinRemote;
    int nAddrSize = sizeof(sinRemote);
    //SOCKET sd = accept(ListeningSocket, (sockaddr*)&sinRemote, &nAddrSize);
    SOCKET sd = accept(ListeningSocket, (struct sockaddr*)&sinRemote, (socklen_t *)&nAddrSize);
    if (sd != INVALID_SOCKET) {
        cout << "Accepted connection from " <<
                inet_ntoa(sinRemote.sin_addr) << ":" <<
                ntohs(sinRemote.sin_port) << "." << endl;

        ListeningSocket = sd;

        u_long iMode = 1;
        ioctl(ListeningSocket, FIONBIO, &iMode);
    }
    else {
//        cout << endl << WSAGetLastErrorMessage(
//                "accept connection") << endl;
    }

    // Return whether a connection was accepted
    return 0;
}

int luaCBReceiveData(lua_State * L)
{
    char buf[64000];
    buf[0] = '\0';
    int nBytes = recv(ListeningSocket, buf, 64000, 0);
    buf[nBytes] = '\0';
    //std::string sBuf = buf;

    lua_pushstring(L, buf);

    // Return any data received
    return 1;
}

int luaCBSendData(lua_State * L)
{
    std::string sText = luaL_checkstring(L, 1);

    int nReadBytes = sText.length();
    char buf[64000];
    strcpy(buf, sText.c_str());

    int nSentBytes = 0;
    while (nSentBytes < nReadBytes) {
        int nTemp = send(ListeningSocket, buf + nSentBytes,
                nReadBytes - nSentBytes, 0);
        if (nTemp > 0) {
            cout << "Sent " << nTemp <<
                    " bytes back to client." << endl;
            nSentBytes += nTemp;
        }
        else if (nTemp == SOCKET_ERROR) {
            cout << "Socket error" <<
                    endl;
            return 0;
        }
        else {
            // Client closed connection before we could reply to
            // all the data it sent, so bomb out early.
            cout << "Peer unexpectedly dropped connection!" <<
                    endl;
            return 0;
        }
    }

    //send(ListeningSocket, sText.c_str(), sText.length(), 0);

    return 0;
}

int luaCBShutdownServer(lua_State * L)
{
    return 0;
}

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

    lua_register(g_pLuaState, "drawLine",              luaCBDrawLine);
    lua_register(g_pLuaState, "getCamPos",             luaCBGetCameraPosition);
    lua_register(g_pLuaState, "setCamPos",             luaCBSetCameraPosition);
    lua_register(g_pLuaState, "getCamFwd",             luaCBGetCameraForward);
    lua_register(g_pLuaState, "rotateCam",             luaCBRotateCamera);
    lua_register(g_pLuaState, "lookAt",                luaCBLookAt);
    lua_register(g_pLuaState, "orbitCamPP",            luaCBOrbitCamPP);
    lua_register(g_pLuaState, "orbitCamOP",            luaCBOrbitCamOP);

    // glBegin
    lua_register(g_pLuaState, "beginTriGL",            luaCBBeginTrianglesGL);
    lua_register(g_pLuaState, "glBeginTriangles",      luaCBBeginTrianglesGL);
    lua_register(g_pLuaState, "beginQuadGL",           luaCBBeginQuadsGL);
    lua_register(g_pLuaState, "glBeginQuads",          luaCBBeginQuadsGL);
    lua_register(g_pLuaState, "beginLinGL",            luaCBBeginLinesGL);
    lua_register(g_pLuaState, "glBeginLines",          luaCBBeginLinesGL);
    // glEnd
    lua_register(g_pLuaState, "endGL",                 luaCBEndGL);
    lua_register(g_pLuaState, "glEnd",                 luaCBEndGL);
    // glVertex
    lua_register(g_pLuaState, "vectorGL",              luaCBVectorGL);
    lua_register(g_pLuaState, "glVertex",              luaCBVectorGL);

    lua_register(g_pLuaState, "texGL",                 luaCBTexGL);
    lua_register(g_pLuaState, "colorGL",               luaCBColorGL);
    lua_register(g_pLuaState, "getColorGL",            luaCBGetColorGL);
    lua_register(g_pLuaState, "lightGL",               luaCBSetLightPosGL);
    lua_register(g_pLuaState, "normalGL",              luaCBNormalGL);
    lua_register(g_pLuaState, "enableLighting",        luaCBEnableLighting);
    lua_register(g_pLuaState, "disableLighting",       luaCBDisableLighting);
    lua_register(g_pLuaState, "enableTexturing",       luaCBEnableTexturing);
    lua_register(g_pLuaState, "disableTexturing",      luaCBDisableTexturing);

    lua_register(g_pLuaState, "enablePolygonOffset",   luaCBEnablePolygonOffset);
    lua_register(g_pLuaState, "disablePolygonOffset",  luaCBDisablePolygonOffset);
    lua_register(g_pLuaState, "setPolygonOffset",      luaCBSetPolygonOffset);

    lua_register(g_pLuaState, "getClipboardText",      luaCBGetClipboardText);
    lua_register(g_pLuaState, "setClipboardText",      luaCBSetClipboardText);
    lua_register(g_pLuaState, "clearClipboardText",    luaCBClearClipboardText);


    lua_register(g_pLuaState, "drawCube",              luaCBDrawCube);

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

    lua_register(g_pLuaState, "newBuffer",             luaCBNewBuffer);
    lua_register(g_pLuaState, "closeBuffer",           luaCBCloseBuffer);
    lua_register(g_pLuaState, "setBufferText",         luaCBSetBufferText);
    lua_register(g_pLuaState, "insertBufferText",      luaCBInsertBufferText);
    lua_register(g_pLuaState, "getBufferText",         luaCBGetBufferText);
    lua_register(g_pLuaState, "loadBuffer",            luaCBLoadBuffer);
    lua_register(g_pLuaState, "saveBuffer",            luaCBSaveBuffer);
    lua_register(g_pLuaState, "getBufferName",         luaCBGetBufferName);
    lua_register(g_pLuaState, "setBufferName",         luaCBSetBufferName);
    lua_register(g_pLuaState, "getParentBufferName",   luaCBGetParentBufferName);
    lua_register(g_pLuaState, "switchToBuffer",        luaCBSwitchToBuffer);
    lua_register(g_pLuaState, "readFile",              luaCBReadFile);

    lua_register(g_pLuaState, "edParseParentheses",    luaCBParseParentheses);
    lua_register(g_pLuaState, "edSetHighlight",        luaCBEdSetHighlight);

    lua_register(g_pLuaState, "setVisColumns",         luaCBSetBufferVisColumns);
    lua_register(g_pLuaState, "setVisLines",           luaCBSetBufferVisLines);
    lua_register(g_pLuaState, "setBufferPosY",         luaCBSetBufferPosY);
    lua_register(g_pLuaState, "getBufferBB",           luaCBGetBufferBB);

    lua_register(g_pLuaState, "gotoBufferStart",       luaCBGotoBufferStart);
    lua_register(g_pLuaState, "gotoBufferEnd",         luaCBGotoBufferEnd);

    lua_register(g_pLuaState, "getEditorLineStart",    luaCBGetEditorLineStart);
    lua_register(g_pLuaState, "getEditorLineEnd",      luaCBGetEditorLineEnd);
    lua_register(g_pLuaState, "getEditorLineText",     luaCBGetEditorLineText);

    lua_register(g_pLuaState, "setSearchText",         luaCBSetSearchText);
    lua_register(g_pLuaState, "getSearchText",         luaCBGetSearchText);
    lua_register(g_pLuaState, "findNext",              luaCBFindNext);
    lua_register(g_pLuaState, "findPrevious",          luaCBFindPrevious);

    lua_register(g_pLuaState, "getSelectedText",       luaCBGetEditorSelectedText);
    lua_register(g_pLuaState, "getSExpr",              luaCBGetEditorSExpr);
    lua_register(g_pLuaState, "gotoLine",              luaCBGotoEditorLine);
    lua_register(g_pLuaState, "getLineNumber",         luaCBGetEditorLine);

    lua_register(g_pLuaState, "selectLines",           luaCBSelectLines);
    lua_register(g_pLuaState, "readFile",              luaCBReadFileToString);

    lua_register(g_pLuaState, "drawText",              luaCBDrawText2D);

    lua_register(g_pLuaState, "drawText2D",             luaCBDrawText2D);
    lua_register(g_pLuaState, "drawText3D",             luaCBDrawText3D);
    lua_register(g_pLuaState, "drawText3DStroked",      luaCBDrawText3DStroked);

    lua_register(g_pLuaState, "glUseOffscreenContext",  luaCBUseOffscreenContext);
    lua_register(g_pLuaState, "glUseMainWindowContext", luaCBUseMainWindowContext);

    lua_register(g_pLuaState, "glMatrixModeProjection", luaCBGLMatrixModeProjection);
    lua_register(g_pLuaState, "glMatrixModeModelView",  luaCBGLMatrixModeModelView);
    lua_register(g_pLuaState, "glPushMatrix",           luaCBGLPushMatrix);
    lua_register(g_pLuaState, "glPopMatrix",            luaCBGLPopMatrix);
    lua_register(g_pLuaState, "glScale",                luaCBGLScale);
    lua_register(g_pLuaState, "glTranslate",            luaCBGLTranslate);
    lua_register(g_pLuaState, "glRotate",               luaCBGLRotate);
    lua_register(g_pLuaState, "glApplyTransform",       luaCBGLApplyTransform);
    lua_register(g_pLuaState, "glLoadIdentity",         luaCBGLLoadIdentity);
    lua_register(g_pLuaState, "glPerspective",          luaCBGLPerspective);
    lua_register(g_pLuaState, "glOrtho",                luaCBGLOrtho);
    lua_register(g_pLuaState, "glViewport",             luaCBGLViewport);
    lua_register(g_pLuaState, "glClear",                luaCBGLClear);

    lua_register(g_pLuaState, "glBuildStencil",         luaCBGLBuildStencil);
    lua_register(g_pLuaState, "glDrawWithinStencil",    luaCBGLDrawWithinStencil);
    lua_register(g_pLuaState, "glRemoveStencil",        luaCBGLRemoveStencil);
    lua_register(g_pLuaState, "glShowStencil",          luaCBGLShowStencil);
    lua_register(g_pLuaState, "glResetStencil",         luaCBGLResetStencil);

    lua_register(g_pLuaState, "edGetTopPosition",       luaCBEdGetTopPosition);
    lua_register(g_pLuaState, "edGetBottomPosition",    luaCBEdGetBottomPosition);
    lua_register(g_pLuaState, "edGetLeftPosition",      luaCBEdGetLeftPosition);
    lua_register(g_pLuaState, "edGetPosition",          luaCBEdGetPosition);
    lua_register(g_pLuaState, "edSetPosition",          luaCBEdSetPosition);
    lua_register(g_pLuaState, "edGetAnchor",            luaCBEdGetHighlightAnchor);
    lua_register(g_pLuaState, "edSetAnchor",            luaCBEdSetHighlightAnchor);
    lua_register(g_pLuaState, "edInsertNewline",        luaCBEdInsertNewline);

    lua_register(g_pLuaState, "edGetRenderMode",        luaCBEdGetRenderMode);
    lua_register(g_pLuaState, "edSetRenderMode",        luaCBEdSetRenderMode);

    lua_register(g_pLuaState, "edGetFlashRate",         luaCBEdGetFlashRate);
    lua_register(g_pLuaState, "edSetFlashRate",         luaCBEdSetFlashRate);

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

    lua_register(g_pLuaState, "setLispTraceVerbosity",  luaCBSetLispTraceVerbosity);
    lua_register(g_pLuaState, "getLispTraceVerbosity",  luaCBGetLispTraceVerbosity);

    lua_register(g_pLuaState, "getCurrentDir",          luaCBGetCurrentDir);
    lua_register(g_pLuaState, "setCurrentDir",          luaCBSetCurrentDir);

    lua_register(g_pLuaState, "qtQuit",                 luaCBQtQuit);

    lua_register(g_pLuaState, "glutWireSphere",         luaCBGlutWireSphere);
    lua_register(g_pLuaState, "glutWireCube",           luaCBGlutWireCube);

    lua_register(g_pLuaState, "glutSolidSphere",        luaCBGlutSolidSphere);
    lua_register(g_pLuaState, "glutSolidCube",          luaCBGlutSolidCube);

    lua_register(g_pLuaState, "svrStart",               luaCBStartServer);
    lua_register(g_pLuaState, "svrAccept",              luaCBAcceptConnection);
    lua_register(g_pLuaState, "svrReceive",             luaCBReceiveData);
    lua_register(g_pLuaState, "svrSend",                luaCBSendData);
    lua_register(g_pLuaState, "svrShutdown",            luaCBShutdownServer);

    const struct luaL_Reg lua_texturelib [] = {
        {"new",                   luaCBTextureNew},
        {"clear",                 luaCBTextureClear},
        {"select",                luaCBTextureSelect},
        {"update",                luaCBTextureUpdate},
        {"setRenderFn",           luaCBTextureSetRenderFunction},
        {"beginDrawing",          luaCBTextureBegin},
        {"endDrawing",            luaCBTextureEnd},
        {NULL, NULL}
    };

    const struct luaL_Reg lua_transformlib [] = {
        {"new",               luaCBTransformNew},
        {"identity",          luaCBTransformIdentity},
        {"camera",            luaCBTransformCamera},
        {"cameraBase",        luaCBTransformCameraBase},
        {"getTranslation",    luaCBTransformGetTranslation},
        {"setTranslation",    luaCBTransformSetTranslation},
        {"applyTranslation",  luaCBTransformApplyTranslation},
        {"getScale",          luaCBTransformGetScale},
        {"setScale",          luaCBTransformSetScale},
        {"applyScale",        luaCBTransformApplyScale},
        {"forward",           luaCBTransformForward},
        {"up",                luaCBTransformUp},
        {"side",              luaCBTransformSide},
        {"lookAt",            luaCBTransformLookAt},
        {"transform",         luaCBTransformTransform},
        {"translate",         luaCBTransformApplyTranslation},
        {"scale",             luaCBTransformApplyScale},
        {"rotate",            luaCBTransformRotate},
        {"localToGlobal",     luaCBTransformLocalToGlobal},
        {"globalToLocal",     luaCBTransformGlobalToLocal},
        {"copy",              luaCBTransformCopyFrom},
        {NULL, NULL}
    };

    // Need a get camera transform function.
    // Then, can lock objects in front of the camera.

    // I was thinking this is once again "wasting time on the engine instead of the app"
    // but I want to do this.


    luaL_register(g_pLuaState, "texture",   lua_texturelib);
    luaL_register(g_pLuaState, "transform", lua_transformlib);

//    lua_newtable(g_pLuaState);
//    luaL_setfuncs (g_pLuaState,lua_texturelib,0);
//    lua_pushvalue(g_pLuaState,-1);
//    lua_setglobal(g_pLuaState,"texture");

//    lua_newtable(g_pLuaState);
//    luaL_setfuncs (g_pLuaState,lua_transformlib,0);
//    lua_pushvalue(g_pLuaState,-1);
//    lua_setglobal(g_pLuaState,"transform");

    // Clear the stack with a lua_pop call here
    lua_pop(g_pLuaState, 2);

    luaL_newmetatable(g_pLuaState, "LiveCode.texture");
    lua_pushstring(g_pLuaState, "__gc");
    lua_pushcfunction(g_pLuaState, luaCBTextureGC);
    lua_settable(g_pLuaState, -3);

    luaL_newmetatable(g_pLuaState, "LiveCode.transform");

    // Clear the stack with a lua_pop call here
    lua_pop(g_pLuaState, 2);
}
