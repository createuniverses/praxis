
// Author:  Greg "fugue" Santucci, 2015
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/

#include "luaCB.h"

#include <sstream>

#include <glm/vec3.hpp> // glm::vec3
#include <glm/vec4.hpp> // glm::vec4
#include <glm/mat4x4.hpp> // glm::mat4
#include <glm/gtc/matrix_transform.hpp> // glm::translate, glm::rotate, glm::scale, glm::perspective


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

int luaCBBeginGL(lua_State * L)
{
    int mode = luaL_checknumber(L, 1);
    glBegin(mode);
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
        slices = luaL_checknumber(L, 2);
    if(lua_gettop(L) > 2)
        stacks = luaL_checknumber(L, 3);
    glutWireSphere(radius, slices, stacks);
    return 0;
}

int luaCBGlutSolidSphere(lua_State * L)
{
    float radius = luaL_checknumber(L, 1);
    float slices = 15;
    float stacks = 15;
    if(lua_gettop(L) > 1)
        slices = luaL_checknumber(L, 2);
    if(lua_gettop(L) > 2)
        stacks = luaL_checknumber(L, 3);
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

#if 0
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
#endif

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

namespace DrawArraysTest
{
    struct vertex
    {
        GLfloat x, y, z;
    };

    vertex vertices[3];
}

int luaCBDrawArraysTest(lua_State * L)
{
    using namespace DrawArraysTest;

    glEnableClientState(GL_VERTEX_ARRAY);

    // Assign some values to all 3 points
    vertices[0].x = 10.0f;
    vertices[0].y = 5.0f;
    vertices[0].z = 7.0f;

    // Vertex 2
    vertices[1].x = -10.0f;
    vertices[1].y = 3.0f;
    vertices[1].z = 1.0f;

    // Vertex 3
    vertices[2].x = 5.0f;
    vertices[2].y = -5.0f;
    vertices[2].z = 2.0f;

    int num_indices = 3;

    glVertexPointer( num_indices,      // number of coordinates per vertex (x,y,z)
                     GL_FLOAT,         // they are floats
                     sizeof(vertex),   // stride
                     &vertices);       // the array pointer

    // Render primitives from array-based data
    glDrawArrays(GL_TRIANGLES, 0, num_indices);
    return 0;
}

int luaCBGLUseProgram(lua_State * L)
{
    GLuint program = luaL_checknumber(L, 1);
    glUseProgram(program);
    return 0;
}

int luaCBGLCreateProgram(lua_State * L)
{
    const char * vertex_shader_source   = luaL_checkstring(L, 1);
    const char * fragment_shader_source = luaL_checkstring(L, 2);

    std::string sMessages;

    GLuint vertex_shader;
    GLuint fragment_shader;
    GLuint program;

    // Create and compile vertex shader
    vertex_shader = glCreateShader(GL_VERTEX_SHADER);
    glShaderSource(vertex_shader, 1, &vertex_shader_source, NULL);
    glCompileShader(vertex_shader);
    {
        int success = 0;
        glGetShaderiv(vertex_shader, GL_COMPILE_STATUS, &success);
        if (!success)
        {
            const int MAX_INFO_LOG_SIZE = 2048;
            GLchar infoLog[MAX_INFO_LOG_SIZE];
            glGetShaderInfoLog(vertex_shader, MAX_INFO_LOG_SIZE, NULL, infoLog);
            std::string sError = std::string("Error in vertex shader compilation\n") +
                                 std::string(infoLog);
            lua_pushnumber(L, -1);
            lua_pushstring(L, sError.c_str());
            glDeleteShader(vertex_shader);
            return 2;
        }
        else
        {
            const int MAX_INFO_LOG_SIZE = 2048;
            GLchar infoLog[MAX_INFO_LOG_SIZE];
            glGetShaderInfoLog(vertex_shader, MAX_INFO_LOG_SIZE, NULL, infoLog);
            sMessages = sMessages +
                        std::string("Messages from vertex shader compiler\n") +
                        std::string(infoLog) + std::string("\n");
        }
    }

    // Create and compile fragment shader
    fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(fragment_shader, 1, &fragment_shader_source, NULL);
    glCompileShader(fragment_shader);
    {
        int success = 0;
        glGetShaderiv(fragment_shader, GL_COMPILE_STATUS, &success);
        if (!success)
        {
            const int MAX_INFO_LOG_SIZE = 2048;
            GLchar infoLog[MAX_INFO_LOG_SIZE];
            glGetShaderInfoLog(fragment_shader, MAX_INFO_LOG_SIZE, NULL, infoLog);
            std::string sError = std::string("Error in fragment shader compilation\n") +
                                 std::string(infoLog);
            lua_pushnumber(L, -1);
            lua_pushstring(L, sError.c_str());
            glDeleteShader(vertex_shader);
            glDeleteShader(fragment_shader);
            return 2;
        }
        else
        {
            const int MAX_INFO_LOG_SIZE = 2048;
            GLchar infoLog[MAX_INFO_LOG_SIZE];
            glGetShaderInfoLog(fragment_shader, MAX_INFO_LOG_SIZE, NULL, infoLog);
            sMessages = sMessages +
                        std::string("Messages from fragment shader compiler\n") +
                        std::string(infoLog) + std::string("\n");
        }
    }

    // Create program, attach shaders to it, and link it
    program = glCreateProgram();
    glAttachShader(program, vertex_shader);
    glAttachShader(program, fragment_shader);
    glLinkProgram(program);
    {
        int success = 0;
        glGetProgramiv(program, GL_LINK_STATUS, &success);
        if (!success)
        {
            const int MAX_INFO_LOG_SIZE = 2048;
            GLchar infoLog[MAX_INFO_LOG_SIZE];
            glGetProgramInfoLog(program, MAX_INFO_LOG_SIZE, NULL, infoLog);
            std::string sError = std::string("Error in program linkage\n") +
                                 std::string(infoLog);
            lua_pushnumber(L, -1);
            lua_pushstring(L, sError.c_str());
            glDeleteShader(vertex_shader);
            glDeleteShader(fragment_shader);
            glDeleteProgram(program);
            return 2;
        }
        else
        {
            const int MAX_INFO_LOG_SIZE = 2048;
            GLchar infoLog[MAX_INFO_LOG_SIZE];
            glGetShaderInfoLog(program, MAX_INFO_LOG_SIZE, NULL, infoLog);
            sMessages = sMessages +
                        std::string("Messages from linker\n") +
                        std::string(infoLog) + std::string("\n");
        }
    }

    // Delete the shaders as the program has them now
    glDeleteShader(vertex_shader);
    glDeleteShader(fragment_shader);

    lua_pushnumber(L, program);
    lua_pushstring(L, sMessages.c_str());
    return 2;
}

int luaCBGLGenFramebuffers(lua_State * L)
{
    GLuint fboId = 0;
    glGenFramebuffersEXT(1, &fboId);
    lua_pushnumber(L, fboId);
    return 1;
}

int luaCBGLBindFramebuffer(lua_State * L)
{
    GLuint fboId = luaL_checknumber(L, 1);
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, fboId);
    return 0;
}

int luaCBGLGenRenderbuffers(lua_State * L)
{
    GLuint rboId = 0;
    glGenRenderbuffersEXT(1, &rboId);
    lua_pushnumber(L, rboId);
    return 1;
}

int luaCBGLBindRenderbuffer(lua_State * L)
{
    GLuint rboId = luaL_checknumber(L, 1);
    glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, rboId);
    //glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT, TEXTURE_WIDTH, TEXTURE_HEIGHT);
    return 0;
}

int luaCBGLBindTexture(lua_State * L)
{
    GLuint texId = luaL_checknumber(L, 1);
    glBindTexture(GL_TEXTURE_2D, texId);
    return 0;
}

int luaCBGLGenerateMipmap(lua_State * L)
{
    glGenerateMipmapEXT(GL_TEXTURE_2D);
    return 0;
}

static float Int2FloatCol(int c)
{
  float n = (float)c / 255.0f;
  if(n < 0.0f)
    n = 0.0f;
  if(n > 1.0f)
    n = 1.0f;
  return n;
}

int luaCBGLClearColor(lua_State * L)
{
    GLuint r = Int2FloatCol(luaL_checknumber(L, 1));
    GLuint g = Int2FloatCol(luaL_checknumber(L, 2));
    GLuint b = Int2FloatCol(luaL_checknumber(L, 3));
    GLuint a = Int2FloatCol(luaL_checknumber(L, 4));
    glClearColor(r, g, b, a);
    return 0;
}

int luaCBGLRenderbufferStorage(lua_State * L)
{
    GLuint nWidth  = luaL_checknumber(L, 1);
    GLuint nHeight = luaL_checknumber(L, 2);
    // glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT, TEXTURE_WIDTH, TEXTURE_HEIGHT);
    glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT, nWidth, nHeight);
    return 0;
}

int luaCBGLFramebufferTexture2D(lua_State * L)
{
    GLuint texId = luaL_checknumber(L, 1);
    // glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, textureId, 0);
    glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, texId, 0);
    return 0;
}

int luaCBGLFramebufferRenderbuffer(lua_State *L)
{
    GLuint rboId = luaL_checknumber(L, 1);
    // glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, rboId);
    glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, rboId);
    return 0;
}

int luaCBGLPrepareFBOTexture(lua_State * L)
{
    GLuint nWidth = 256;
    GLuint nHeight = 256;

    int n = lua_gettop(L);

    if(n<2) luaL_error(L, "At least 2 arguments required.");

    nWidth = luaL_checknumber(L, 1);
    nHeight = luaL_checknumber(L, 2);

    GLint filterparam = GL_NEAREST;
    GLint wrapparam = GL_CLAMP_TO_EDGE;
    //GLint wrapparam = GL_CLAMP;

    if(n >= 3)
    {
        filterparam = luaL_checknumber(L, 3);
        if(filterparam != GL_NEAREST &&
           filterparam != GL_LINEAR)
        {
            luaL_error(L, "Only GL_NEAREST or GL_LINEAR for arg 3");
        }
    }

    if(n >= 4)
    {
        wrapparam = luaL_checknumber(L, 4);
        if(wrapparam != GL_CLAMP_TO_EDGE &&
           wrapparam != GL_REPEAT &&
           wrapparam != GL_CLAMP)
        {
            luaL_error(L, "Only GL_CLAMP_TO_EDGE, GL_REPEAT or GL_CLAMP for arg 4");
        }
    }

    GLuint textureId = 0;

    glGenTextures(1, &textureId);
    glBindTexture(GL_TEXTURE_2D, textureId);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, filterparam);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, filterparam);

    //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, wrapparam);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, wrapparam);

    //glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE); // automatic mipmap generation included in OpenGL v1.4

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA32F_ARB, nWidth, nHeight, 0, GL_RGBA, GL_FLOAT, 0);

    glClampColorARB(GL_CLAMP_VERTEX_COLOR_ARB, GL_FALSE);
    glClampColorARB(GL_CLAMP_FRAGMENT_COLOR_ARB, GL_FALSE);

    glBindTexture(GL_TEXTURE_2D, 0);

    lua_pushnumber(L, textureId);
    return 1;
}

char * g_pStringTex = 0;
int luaCBGLStringToTexture(lua_State * L)
{
    // Split this up into Lua callable functions
    // This is so I can define how the string texture gets put together in Lua.

    const char * sText = luaL_checkstring(L, 1);
    int len = strlen(sText);

    if(len <= 0)
        return 0;

    const int stringtexsize = 512*512*16;

    GLint internalformat = GL_RGBA32F_ARB;
    GLenum format = GL_RGBA;
    GLenum type = GL_FLOAT;

    int nargs = lua_gettop(L);
    if(nargs>1) internalformat = luaL_checknumber(L, 2);
    if(nargs>2)         format = luaL_checknumber(L, 3);
    if(nargs>3)           type = luaL_checknumber(L, 4);

    int nWidth  = 512;
    int nHeight = 512;

    if(g_pStringTex == 0)
        g_pStringTex = new char[stringtexsize];

    memset(g_pStringTex, 0, stringtexsize);

#if 1
    int j = 0;
    for(int i = 0; i < stringtexsize; i++)
    {
        g_pStringTex[i] = sText[j%len];
        j++;

        if(g_pStringTex[i] == ' ')
            g_pStringTex[i] = 0x0;
    }
#endif

#if 0
    for(int line = 0; line < 10; line++)
    {
        int lineStart = line*512*16;
        for(int i = 0; i < len; i++)
        {
            g_pStringTex[i+lineStart] = sText[i];
            //if(i % 4 == 2)
            //    g_pStringTex[i+lineStart] = 0x00;
        }

        //for(int i=len; i < len+16; i++)
        //    g_pStringTex[i+lineStart] = 0x20;
    }
#endif

    GLuint textureId = 0;

    glGenTextures(1, &textureId);
    glBindTexture(GL_TEXTURE_2D, textureId);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    //glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA32F_ARB, nWidth, nHeight, 0, GL_RGBA, GL_FLOAT, g_pStringTex);
    //glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA32UI_EXT, nWidth, nHeight, 0, GL_RGBA, GL_UNSIGNED_INT, g_pStringTex);
    //glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA32I_EXT, nWidth, nHeight, 0, GL_RGBA, GL_INT, g_pStringTex);

    glTexImage2D(GL_TEXTURE_2D, 0, internalformat, nWidth, nHeight, 0, format, type, g_pStringTex);

    GLenum e = glGetError();

    glClampColorARB(GL_CLAMP_READ_COLOR_ARB, GL_FALSE);

    glClampColorARB(GL_CLAMP_VERTEX_COLOR_ARB, GL_FALSE);
    glClampColorARB(GL_CLAMP_FRAGMENT_COLOR_ARB, GL_FALSE);

    glBindTexture(GL_TEXTURE_2D, 0);

    memset(g_pStringTex, 0, stringtexsize);

    glBindTexture(GL_TEXTURE_2D, textureId);
    glGetTexImage(GL_TEXTURE_2D, 0, format, type, g_pStringTex);
    glBindTexture(GL_TEXTURE_2D, 0);

    lua_pushnumber(L, textureId);
    lua_pushnumber(L, e);
    return 2;
}

// Split into separate functions to facilitate experiments
// Seeing the effect of different patterns of bytes


int luaCBGLSetStringToTextureBuffer(lua_State * L)
{
    return 0;
}

int luaCBGLGetStringToTextureBuffer(lua_State * L)
{
    return 0;
}

int luaCBGLSetTexParams(lua_State * L)
{
    int n = lua_gettop(L);

    GLint filterparam = GL_NEAREST;
    //GLint wrapparam = GL_CLAMP_TO_EDGE;
    GLint wrapparam = GL_CLAMP;

    if(n >= 1)
    {
        filterparam = luaL_checknumber(L, 1);
        if(filterparam != GL_NEAREST &&
           filterparam != GL_LINEAR)
        {
            luaL_error(L, "Only GL_NEAREST or GL_LINEAR for arg 3");
        }
    }

    if(n >= 2)
    {
        wrapparam = luaL_checknumber(L, 2);
        if(wrapparam != GL_CLAMP_TO_EDGE &&
           wrapparam != GL_REPEAT &&
           wrapparam != GL_CLAMP)
        {
            luaL_error(L, "Only GL_CLAMP_TO_EDGE, GL_REPEAT or GL_CLAMP for arg 4");
        }
    }

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, filterparam);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, filterparam);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, wrapparam);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, wrapparam);

    return 0;
}

int luaCBGLGetTextureAccessLimits(lua_State * L)
{
    GLint maxTexInstructions = 0;
    GLint maxTexIndirections = 0;

    glGetProgramivARB(GL_FRAGMENT_PROGRAM_ARB, GL_MAX_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB, &maxTexInstructions);
    glGetProgramivARB(GL_FRAGMENT_PROGRAM_ARB, GL_MAX_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB, &maxTexIndirections);

    lua_pushnumber(L, maxTexInstructions);
    lua_pushnumber(L, maxTexIndirections);

    return 2;
}

int luaCBGLEnable(lua_State * L)
{
    GLenum cap = (GLenum)luaL_checknumber(L, 1);
    glEnable(cap);
    return 0;
}

int luaCBGLDisable(lua_State * L)
{
    GLenum cap = (GLenum)luaL_checknumber(L, 1);
    glDisable(cap);
    return 0;
}

int luaCBGLUniformf(lua_State * L)
{
    int n = lua_gettop(L);

    if(n<2) luaL_error(L, "At least 2 arguments required.");

    GLuint id = luaL_checknumber(L, 1);
    GLfloat params[4];

    switch(n)
    {
    case 2:
        params[0] = luaL_checknumber(L, 2);
        glUniform1f(id, params[0]);
        break;
    case 3:
        params[0] = luaL_checknumber(L, 2);
        params[1] = luaL_checknumber(L, 3);
        glUniform2f(id, params[0], params[1]);
        break;
    case 4:
        params[0] = luaL_checknumber(L, 2);
        params[1] = luaL_checknumber(L, 3);
        params[2] = luaL_checknumber(L, 4);
        glUniform3f(id, params[0], params[1], params[3]);
        break;
    case 5:
        params[0] = luaL_checknumber(L, 2);
        params[1] = luaL_checknumber(L, 3);
        params[2] = luaL_checknumber(L, 4);
        params[3] = luaL_checknumber(L, 5);
        glUniform4f(id, params[0], params[1], params[2], params[3]);
        break;
    default:
        luaL_error(L, "id followed by 1 to 4 arguments expected.");
        break;
    }

    return 0;
}

int luaCBGLUniformi(lua_State * L)
{
    int n = lua_gettop(L);

    if(n<2) luaL_error(L, "At least 2 arguments required.");

    GLuint id = luaL_checknumber(L, 1);
    GLuint params[4];

    switch(n)
    {
    case 2:
        params[0] = luaL_checknumber(L, 2);
        glUniform1i(id, params[0]);
        break;
    case 3:
        params[0] = luaL_checknumber(L, 2);
        params[1] = luaL_checknumber(L, 3);
        glUniform2i(id, params[0], params[1]);
        break;
    case 4:
        params[0] = luaL_checknumber(L, 2);
        params[1] = luaL_checknumber(L, 3);
        params[2] = luaL_checknumber(L, 4);
        glUniform3i(id, params[0], params[1], params[3]);
        break;
    case 5:
        params[0] = luaL_checknumber(L, 2);
        params[1] = luaL_checknumber(L, 3);
        params[2] = luaL_checknumber(L, 4);
        params[3] = luaL_checknumber(L, 5);
        glUniform4i(id, params[0], params[1], params[2], params[3]);
        break;
    default:
        luaL_error(L, "id followed by 1 to 4 arguments expected.");
        break;
    }

    return 0;
}

int luaCBGLGetUniformLocation(lua_State * L)
{
    int n = lua_gettop(L);

    if(n!=2) luaL_error(L, "Prog id and uniform name required.");

    GLuint       prog = luaL_checknumber(L, 1);
    const char * name = luaL_checkstring(L, 2);

    GLuint loc = glGetUniformLocation(prog, name);

    lua_pushnumber(L, loc);

    return 1;
}

int luaCBGLGetError(lua_State * L)
{
    /* #define GL_NO_ERROR 0
     * #define GL_INVALID_ENUM 0x0500
     * #define GL_INVALID_VALUE 0x0501
     * #define GL_INVALID_OPERATION 0x0502
     * #define GL_STACK_OVERFLOW 0x0503
     * #define GL_STACK_UNDERFLOW 0x0504
     * #define GL_OUT_OF_MEMORY 0x0505
     */

    GLenum e = glGetError();

    switch(e)
    {
    case GL_NO_ERROR:
        lua_pushstring(L, "GL_NO_ERROR");
        break;
    case GL_INVALID_ENUM:
        lua_pushstring(L, "GL_INVALID_ENUM");
        break;
    case GL_INVALID_VALUE:
        lua_pushstring(L, "GL_INVALID_VALUE");
        break;
    case GL_INVALID_OPERATION:
        lua_pushstring(L, "GL_INVALID_OPERATION");
        break;
    case GL_INVALID_FRAMEBUFFER_OPERATION_EXT:
        lua_pushstring(L, "GL_INVALID_FRAMEBUFFER_OPERATION");
        break;
    case GL_OUT_OF_MEMORY:
        lua_pushstring(L, "GL_OUT_OF_MEMORY");
        break;
    case GL_STACK_UNDERFLOW:
        lua_pushstring(L, "GL_STACK_UNDERFLOW");
        break;
    case GL_STACK_OVERFLOW:
        lua_pushstring(L, "GL_STACK_OVERFLOW");
        break;

    default:
        lua_pushstring(L, "Unknown");
        break;
    }

    lua_pushnumber(L, e);

    return 2;
}

int luaCBGLActiveTexture(lua_State * L)
{
    GLuint tnum = luaL_checknumber(L, 1);
    glActiveTexture(GL_TEXTURE0 + tnum);
    return 0;
}

void luaInitCallbacksOpenGL()
{
    stringstream ss;
    ss << "GL_PROJECTION = " << GL_PROJECTION << "\n";
    ss << "GL_MODELVIEW = " << GL_MODELVIEW << "\n";

    ss << "GL_TRIANGLES = " << GL_TRIANGLES << "\n";
    ss << "GL_QUADS = " << GL_QUADS << "\n";
    ss << "GL_LINES = " << GL_LINES << "\n";

    ss << "GL_LIGHTING = " << GL_LIGHTING << "\n";
    ss << "GL_LIGHT0 = " << GL_LIGHT0 << "\n";
    ss << "GL_LIGHT1 = " << GL_LIGHT1 << "\n";
    ss << "GL_TEXTURE_2D = " << GL_TEXTURE_2D << "\n";
    ss << "GL_POLYGON_OFFSET_FILL = " << GL_POLYGON_OFFSET_FILL << "\n";

    ss << "GL_LINEAR = " << GL_LINEAR << "\n";
    ss << "GL_NEAREST = " << GL_NEAREST << "\n";
    ss << "GL_CLAMP_TO_EDGE = " << GL_CLAMP_TO_EDGE << "\n";
    ss << "GL_REPEAT = " << GL_REPEAT << "\n";

    ss << "GL_BLEND = " << GL_BLEND << "\n";

    ss << "GL_RGBA32F_ARB = " << GL_RGBA32F_ARB << "\n";
    ss << "GL_RGBA32UI_EXT = " << GL_RGBA32UI_EXT << "\n";
    ss << "GL_RGBA32I_EXT = " << GL_RGBA32I_EXT << "\n";

    ss << "GL_FLOAT = " << GL_FLOAT << "\n";

    ss << "GL_INT = " << GL_INT << "\n";
    ss << "GL_UNSIGNED_INT = " << GL_UNSIGNED_INT << "\n";

    ss << "GL_BYTE = " << GL_BYTE << "\n";
    ss << "GL_UNSIGNED_BYTE = " << GL_UNSIGNED_BYTE << "\n";

    ss << "GL_4_BYTES = " << GL_4_BYTES << "\n";

    ss << "GL_RGBA = " << GL_RGBA << "\n";

    ss << "GL_RGBA8UI_EXT = " << GL_RGBA8UI_EXT << "\n";
    ss << "GL_RGBA_INTEGER_EXT = " << GL_RGBA_INTEGER_EXT << "\n";

    luaCall(ss.str());

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
    lua_register(g_pLuaState, "beginQuadGL",           luaCBBeginQuadsGL);
    lua_register(g_pLuaState, "beginLinGL",            luaCBBeginLinesGL);
    lua_register(g_pLuaState, "endGL",                 luaCBEndGL);

    lua_register(g_pLuaState, "vectorGL",              luaCBVectorGL);
    lua_register(g_pLuaState, "texGL",                 luaCBTexGL);
    lua_register(g_pLuaState, "colorGL",               luaCBColorGL);
    lua_register(g_pLuaState, "getColorGL",            luaCBGetColorGL);
    lua_register(g_pLuaState, "lightGL",               luaCBSetLightPosGL);
    lua_register(g_pLuaState, "normalGL",              luaCBNormalGL);

    lua_register(g_pLuaState, "glBeginTriangles",      luaCBBeginTrianglesGL);
    lua_register(g_pLuaState, "glBeginQuads",          luaCBBeginQuadsGL);
    lua_register(g_pLuaState, "glBeginLines",          luaCBBeginLinesGL);

    lua_register(g_pLuaState, "glBegin",               luaCBBeginGL);
    lua_register(g_pLuaState, "glEnd",                 luaCBEndGL);

    lua_register(g_pLuaState, "glVertex",              luaCBVectorGL);
    lua_register(g_pLuaState, "glTexCoord",            luaCBTexGL);
    lua_register(g_pLuaState, "glColor",               luaCBColorGL);
    lua_register(g_pLuaState, "glLight",               luaCBSetLightPosGL);
    lua_register(g_pLuaState, "glNormal",              luaCBNormalGL);

    lua_register(g_pLuaState, "glGetColor",            luaCBGetColorGL);

    lua_register(g_pLuaState, "enableLighting",        luaCBEnableLighting);
    lua_register(g_pLuaState, "disableLighting",       luaCBDisableLighting);
    lua_register(g_pLuaState, "enableTexturing",       luaCBEnableTexturing);
    lua_register(g_pLuaState, "disableTexturing",      luaCBDisableTexturing);

    lua_register(g_pLuaState, "enablePolygonOffset",   luaCBEnablePolygonOffset);
    lua_register(g_pLuaState, "disablePolygonOffset",  luaCBDisablePolygonOffset);
    lua_register(g_pLuaState, "setPolygonOffset",      luaCBSetPolygonOffset);

    lua_register(g_pLuaState, "drawCube",              luaCBDrawCube);

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

    lua_register(g_pLuaState, "glutWireSphere",         luaCBGlutWireSphere);
    lua_register(g_pLuaState, "glutWireCube",           luaCBGlutWireCube);

    lua_register(g_pLuaState, "glutSolidSphere",        luaCBGlutSolidSphere);
    lua_register(g_pLuaState, "glutSolidCube",          luaCBGlutSolidCube);

    lua_register(g_pLuaState, "glutStrokeString",       luaCBGlutStrokeString);
    lua_register(g_pLuaState, "glutBitmapString",       luaCBGlutBitmapString);

    lua_register(g_pLuaState, "drawText",               luaCBDrawText2D);
    lua_register(g_pLuaState, "drawText2D",             luaCBDrawText2D);
    lua_register(g_pLuaState, "drawText3D",             luaCBDrawText3D);
    lua_register(g_pLuaState, "drawText3DStroked",      luaCBDrawText3DStroked);

//    lua_register(g_pLuaState, "glCreateArray",          luaCBGLCreateArray);
//    lua_register(g_pLuaState, "glWriteToArray",         luaCBGLWriteToBuffer);
//    lua_register(g_pLuaState, "glDrawArray",            luaCBDrawArray);

    lua_register(g_pLuaState, "glDATest",               luaCBDrawArraysTest);

    lua_register(g_pLuaState, "glCreateProgram",        luaCBGLCreateProgram);
    lua_register(g_pLuaState, "glUseProgram",           luaCBGLUseProgram);

    lua_register(g_pLuaState, "glGenFramebuffers",      luaCBGLGenFramebuffers);
    lua_register(g_pLuaState, "glBindFramebuffer",      luaCBGLBindFramebuffer);
    lua_register(g_pLuaState, "glGenRenderbuffers",     luaCBGLGenRenderbuffers);
    lua_register(g_pLuaState, "glBindRenderbuffer",     luaCBGLBindRenderbuffer);

    lua_register(g_pLuaState, "glBindTexture",          luaCBGLBindTexture);
    lua_register(g_pLuaState, "glGenerateMipmap",       luaCBGLGenerateMipmap);

    lua_register(g_pLuaState, "glClearColor",           luaCBGLClearColor);

    lua_register(g_pLuaState, "glRenderbufferStorage",      luaCBGLRenderbufferStorage);
    lua_register(g_pLuaState, "glFramebufferTexture2D",     luaCBGLFramebufferTexture2D);
    lua_register(g_pLuaState, "glFramebufferRenderbuffer",  luaCBGLFramebufferRenderbuffer);

    lua_register(g_pLuaState, "glPrepareFBOTexture",        luaCBGLPrepareFBOTexture);

    lua_register(g_pLuaState, "glSetTexParams",             luaCBGLSetTexParams);

    // This should be enough for shadertoy, I may add the vector and matrix versions of these
    // later if needed.
    lua_register(g_pLuaState, "glUniformf",              luaCBGLUniformf);
    lua_register(g_pLuaState, "glUniformi",              luaCBGLUniformi);

    lua_register(g_pLuaState, "glGetUniformLocation",    luaCBGLGetUniformLocation);

    lua_register(g_pLuaState, "glGetError",              luaCBGLGetError);

    lua_register(g_pLuaState, "glActiveTexture",         luaCBGLActiveTexture);

    lua_register(g_pLuaState, "glGetTextureAccessLimits",  luaCBGLGetTextureAccessLimits);

    lua_register(g_pLuaState, "glEnable",                luaCBGLEnable);
    lua_register(g_pLuaState, "glDisable",               luaCBGLDisable);

    lua_register(g_pLuaState, "glStringToTexture",       luaCBGLStringToTexture);

    //lua_register(g_pLuaState, "glMakeStringTexture",     )
}
