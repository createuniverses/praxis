
#include "lispCallbacks.h"

#include "lispInterface.h"

#include "PraxisTexture.h"
#include "PraxisServer.h"
#include "PraxisLog.h"

#include "World.h"

#include "SingleWorldConfiguration.h"

#include <sstream>

extern s7_scheme * g_pLisp;

#include <glm/vec3.hpp> // glm::vec3
#include <glm/vec4.hpp> // glm::vec4
#include <glm/mat4x4.hpp> // glm::mat4
#include <glm/gtc/matrix_transform.hpp> // glm::translate, glm::rotate, glm::scale, glm::perspective

static int s7_type_tag_transform = 0;

static s7_pointer s7cbDrawLine(s7_scheme *sc, s7_pointer args)
{
    float x1 = s7_number_to_real(sc, s7_list_ref(sc, args, 0));
    float y1 = s7_number_to_real(sc, s7_list_ref(sc, args, 1));
    float z1 = s7_number_to_real(sc, s7_list_ref(sc, args, 2));
    float x2 = s7_number_to_real(sc, s7_list_ref(sc, args, 3));
    float y2 = s7_number_to_real(sc, s7_list_ref(sc, args, 4));
    float z2 = s7_number_to_real(sc, s7_list_ref(sc, args, 5));

    glBegin(GL_LINES);

    glVertex3f(x1,y1,z1);
    glVertex3f(x2,y2,z2);

    glEnd();

    return(s7_make_integer(sc, 0));
}

// Old OpenGL, adequate for low poly
static s7_pointer s7cbGLBegin(s7_scheme * sc, s7_pointer args)
{
    GLenum arg = s7_number_to_integer(sc, s7_list_ref(sc, args, 0));
    glBegin(arg);
    return s7_nil(sc);
}

static s7_pointer s7cbGLEnd(s7_scheme * sc, s7_pointer args)
{
    glEnd();
    return s7_nil(sc);
}

static s7_pointer s7cbGLVertex(s7_scheme * sc, s7_pointer args)
{
    GLfloat x,y,z;
    x = s7_number_to_real(sc, s7_list_ref(sc, args, 0));
    y = s7_number_to_real(sc, s7_list_ref(sc, args, 1));
    z = s7_number_to_real(sc, s7_list_ref(sc, args, 2));
    glVertex3f(x,y,z);
    return s7_nil(sc);
}

static s7_pointer s7cbGLColor(s7_scheme * sc, s7_pointer args)
{
    GLubyte r,g,b,a;
    r = s7_number_to_integer(sc, s7_list_ref(sc, args, 0));
    g = s7_number_to_integer(sc, s7_list_ref(sc, args, 1));
    b = s7_number_to_integer(sc, s7_list_ref(sc, args, 2));
    a = s7_number_to_integer(sc, s7_list_ref(sc, args, 3));
    glColor4ub(r,g,b,a);
    return s7_nil(sc);
}

static s7_pointer s7cbGLNormal(s7_scheme * sc, s7_pointer args)
{
    GLfloat x,y,z;
    x = s7_number_to_real(sc, s7_list_ref(sc, args, 0));
    y = s7_number_to_real(sc, s7_list_ref(sc, args, 1));
    z = s7_number_to_real(sc, s7_list_ref(sc, args, 2));
    glNormal3f(x,y,z);
    return s7_nil(sc);
}

static s7_pointer s7cbGLTexCoord(s7_scheme * sc, s7_pointer args)
{
    return s7_nil(sc);
}

static s7_pointer s7cbGLEnable(s7_scheme * sc, s7_pointer args)
{
    return s7_nil(sc);
}

static s7_pointer s7cbGLDisable(s7_scheme * sc, s7_pointer args)
{
    return s7_nil(sc);
}

static s7_pointer s7cbNewTransform(s7_scheme * sc, s7_pointer args)
{

    return s7_make_object(sc, s7_type_tag_transform, (void *)0);
}

void lispInitCallbacks()
{
    s7_type_tag_transform = s7_new_type("transform", NULL, NULL, NULL, NULL, NULL, NULL);

    stringstream ss;
    ss << "(define GL_PROJECTION " << GL_PROJECTION << ")\n";
    ss << "(define GL_MODELVIEW " << GL_MODELVIEW << ")\n";

    ss << "(define GL_TRIANGLES " << GL_TRIANGLES << ")\n";
    ss << "(define GL_QUADS " << GL_QUADS << ")\n";
    ss << "(define GL_LINES " << GL_LINES << ")\n";

    ss << "(define GL_LIGHTING " << GL_LIGHTING << ")\n";
    ss << "(define GL_LIGHT0 " << GL_LIGHT0 << ")\n";
    ss << "(define GL_LIGHT1 " << GL_LIGHT1 << ")\n";
    ss << "(define GL_TEXTURE_2D " << GL_TEXTURE_2D << ")\n";
    ss << "(define GL_POLYGON_OFFSET_FILL " << GL_POLYGON_OFFSET_FILL << ")\n";

    ss << "(define GL_LINEAR " << GL_LINEAR << ")\n";
    ss << "(define GL_NEAREST " << GL_NEAREST << ")\n";
    ss << "(define GL_CLAMP_TO_EDGE " << GL_CLAMP_TO_EDGE << ")\n";
    ss << "(define GL_REPEAT " << GL_REPEAT << ")\n";
    lispCall(ss.str());

    s7_define_function(g_pLisp, "draw-line", s7cbDrawLine,    6,      0, false, "");

    s7_define_function(g_pLisp, "glBegin",   s7cbGLBegin,     1,      0, false, "");
    s7_define_function(g_pLisp, "glEnd",     s7cbGLEnd,       0,      0, false, "");
    s7_define_function(g_pLisp, "glVertex",  s7cbGLVertex,    3,      0, false, "");
    s7_define_function(g_pLisp, "glColor",   s7cbGLColor,     4,      0, false, "");
    s7_define_function(g_pLisp, "glNormal",  s7cbGLNormal,    3,      0, false, "");
    // glTexCoord
    // glEnable
    // glDisable

    // Shader functions
    // FBO functions
}
