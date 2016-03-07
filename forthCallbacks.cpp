
#include "forthCallbacks.h"

extern "C"
{
#include "pforth/pf_all.h"
}

#include "World.h"

#include "SingleWorldConfiguration.h"

#include <sstream>

#include <glm/vec3.hpp> // glm::vec3
#include <glm/vec4.hpp> // glm::vec4
#include <glm/mat4x4.hpp> // glm::mat4
#include <glm/gtc/matrix_transform.hpp> // glm::translate, glm::rotate, glm::scale, glm::perspective

static void pfcbTest()
{
    PF_FLOAT n1 = POP_FLOAT_STACK;
    PF_FLOAT n2 = POP_FLOAT_STACK;

    PUSH_FLOAT_STACK(n1*n2);

    MSG("praxis PForth C Test!");
}

static void pfcbDrawLine()
{
    PF_FLOAT x1 = POP_FLOAT_STACK;
    PF_FLOAT y1 = POP_FLOAT_STACK;
    PF_FLOAT z1 = POP_FLOAT_STACK;

    PF_FLOAT x2 = POP_FLOAT_STACK;
    PF_FLOAT y2 = POP_FLOAT_STACK;
    PF_FLOAT z2 = POP_FLOAT_STACK;

    glBegin(GL_LINES);

    glVertex3f(x1,y1,z1);
    glVertex3f(x2,y2,z2);

    glEnd();
}

//////////////////////////////////////////////////////////////////////////////////////////

void forthInitCallbacks()
{
    void praxisDefinePForthCFunction(const char * name, CFunc0 func);

    praxisDefinePForthCFunction( "ctest",    (CFunc0)pfcbTest     );
    praxisDefinePForthCFunction( "drawline", (CFunc0)pfcbDrawLine );
}
