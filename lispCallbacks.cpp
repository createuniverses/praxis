
#include "lispCallbacks.h"

#include "PraxisTexture.h"
#include "PraxisServer.h"
#include "PraxisLog.h"

#include "World.h"

#include "SingleWorldConfiguration.h"

extern s7_scheme * g_pLisp;

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

void lispInitCallbacks()
{
    s7_define_function(g_pLisp, "draw-line", s7cbDrawLine, 6, 0, false, "");
}
