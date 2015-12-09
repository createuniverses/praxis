
#include "lispCallbacks.h"

extern s7_scheme * g_pLisp;

static s7_pointer s7cbDrawLine(s7_scheme *sc, s7_pointer args)
{
    return(s7_make_integer(sc, 0));
}

void lispInitCallbacks()
{
    s7_define_function(g_pLisp, "draw-line", s7cbDrawLine, 6, 0, false, "");
}
