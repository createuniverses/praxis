
#include "ioCallbacks.h"

#include "io/IoState.h"
#include "io/IoObject.h"

#include "io/IoNumber.h"

#include "World.h"

extern IoState * g_pIoState;

IO_METHOD(IoObject, drawLine)
{
    int argcount = IoMessage_argCount(m);

    if(argcount != 6)
    {
        IoState_error_(g_pIoState, m, "6 numerical arguments expected for drawLine");
        return IONIL(self);
    }

    for(int i = 0; i < 6; i++)
    {
        IoObject * v = IoMessage_locals_valueArgAt_(m, locals, i);
        if(!ISNUMBER(v))
        {
            IoState_error_(g_pIoState, m, "Argument %d not a number", i);
            return IONIL(self);
        }
    }

    float x1 = IoNumber_asFloat(IoMessage_locals_valueArgAt_(m, locals, 0));
    float y1 = IoNumber_asFloat(IoMessage_locals_valueArgAt_(m, locals, 1));
    float z1 = IoNumber_asFloat(IoMessage_locals_valueArgAt_(m, locals, 2));
    float x2 = IoNumber_asFloat(IoMessage_locals_valueArgAt_(m, locals, 3));
    float y2 = IoNumber_asFloat(IoMessage_locals_valueArgAt_(m, locals, 4));
    float z2 = IoNumber_asFloat(IoMessage_locals_valueArgAt_(m, locals, 5));

    glBegin(GL_LINES);

    glVertex3f(x1,y1,z1);
    glVertex3f(x2,y2,z2);

    glEnd();

    return IONIL(self);
}

void ioInitCallbacks()
{
    IoMethodTable methodTable[] = {
        {"drawLine", IoObject_drawLine},
        {NULL, NULL}};

    IoObject *self = IoState_protoWithId_(g_pIoState, "Object");
    IoObject_addTaglessMethodTable_(self, methodTable);
}

