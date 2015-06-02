
#include "ioCallbacks.h"

#include "io/IoState.h"
#include "io/IoObject.h"

#include "io/IoNumber.h"

#include "io/IoCFunction.h"

#include "World.h"
#include "PraxisServer.h"

extern IoState * g_pIoState;

static const char *protoId = "Praxis";

class IoPraxis
{
public:
    static IoObject* DrawLine(IoObject *self, IoObject *locals, IoMessage *m);
    static IoObject* SvrSend(IoObject *self, IoObject *locals, IoMessage *m);

    static IoObject* proto_tagless(IoState* state);

    static IoObject* proto(IoState* state);
    static IoObject* rawClone(IoObject* self);
    static void mark(IoObject* self);
    static void free(IoObject* self);
};

IoObject *IoPraxis::proto_tagless(IoState* state)
{
    IoMethodTable methods[] = {
        {"drawLine", DrawLine},
        {"svrSend", SvrSend},
        {NULL, NULL}};

    IoObject* self = IoObject_new(state);
    IoObject_setSlot_to_(self, IOSYMBOL("type"), IOSYMBOL(protoId));
    IoObject_setSlot_to_(state->core, IoState_symbolWithCString_(state, protoId), self);

    IoObject_addMethodTable_(self, methods);

    return self;
}

IoObject *IoPraxis::proto(IoState* state)
{
    IoMethodTable methods[] = {
        {"drawLine", DrawLine},
        {NULL, NULL}};

    IoObject* self = IoObject_new(state);

    IoTag* tag = IoTag_newWithName_(protoId);
    IoTag_state_(tag, state);
    IoTag_cloneFunc_(tag, (IoTagCloneFunc*) rawClone);
    IoTag_markFunc_(tag, (IoTagMarkFunc*) mark);
    IoTag_freeFunc_(tag, (IoTagFreeFunc*) free);
    IoObject_tag_(self, tag);

    IoState_registerProtoWithId_(state, self, protoId);
    IoObject_setSlot_to_(state->core, IoState_symbolWithCString_(state, protoId), self);

    IoObject_setDataPointer_(self, 0);
    IoObject_addMethodTable_(self, methods);

    return self;
}

IoObject *IoPraxis::rawClone(IoObject *self)
{
    IoObject *clone = IoObject_rawClonePrimitive(self);
    return clone;
}

void IoPraxis::mark(IoObject *)
{
}

void IoPraxis::free(IoObject *)
{
}

IoObject* IoPraxis::DrawLine(IoObject *self, IoObject *locals, IoMessage *m)
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

IoObject* IoPraxis::SvrSend(IoObject *self, IoObject *locals, IoMessage *m)
{
    int argcount = IoMessage_argCount(m);

    if(argcount != 2)
    {
        IoState_error_(g_pIoState, m, "2 numerical arguments expected for svrSend");
        return IONIL(self);
    }

    IoObject * socket = IoMessage_locals_valueArgAt_(m, locals, 0);
    if(!ISNUMBER(socket))
    {
        IoState_error_(g_pIoState, m, "Argument %d not a socket id", 0);
        return IONIL(self);
    }

    IoObject * data = IoMessage_locals_valueArgAt_(m, locals, 1);
    if(!ISSEQ(data))
    {
        IoState_error_(g_pIoState, m, "Argument %d not a string", 1);
        return IONIL(self);
    }

    SOCKET s = IoNumber_asInt(IoMessage_locals_valueArgAt_(m, locals, 0));
    std::string d = IoSeq_asCString(data);

    PraxisServer::Send(s, d);

    return IONIL(self);
}

void ioInitCallbacks()
{
    IoPraxis::proto_tagless(g_pIoState);
}

#if 0
void ioInitCallbacks()
{
    IoMethodTable methodTable[] = {
        {"drawLine", IoPraxis::DrawLine},
        {NULL, NULL}};

    IoObject *self = IoObject_getInstance(g_pIoState);
    IoObject_addTaglessMethodTable_(self, methodTable);
}
#endif
