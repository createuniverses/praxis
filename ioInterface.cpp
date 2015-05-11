
#include "ioInterface.h"

#include "io/IoState.h"
#include "io/IoObject.h"

#include "io/IoNumber.h"

IoState * g_pIoState = 0;

std::string g_sIoTrace;
std::string g_sIoReply;

void IoPraxis_printCallback(void *, const UArray *self)
{
    if(self->encoding == CENCODING_ASCII || self->encoding == CENCODING_UTF8)
    {
        g_sIoTrace += (char *)(self->data);
    }
}

void ioInit()
{
    g_pIoState = IoState_new();

    IoState_doCString_(g_pIoState, "render := method()");
    IoState_printCallback_(g_pIoState, IoPraxis_printCallback);
}

void ioCall(std::string sCmd)
{
    IoState_doCString_(g_pIoState, sCmd.c_str());
}

void ioCallWithReply(std::string sCmd)
{
    g_sIoTrace = "";
    g_sIoReply = "No reply";

    IoObject  * v = IoState_doCString_(g_pIoState, sCmd.c_str());
    IoMessage * m = IoMessage_newWithName_(g_pIoState, IoState_symbolWithCString_(g_pIoState, "asString"));
    IoObject  * s = IoMessage_locals_performOn_(m, IoState_lobby(g_pIoState), v);

    if(ISSEQ(s))
        g_sIoReply = IoSeq_asCString(s);
}

void ioClose()
{
    IoState_free(g_pIoState);
}

std::string & ioGetReply()
{
    return g_sIoReply;
}

std::string & ioGetTrace()
{
    return g_sIoTrace;
}
