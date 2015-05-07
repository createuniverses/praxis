
#include "ioInterface.h"

#include "io/IoState.h"
#include "io/IoObject.h"

#include "io/IoNumber.h"

IoState * g_pIoState = 0;

std::string g_sIoOutput;

void IoPraxis_UArray_print(const UArray *self)
{
    char buf[1024];
    char * p = buf;
    sprintf(p, "");

    if(self->encoding == CENCODING_ASCII || self->encoding == CENCODING_UTF8)
    {
        p += sprintf(p, "%s", self->data);
    }
    else if(self->encoding != CENCODING_NUMBER)
    {
        UARRAY_FOREACH(self, i, v, p += sprintf(p, "%c", (int)v); );
    }
    else if(UArray_isFloatType(self))
    {
        p += sprintf(p, "[");
        UARRAY_FOREACH(self, i, v,
                    p += sprintf(p, "%f", (float)v);
                    if(i != self->size - 1) p += sprintf(p, ", ");
                    );
        p += sprintf(p, "]");
    }
    else
    {
        p += sprintf(p, "[");
        UARRAY_FOREACH(self, i, v,
                    p += sprintf(p, "%i", (int)v);
                    if(i != self->size - 1) p += sprintf(p, ", ");
                    );
        p += sprintf(p, "]");
    }

    g_sIoOutput += buf;
}

void IoPraxis_printCallback(void *voidSelf, const UArray *ba)
{
    voidSelf;

    IoPraxis_UArray_print(ba);
}

void ioInit()
{
    g_pIoState = IoState_new();

    IoState_doCString_(g_pIoState, "render := method()");
    IoState_printCallback_(g_pIoState, IoPraxis_printCallback);
}

std::string ioCall(std::string sCmd)
{
    char buf[1024];
    g_sIoOutput = "";

    IoObject * v = IoState_doCString_(g_pIoState, sCmd.c_str());
    if (ISNUMBER(v))
    {
        sprintf(buf, "%f", IoNumber_asFloat(v));
    }
    else if(ISSEQ(v))
    {
        sprintf(buf, "%s", IoSeq_asCString(v));
    }
    else
    {
        sprintf(buf, "");
    }

    return std::string("==>") + std::string(buf) + std::string("\n") + g_sIoOutput;
}

void ioClose()
{
    IoState_free(g_pIoState);
}
