
#include "forthInterface.h"

ficlVm * g_ficlVm = 0;
ficlSystem * g_ficlSystem = 0;
ficlDictionary * g_ficlDictionary = 0;

std::string g_ficlTextOut;
std::string g_ficlErrorOut;

void ficlTextOutCapture(ficlCallback *callback, char *text);
void ficlErrorOutCapture(ficlCallback *callback, char *text);

void forthInit()
{
    g_ficlSystem = ficlSystemCreate(NULL);
    ficlSystemCompileExtras(g_ficlSystem);

    g_ficlVm = ficlSystemCreateVm(g_ficlSystem);
    g_ficlDictionary = ficlSystemGetDictionary(g_ficlSystem);

    g_ficlVm->callback.textOut = ficlTextOutCapture;
    g_ficlVm->callback.errorOut = ficlErrorOutCapture;
}

bool forthCall(std::string sCmd)
{
    g_ficlTextOut = "";
    g_ficlErrorOut = "";

    ficlVmEvaluate(g_ficlVm, const_cast<char *>(sCmd.c_str()));

    return true;
}

void forthClose()
{
    ficlSystemDestroy(g_ficlSystem);

    g_ficlVm = 0;
    g_ficlSystem = 0;
    g_ficlDictionary = 0;

    //ficlSystemGlobal = 0;
}

std::string & forthGetError()
{
    return g_ficlErrorOut;
}

std::string & forthGetOutput()
{
    return g_ficlTextOut;
}

void forthClearError()
{
    g_ficlErrorOut = "";
}

void forthClearOutput()
{
    g_ficlTextOut = "";
}


// FICL output capturing

void ficlTextOutCapture(ficlCallback *callback, char *text)
{
    g_ficlTextOut += std::string(text);
}

void ficlErrorOutCapture(ficlCallback *callback, char *text)
{
    g_ficlErrorOut += std::string(text);
}
