
#include "forthInterface.h"

ficlVm * g_ficlVm = 0;
ficlSystem * g_ficlSystem = 0;
ficlDictionary * g_ficlDictionary = 0;

std::string g_ficlTextOut;
std::string g_ficlErrorOut;

void ficlTextOutCapture(ficlCallback *callback, char *text);
void ficlErrorOutCapture(ficlCallback *callback, char *text);

void forthInit_ficl()
{
    g_ficlSystem = ficlSystemCreate(NULL);
    ficlSystemCompileExtras(g_ficlSystem);

    g_ficlVm = ficlSystemCreateVm(g_ficlSystem);
    g_ficlDictionary = ficlSystemGetDictionary(g_ficlSystem);

    g_ficlVm->callback.textOut = ficlTextOutCapture;
    g_ficlVm->callback.errorOut = ficlErrorOutCapture;
}

bool forthCall_ficl(std::string sCmd)
{
    g_ficlTextOut = "";
    g_ficlErrorOut = "";

    ficlVmEvaluate(g_ficlVm, const_cast<char *>(sCmd.c_str()));

    return true;
}

void forthClose_ficl()
{
    ficlSystemDestroy(g_ficlSystem);

    g_ficlVm = 0;
    g_ficlSystem = 0;
    g_ficlDictionary = 0;

    //ficlSystemGlobal = 0;
}

std::string & forthGetError_ficl()
{
    return g_ficlErrorOut;
}

std::string & forthGetOutput_ficl()
{
    return g_ficlTextOut;
}

void forthClearError_ficl()
{
    g_ficlErrorOut = "";
}

void forthClearOutput_ficl()
{
    g_ficlTextOut = "";
}

std::string forthGetState_ficl()
{
    if(g_ficlVm)
    {
        if(g_ficlVm->state == FICL_VM_STATE_INTERPRET)
            return "interpret";
        else if(g_ficlVm->state == FICL_VM_STATE_COMPILE)
            return "compiling";
        else
            return "invalid";
    }
    else
    {
        return "VM not initialized";
    }
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

/////////////////////////
// pForth

std::string g_sOut;
std::string g_sIn;
int g_nInPos = 0;


extern "C"
{

#include "pforth/pf_all.h"

int  sdTerminalOut( char c )
{
    //printf("%c", c);
    g_sOut = g_sOut + c;
    return c;
}

int  sdTerminalEcho( char c )
{
    //printf("%c", c);
    g_sOut = g_sOut + c;
    return c;
}

int  sdTerminalIn( void )
{
    if(g_nInPos >= g_sIn.length())
        return '\n';

    if(g_nInPos < 0)
        return '\n';

    char c = g_sIn[g_nInPos];
    g_nInPos++;
    if(g_nInPos >= g_sIn.length())
        g_nInPos = -1;

    return c;
}

int  sdQueryTerminal( void )
{
    return g_nInPos < g_sIn.length();
}

int  sdTerminalFlush( void )
{
    //return fflush(PF_STDOUT);
    return true;
}

void sdTerminalInit( void )
{
}

void sdTerminalTerm( void )
{
}

}

PForthTask g_cftd = 0;
PForthDictionary g_dic = 0;
ExecToken g_entryPoint = 0;

#define DEFAULT_RETURN_DEPTH (512)
#define DEFAULT_USER_DEPTH (512)
#define DEFAULT_HEADER_SIZE (120000)
#define DEFAULT_CODE_SIZE (300000)

void forthInit()
{
    pfInit();
    g_cftd = pfCreateTask( DEFAULT_USER_DEPTH, DEFAULT_RETURN_DEPTH );
    pfSetCurrentTask( g_cftd );
    //g_dic = pfLoadDictionary( "pforth.dic", &g_entryPoint );
    g_dic = pfBuildDictionary( DEFAULT_HEADER_SIZE, DEFAULT_CODE_SIZE );
}

bool forthCall(std::string sCmd)
{
    g_nInPos = 0;
    g_sIn = sCmd;
    g_sOut = "";

    if(ffRefill() > 0)
    {
      cell_t exception = ffInterpret();
      if( exception == 0 )
      {
        // ffInterpret ran the forth code no problem
        // ffOK now checks the stack.
        exception = ffOK();
      }

      // This will print out an error message, if any.
      switch( exception )
      {
      case 0:
        return true;
        break;

      case THROW_BYE:
        g_sOut = g_sOut + "ffInterpret exception: bye\n";
        return false;
        break;

      case THROW_ABORT:
      default:
        ffDotS();
        pfReportThrow( exception );
        pfHandleIncludeError();
        pfResetForthTask();
        return false;
        break;
      }
    }
    else
    {
      g_sOut = g_sOut + "An exception occured in ffRefill\n";
      return false;
    }

    //return g_sOut;
    return true;
}

std::string & forthGetError()
{
    return g_ficlErrorOut;
}

std::string & forthGetOutput()
{
    return g_sOut;
}

void forthClearError()
{

}

void forthClearOutput()
{
    g_sOut = "";
}

std::string forthGetState()
{
    return "NA";
}

void forthClose()
{

}
