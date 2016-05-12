
#include "forthInterface.h"

std::string g_sOut;
std::string g_sIn;
int g_nInPos = 0;


extern "C"
{

#include "pforth/pf_all.h"

extern CFunc0 CustomFunctionTable[];

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

std::string g_sForthError;

std::string & forthGetError()
{
    return g_sForthError;
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

void praxisDefinePForthCFunction(const char * name, CFunc0 func)
{
    static int i = 0;
    if(i<PF_MAX_CUSTOM_FUNCTIONS)
    {
        CustomFunctionTable[i] = func;
        CreateGlueToC( name, i, C_RETURNS_VOID, 0 );
        i++;
        printf("Defined PForth C function %s, index is now %d\n", name, i);
    }
    else
    {
        printf("Maximum number of custom PForth C functions reached.\n");
        printf("Increase PF_MAX_CUSTOM_FUNCTIONS.\n");
    }
}
