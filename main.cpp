#include <stdlib.h>
#include <stdio.h>

#include <iostream>
#include <algorithm>

#include "SingleWorldConfiguration.h"

#include "World.h"

#include "luaInterface.h"
#include "luaCallbacks.h"

#include "lispInterface.h"
#include "lispCallbacks.h"

#include "forthInterface.h"
#include "forthCallbacks.h"

#include "AI_NeuralNetworkSystem.h"

#ifdef __PRAXIS_WINDOWS__
#include "fmod.h"
#include "fmod_errors.h"    /* optional */

CRITICAL_SECTION g_cs;
#endif

#ifdef __PRAXIS_LINUX__

#include <signal.h>

struct sigaction new_act, old_act;

void handle_sigint(int ignored)
{
    std::cout << "Praxis:SIGINT" << std::endl;

    sigaction(SIGINT, &new_act, NULL);
}

#include <unistd.h>
#include <pthread.h>

pthread_t g_inputthread;
pthread_mutex_t g_inputthreadmutex;

std::string g_sInputThreadString;
bool g_bInputThreadNewString = false;

void * inputthread_function( void *ptr )
{
    printf("\nHello from the input thread.\n\n");
    char mystring [65536];
    //std::string sCode;
    while(true)
    {
        int n = read(STDIN_FILENO, mystring, 65535);
        if ( n > 0 )
        {
            // n bytes read

            mystring[n] = '\0';
            //sCode = sCode + mystring;
            //puts(sCode.c_str());

            pthread_mutex_lock (&g_inputthreadmutex);
            g_sInputThreadString = mystring;
            g_bInputThreadNewString = true;
            pthread_mutex_unlock (&g_inputthreadmutex);
        }
    }
}

#endif

void MoveConsTest();

World *          g_pWorld;

#ifdef __PRAXIS_WINDOWS__
FSOUND_STREAM * g_pMp3Stream    = 0;
int             g_nMp3Channel   = 0;
#endif

int g_nLastBreakTime = 0;

#ifdef __PRAXIS_WINDOWS__
int CALLBACK WinMain( HINSTANCE hInstance,
                      HINSTANCE hPrevInstance,
                      LPSTR lpCmdLine,
                      int nCmdShow)
#else
int main()
#endif
{
#ifdef __PRAXIS_LINUX__
    sigaction(SIGINT, NULL, &old_act);
    if (old_act.sa_handler != SIG_IGN)
    {
        memset(&new_act, 0, sizeof(new_act));
        new_act.sa_handler = &handle_sigint;
        sigaction(SIGINT, &new_act, NULL);
    }

    pthread_mutex_init(&g_inputthreadmutex, NULL);
    int iret1 = pthread_create( &g_inputthread, NULL, inputthread_function, NULL);

    if(iret1)
    {
      fprintf(stderr,"Error - pthread_create() return code: %d\n",iret1);
    }

#endif

    MoveConsTest();

#ifdef __PRAXIS_WINDOWS__
    hInstance;hPrevInstance;lpCmdLine;nCmdShow;

    InitializeCriticalSection(&g_cs);
#endif

    aiNeuralNetworkSystem::Startup();

    // Lua
    luaInit();
    luaInitCallbacks();

    // Lisp
    lispInit();
    lispInitCallbacks();

    // Forth
    forthInit();
    forthInitCallbacks();

#ifdef __PRAXIS_WINDOWS__
    // FMOD mp3 playing
    // Can we have FMOD mp3 playing on top of synthesizing audio??
    if (!FSOUND_Init(44100, 32, FSOUND_INIT_USEDEFAULTMIDISYNTH))
    {
        //qDebug("FMOD: FSOUND_Init error!\n");
        //qDebug("%s\n", FMOD_ErrorString(FSOUND_GetError()));
    }
    else
    {
        g_pMp3Stream = FSOUND_Stream_Open("music.mp3", FSOUND_MPEGACCURATE, 0, 0);
        if (!g_pMp3Stream)
        {
            //qDebug("FMOD: FSOUND_Stream_Open error!\n");
            //qDebug("%s\n", FMOD_ErrorString(FSOUND_GetError()));
        }
    }
#endif

    World * pWorld = new World();
    g_pWorld = pWorld;

    int argc = 0;
    char ** argv = 0;

    RunSingleWorldConfiguration(pWorld, "praxis", argc, argv);

	delete pWorld;

    luaClose();
    lispClose();
    forthClose();

#ifdef __PRAXIS_WINDOWS__
    FSOUND_Stream_Close(g_pMp3Stream);
    FSOUND_Close();
#endif

    aiNeuralNetworkSystem::Shutdown();

#ifdef __PRAXIS_WINDOWS__
    DeleteCriticalSection (&g_cs) ;
#endif

#ifdef __PRAXIS_WINDOWS__
    return EXIT_SUCCESS;
#else
    return 0;
#endif
}

// MoveConsTest code

#include <iostream>
#include <algorithm>

class MemoryBlock
{
public:

   // Simple constructor that initializes the resource.
   explicit MemoryBlock(size_t length)
      : _length(length)
      , _data(new int[length])
   {
      std::cout << "In MemoryBlock(size_t). length = "
                << _length << "." << std::endl;
   }

   // Destructor.
   ~MemoryBlock()
   {
      std::cout << "In ~MemoryBlock(). length = "
                << _length << ".";

      if (_data != NULL)
      {
         std::cout << " Deleting resource.";
         // Delete the resource.
         delete[] _data;
      }

      std::cout << std::endl;
   }

   // Copy constructor.
   MemoryBlock(const MemoryBlock& other)
      : _length(other._length)
      , _data(new int[other._length])
   {
      std::cout << "In MemoryBlock(const MemoryBlock&). length = "
                << other._length << ". Copying resource." << std::endl;

      std::copy(other._data, other._data + _length, _data);
   }

   // Copy assignment operator.
   MemoryBlock& operator=(const MemoryBlock& other)
   {
      std::cout << "In operator=(const MemoryBlock&). length = "
                << other._length << ". Copying resource." << std::endl;

      if (this != &other)
      {
         // Free the existing resource.
         delete[] _data;

         _length = other._length;
         _data = new int[_length];
         std::copy(other._data, other._data + _length, _data);
      }
      return *this;
   }

   // Retrieves the length of the data resource.
   size_t Length() const
   {
      return _length;
   }

#if 1
// Move constructor.
MemoryBlock(MemoryBlock&& other)
   : _data(NULL)
   , _length(0)
{
   std::cout << "In MemoryBlock(MemoryBlock&&). length = "
             << other._length << ". Moving resource." << std::endl;

   // Copy the data pointer and its length from the
   // source object.
   _data = other._data;
   _length = other._length;

   // Release the data pointer from the source object so that
   // the destructor does not free the memory multiple times.
   other._data = NULL;
   other._length = 0;
}

// Move assignment operator.
MemoryBlock& operator=(MemoryBlock&& other)
{
   std::cout << "In operator=(MemoryBlock&&). length = "
             << other._length << "." << std::endl;

   if (this != &other)
   {
      // Free the existing resource.
      delete[] _data;

      // Copy the data pointer and its length from the
      // source object.
      _data = other._data;
      _length = other._length;

      // Release the data pointer from the source object so that
      // the destructor does not free the memory multiple times.
      other._data = NULL;
      other._length = 0;
   }
   return *this;
}
#endif

private:
   size_t _length; // The length of the resource.
   int* _data; // The resource.
};

#include <vector>

using namespace std;

void MoveConsTest()
{
   // Create a vector object and add a few elements to it.
   vector<MemoryBlock> v;
   v.push_back(MemoryBlock(25));
   v.push_back(MemoryBlock(75));

   // Insert a new element into the second position of the vector.
   v.insert(v.begin() + 1, MemoryBlock(50));
}
