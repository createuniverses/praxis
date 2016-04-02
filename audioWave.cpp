
#include <stdio.h>
#include <stdlib.h>
#include <iostream>


/* extern const int SAMPLE_RATE          = 44100; */
extern const int SAMPLE_RATE          = 8000;
extern const int SAMPLES_PER_REQUEST  = 1024;
extern const int LIVE_BUFFER_SIZE     = 8000;

#define PI               3.141592653589793238462643383279
#define TWOPI            6.283185307179586476925286766559

short *      pLiveBuffer                = 0;
int          nLiveBufferReadPosition    = 0;
int          nLiveBufferWritePosition   = 1;
int          nLiveBufferTotalReads      = 0;
int          nLiveBufferTotalWrites     = 0;

void MakeLiveBuffer()
{
    pLiveBuffer = (short *)malloc (LIVE_BUFFER_SIZE * sizeof(short)) ;
    nLiveBufferReadPosition = 0;

    for (int i = 0 ; i < LIVE_BUFFER_SIZE ; i++)
    {
        pLiveBuffer [i] = 0;
    }
}

short ReadLiveBufferSample()
{
    if (pLiveBuffer==0) return 0;

    short nSample = pLiveBuffer[nLiveBufferReadPosition];

    if(nLiveBufferReadPosition == nLiveBufferWritePosition)
    {
        // std::cout << "Read position stalled at write position: " << nLiveBufferReadPosition << "\n";
        return nSample;
    }

    nLiveBufferReadPosition = nLiveBufferReadPosition + 1;
    if(nLiveBufferReadPosition >= LIVE_BUFFER_SIZE)
        nLiveBufferReadPosition = 0;

    nLiveBufferTotalReads++;

    return nSample;
}

void WriteLiveBufferSample(short nSample)
{
    if (pLiveBuffer==0) return;

    pLiveBuffer[nLiveBufferWritePosition] = nSample;

    nLiveBufferWritePosition = nLiveBufferWritePosition + 1;
    if(nLiveBufferWritePosition >= LIVE_BUFFER_SIZE)
        nLiveBufferWritePosition = 0;

    nLiveBufferTotalWrites++;

    if(nLiveBufferReadPosition == nLiveBufferWritePosition)
    {
        std::cout << "Write position arrived at read position: " << nLiveBufferReadPosition << "\n";
    }
}

void FillBufferFromLiveBuffer (short * pBuffer, int nLength = SAMPLES_PER_REQUEST)
{
    for (int i = 0 ; i < nLength ; i++)
    {
        pBuffer [i] = ReadLiveBufferSample();
    }
}

#ifdef __PRAXIS_LINUX__

#include "SDL.h"

#include <string.h>

bool g_bSDLAudioStarted = false;
bool g_bSDLAudioPlaying = false;

static void SDLAudioCallback(void *userdata, Uint8 *stream, int len)
{
#if 1
    if (g_bSDLAudioPlaying)
    {
        FillBufferFromLiveBuffer((short*)stream, len/2);
#if 0
        unsigned int l = len/2;
        float fbuf[l];
        memset(fbuf, 0, sizeof(fbuf));
        SynthSample(l, fbuf, NULL);
        while (l--)
        {
            float f = fbuf[l];
            if (f < -1.0) f = -1.0;
            if (f > 1.0) f = 1.0;
            ((Sint16*)stream)[l] = (Sint16)(f * 32767);
        }
#endif
    }
    else memset(stream, 0, len);
#else
    memset(stream, 0, len);
#endif
}

void StartPlayingSound()
{
    if(!g_bSDLAudioStarted)
    {
        MakeLiveBuffer();

        SDL_AudioSpec des;
        des.freq = SAMPLE_RATE;
        des.format = AUDIO_S16SYS;
        des.channels = 1;
        des.samples = 512;
        des.callback = SDLAudioCallback;
        des.userdata = NULL;
        SDL_OpenAudio(&des, NULL);
        g_bSDLAudioStarted = true;
    }

    SDL_PauseAudio(0);
    g_bSDLAudioPlaying = true;
}

void StopPlayingSound()
{
    SDL_PauseAudio(1);
    g_bSDLAudioPlaying = false;
}

#endif

#ifdef __PRAXIS_WINDOWS__
/*
You must link with winmm.lib. If using Visual C++, go to Build->Settings. Flip to the Link page,
and add winmm.lib to the library/object modules.

This app shows how to use the Low Level Wave API to play the WAVE file C:\Windows\Chord.wav.
It also uses the MMIO (RIFF) File Parsing functions.
*/

#include <windows.h>
#include <stdio.h>
#include <conio.h>
#include <mmsystem.h>
#include <malloc.h>
#include <math.h>

HWAVEOUT     hWaveOut    = NULL;
short *      pBuffer1    = NULL;
short *      pBuffer2    = NULL;
PWAVEHDR     pWaveHdr1   = NULL;
PWAVEHDR     pWaveHdr2   = NULL;
WAVEFORMATEX waveformat;

extern "C" {

void oglcbDoMMWOMDONE(DWORD dwParam1)
{
    FillBufferFromLiveBuffer( ((short *)((PWAVEHDR) dwParam1)->lpData )) ;
    waveOutWrite (hWaveOut, (PWAVEHDR) dwParam1, sizeof (WAVEHDR)) ;
}

extern HWND g_AppHWND;

}

void StartPlayingSound()
{
    if (hWaveOut == NULL)
    {
        std::cout << "StartPlayingSound initialising and playing audio\n";

        MMRESULT err;

        MakeLiveBuffer();

        // Allocate memory for 2 headers and 2 buffers
        pWaveHdr1 = (PWAVEHDR)malloc (sizeof (WAVEHDR));
        pWaveHdr2 = (PWAVEHDR)malloc (sizeof (WAVEHDR));
        pBuffer1  = (short *)malloc (sizeof(short) * SAMPLES_PER_REQUEST);
        pBuffer2  = (short *)malloc (sizeof(short) * SAMPLES_PER_REQUEST);

        if (!pWaveHdr1 || !pWaveHdr2 || !pBuffer1 || !pBuffer2)
        {
            if (!pWaveHdr1) free (pWaveHdr1) ;
            if (!pWaveHdr2) free (pWaveHdr2) ;
            if (!pBuffer1)  free (pBuffer1) ;
            if (!pBuffer2)  free (pBuffer2) ;

            std::cout << "Error allocating memory!\n";
            return;
        }

        waveformat.wFormatTag      = WAVE_FORMAT_PCM ;
        waveformat.nChannels       = 1 ;
        waveformat.nSamplesPerSec  = SAMPLE_RATE ;
        waveformat.nAvgBytesPerSec = SAMPLE_RATE * 2 ;
        waveformat.nBlockAlign     = 2 ;
        waveformat.wBitsPerSample  = 16 ;
        waveformat.cbSize          = 0 ;


        if (err = waveOutOpen (&hWaveOut, WAVE_MAPPER, &waveformat,
                               (DWORD) g_AppHWND, 0, CALLBACK_WINDOW))
        {
            free (pWaveHdr1) ;
            free (pWaveHdr2) ;
            free (pBuffer1) ;
            free (pBuffer2) ;

            hWaveOut = NULL ;

            std::cout << "Error opening waveform audio device! Error: " << err << "\n";
            return;
        }

        // Set up headers and prepare them

        pWaveHdr1->lpData          = (LPSTR)pBuffer1 ;
        pWaveHdr1->dwBufferLength  = SAMPLES_PER_REQUEST * 2 ;
        pWaveHdr1->dwBytesRecorded = 0 ;
        pWaveHdr1->dwUser          = 0 ;
        pWaveHdr1->dwFlags         = 0 ;
        pWaveHdr1->dwLoops         = 1 ;
        pWaveHdr1->lpNext          = NULL ;
        pWaveHdr1->reserved        = 0 ;

        err = waveOutPrepareHeader (hWaveOut, pWaveHdr1, sizeof (WAVEHDR)) ;

        std::cout << "waveOutPrepareHeader 1 returns " << err << ", (MMSYSERR_INVALHANDLE = " << MMSYSERR_INVALHANDLE << ")\n";

        pWaveHdr2->lpData          = (LPSTR)pBuffer2 ;
        pWaveHdr2->dwBufferLength  = SAMPLES_PER_REQUEST * 2 ;
        pWaveHdr2->dwBytesRecorded = 0 ;
        pWaveHdr2->dwUser          = 0 ;
        pWaveHdr2->dwFlags         = 0 ;
        pWaveHdr2->dwLoops         = 1 ;
        pWaveHdr2->lpNext          = NULL ;
        pWaveHdr2->reserved        = 0 ;

        waveOutPrepareHeader (hWaveOut, pWaveHdr2, sizeof (WAVEHDR)) ;

        std::cout << "waveOutPrepareHeader 2 returns " << err << ", (MMSYSERR_INVALHANDLE = " << MMSYSERR_INVALHANDLE << ")\n";

        FillBufferFromLiveBuffer (pBuffer1) ;
        err = waveOutWrite (hWaveOut, pWaveHdr1, sizeof (WAVEHDR)) ;

        std::cout << "Initial waveOutWrite result 1: " << err << ", (WAVERR_UNPREPARED = " << WAVERR_UNPREPARED << ")\n";

        FillBufferFromLiveBuffer (pBuffer2) ;
        err = waveOutWrite (hWaveOut, pWaveHdr2, sizeof (WAVEHDR)) ;

        std::cout << "Initial waveOutWrite result 2: " << err << ", (WAVERR_UNPREPARED = " << WAVERR_UNPREPARED << ")\n";

        std::cout << "StartPlayingSound completed.\n";
    }
    else
    {
        std::cout << "StartPlayingSound resuming.\n";
        waveOutRestart(hWaveOut);
    }
}

void StopPlayingSound()
{
    std::cout << "StopPlayingSound\n";

    waveOutPause(hWaveOut);
}
#endif
