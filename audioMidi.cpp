/*
You must link with winmm.lib. If using Visual C++, go to Build->Settings. Flip to the Link page,
and add winmm.lib to the library/object modules.

This app plays a musical phrase from the song "Twinkle, Twinkle Little Star". It sends the MIDI
events to the default MIDI Out device. It uses a simple console delay to time the length of each
note (ie, the time inbetween the MIDI Note-On and Note-Off events). Obviously, a Windowed app can't
use such a technique, and should instead use a MultiMedia timer.
*/

#include <windows.h>
#include <stdio.h>
#include <mmsystem.h>





/* Here's an array of MIDI Note-On and Note-Off events to play our musical phrase. Each event
consists of 4 bytes. The first byte is how many "PPQN clocks" to delay before outputting the
event. The second byte is the MIDI Status. The third byte is the MIDI Note Number. The fourth
byte is the MIDI note velocity. To mark the end of the array, we place a dummy MIDI event (whose
status is 0x00).

By setting our time byte in terms of "PPQN clocks", we can vary the tempo. For example, here we
assume that each clock is equal to 10 milliseconds. But for a slower tempo, we can say that each
clock is equal to 20 milliseconds.
*/

#define PPQN_TEMPO_FACTOR 10

unsigned char Phrase[] = {0, 0x90, 60, 127,
50, 0x90, 60, 00,
0, 0x90, 60, 127,
50, 0x90, 60, 00,
0, 0x90, 67, 127,
50, 0x90, 67, 00,
0, 0x90, 67, 127,
50, 0x90, 67, 00,
0, 0x90, 69, 127,
50, 0x90, 69, 00,
0, 0x90, 69, 127,
50, 0x90, 69, 00,
0, 0x90, 67, 127,
90, 0x90, 67, 00,
10, 0x90, 65, 127,
50, 0x90, 65, 00,
0, 0x90, 65, 127,
50, 0x90, 65, 00,
0, 0x90, 64, 127,
50, 0x90, 64, 00,
0, 0x90, 64, 127,
50, 0x90, 64, 00,
0, 0x90, 62, 127,
50, 0x90, 62, 00,
0, 0x90, 62, 127,
50, 0x90, 62, 00,
0, 0x90, 60, 127,
100, 0x90, 60, 00,
0, 0x00};


// minmax merely clamps a value within a range.
// call the damn function "clamp".


/*********************** PrintMidiOutErrorMsg() **************************
 * Retrieves and displays an error message for the passed MIDI Out error
 * number. It does this using midiOutGetErrorText().
 *************************************************************************/

void PrintMidiOutErrorMsg(unsigned long err)
{
#define BUFFERSIZE 120
	char	buffer[BUFFERSIZE];
	
	if (!(err = midiOutGetErrorText(err, &buffer[0], BUFFERSIZE)))
	{
		printf("%s\r\n", &buffer[0]);
	}
	else if (err == MMSYSERR_BADERRNUM)
	{
		printf("Strange error number returned!\r\n");
	}
	else
	{
		printf("Specified pointer is invalid!\r\n");
	}
}

/* ************************************************************************ */

int PlayMidiTest()
{
	HMIDIOUT					handle;
	unsigned char *				ptr;
	unsigned long				err;
	union {
		DWORD	dwData;
		UCHAR	bData[4];
	} u;

	/* Open whichever MIDI Out device he has specified in Control Panel's "Multimedia"*/
	if (!(err = midiOutOpen(&handle, (UINT)-1, 0, 0, CALLBACK_NULL)))
	{
		/* Get pointer to first event */
		ptr = &Phrase[0];

		/* Play the phrase. Note that we always assume at least one event, so I
		put the while test at the end of the loop */
		do
		{
		    /* Insert a delay here according the time byte. Factor in the tempo. */
			if (*ptr) Sleep(*ptr * PPQN_TEMPO_FACTOR);
			ptr++;
 
			/* Construct the MIDI message */
			u.bData[0] = (UCHAR)*(ptr)++;	/* MIDI status byte */
			u.bData[1] = (UCHAR)*(ptr)++;	/* first MIDI data byte */
			u.bData[2] = (UCHAR)*(ptr)++;	/* second MIDI data byte */
			u.bData[3] = 0;

			/* Output the note event */
			if ((err = midiOutShortMsg(handle, u.dwData)))
			{
				PrintMidiOutErrorMsg(err);
			}

			/* Another note? */
		} while (*(ptr + 1));

	     /* Close the MIDI device */
		 midiOutClose(handle);
	}
	else
	{
		printf("Error opening the default MIDI Out device!\r\n");
		PrintMidiOutErrorMsg(err);
	}

	return(0);
}

HMIDIOUT g_midihandle;

DWORD MidiOutMessage (HMIDIOUT hMidi, int iStatus, int iChannel,
                      int iData1,  int iData2)
{
     DWORD dwMessage ;

     dwMessage = iStatus | iChannel | (iData1 << 8) | (iData2 << 16) ;

     return midiOutShortMsg (hMidi, dwMessage) ;
}

void SelectMidiInstrument(unsigned char nInstrument)
{
    MidiOutMessage (g_midihandle, 0x0C0, 0, nInstrument, 0) ;
}

#define minmax(a,x,b) (min (max (x, a), b))
#define TIMER_RES   5
UINT     uTimerRes;


void StartMidi()
{
    TIMECAPS tc ;

    unsigned long err = midiOutOpen(&g_midihandle, (UINT)-1, 0, 0, CALLBACK_NULL);

    // Test instrument selection
    //MidiOutMessage (g_midihandle, 0x0C0, 0, 8, 0) ;

    timeGetDevCaps (&tc, sizeof (TIMECAPS)) ;
    uTimerRes = minmax (tc.wPeriodMin, TIMER_RES, tc.wPeriodMax) ;
    timeBeginPeriod (uTimerRes) ;

    if (err)
    {
        printf("Error opening the default MIDI Out device!\r\n");
        PrintMidiOutErrorMsg(err);
    }
}

void StopMidi()
{
    timeEndPeriod (uTimerRes) ;

    /* Close the MIDI device */
    midiOutClose(g_midihandle);
}

void MidiNoteOn(unsigned char nNote, unsigned char nVolume)
{
    union {
        DWORD	dwData;
        UCHAR	bData[4];
    } u;

    u.bData[0] = 0x90;
    u.bData[1] = nNote;
    u.bData[2] = nVolume;
    u.bData[3] = 0;

    unsigned long err = midiOutShortMsg(g_midihandle, u.dwData);

    if (err)
        PrintMidiOutErrorMsg(err);
}

void MidiNoteOff(unsigned char nNote)
{
    union {
        DWORD	dwData;
        UCHAR	bData[4];
    } u;

    u.bData[0] = 0x90;
    u.bData[1] = nNote;
    u.bData[2] = 0;
    u.bData[3] = 0;

    unsigned long err = midiOutShortMsg(g_midihandle, u.dwData);

    if (err)
        PrintMidiOutErrorMsg(err);
}
