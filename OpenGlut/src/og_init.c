/*!
    \file  og_init.c
    \brief Initialization
*/

/*
 * Portions copyright (C) 2004, the OpenGLUT project contributors.
 * OpenGLUT branched from freeglut in February, 2004.
 *
 * Copyright (c) 1999-2000 Pawel W. Olszta. All Rights Reserved.
 * Written by Pawel W. Olszta, <olszta@sourceforge.net>
 * Creation date: Thu Dec 2 1999
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * PAWEL W. OLSZTA BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <GL/openglut.h>
#include "og_internal.h"

/*
 * TODO BEFORE THE STABLE RELEASE:
 *
 *  ogDeinitialize()        -- Win32's OK, X11 needs the OS-specific
 *                             deinitialization done (What's missing?)
 *
 * Wouldn't it be cool to use gettext() for error messages? I just love
 * bash saying  "nie znaleziono pliku" instead of "file not found" :)
 * Is gettext easily portable?
 */

/* -- GLOBAL VARIABLES ----------------------------------------------------- */

/*
 * A structure pointed by ogDisplay holds all information
 * regarding the display, screen, root window etc.
 */
SOG_Display ogDisplay;

/*
 * The settings for the current OpenGLUT session
 */
SOG_State ogState =
{
    { -1, -1, GL_FALSE },   /* Position */
    { 300, 300, GL_TRUE },  /* Size */
    GLUT_RGBA | GLUT_SINGLE | GLUT_DEPTH,  /* DisplayMode */
    GL_FALSE,               /* Initialised */
    GL_FALSE,               /* ForceDirectContext */
    GL_TRUE,                /* TryDirectContext */
    GL_FALSE,               /* ForceIconic */
    GL_FALSE,               /* UseCurrentContext */
    GL_FALSE,               /* GLDebugSwitch */
    GL_FALSE,               /* XSyncSwitch */
    GLUT_KEY_REPEAT_ON,     /* KeyRepeat */
    0xffffffff,             /* Modifiers */
    0,                      /* FPSInterval */
    0,                      /* SwapCount */
    0,                      /* SwapTime */
#if TARGET_HOST_WIN32 || TARGET_HOST_WINCE
    { 0, GL_FALSE },        /* Time                */
#else
    { { 0, 0 }, GL_FALSE }, /* Time                */
#endif
    { NULL, NULL },         /* Timers              */
    { NULL, NULL },         /* FreeTimers          */
    NULL,                   /* IdleCallback        */
    0,                      /* ActiveMenus         */
    NULL,                   /* MenuStateCallback   */
    NULL,                   /* MenuStatusCallback  */
    { 640, 480, GL_TRUE },  /* GameModeSize        */
    16,                     /* GameModeDepth       */
    72,                     /* GameModeRefresh     */
    GLUT_ACTION_EXIT,       /* ActionOnWindowClose */
    GLUT_EXEC_STATE_INIT,   /* ExecState           */
    NULL,                   /* ProgramName         */
    GL_FALSE,               /* JoysticksInitted    */
#if TARGET_HOST_UNIX_X11
    {0},                    /* A jmpbuf BackToMainLoop */
#elif TARGET_HOST_WIN32 || TARGET_HOST_WINCE
    0,                      /* A jmpbuf BackToMainLoop */
#endif
    0,                      /* InMainLoop flag     */
};


/* -- PRIVATE FUNCTIONS ---------------------------------------------------- */

/*
 * A call to this function should initialize all the display stuff...
 */
static void ogInitializeDisplay( const char *displayName )
{
#if TARGET_HOST_UNIX_X11
    ogDisplay.Display = XOpenDisplay( displayName );

    if( ogDisplay.Display == NULL )
        ogError( "Failed to open display '%s'.", XDisplayName( displayName ) );

    if( !glXQueryExtension( ogDisplay.Display, NULL, NULL ) )
        ogError( "OpenGL GLX extension not supported by display '%s'.",
            XDisplayName( displayName ) );

    ogDisplay.Screen = DefaultScreen( ogDisplay.Display );
    ogDisplay.RootWindow = RootWindow(
        ogDisplay.Display,
        ogDisplay.Screen
    );

    ogDisplay.ScreenWidth  = DisplayWidth(
        ogDisplay.Display,
        ogDisplay.Screen
    );
    ogDisplay.ScreenHeight = DisplayHeight(
        ogDisplay.Display,
        ogDisplay.Screen
    );

    ogDisplay.ScreenWidthMM = DisplayWidthMM(
        ogDisplay.Display,
        ogDisplay.Screen
    );
    ogDisplay.ScreenHeightMM = DisplayHeightMM(
        ogDisplay.Display,
        ogDisplay.Screen
    );

    ogDisplay.Connection = ConnectionNumber( ogDisplay.Display );

    /* Create the window deletion atom */
    ogDisplay.DeleteWindow = XInternAtom(
        ogDisplay.Display,
        "WM_DELETE_WINDOW",
        FALSE
    );

#elif TARGET_HOST_WIN32 || TARGET_HOST_WINCE

    WNDCLASS wc;
    ATOM atom;

    /* We need to initialize the ogDisplay global structure here. */
    ogDisplay.Instance = GetModuleHandle( NULL );

    atom = GetClassInfo( ogDisplay.Instance, _T( OPENGLUT_STRING ), &wc );
    if( !atom )
    {
        ZeroMemory( &wc, sizeof(WNDCLASS) );

        /*
         * Each of the windows should have its own device context, and we
         * want redraw events during Vertical and Horizontal Resizes by
         * the user.
         *
         * XXX Old code had "| CS_DBCLCKS" commented out.  Plans for the
         * XXX future?  Dead-end idea?
         */
#if TARGET_HOST_WIN32
        wc.style          = CS_OWNDC;
#endif
        wc.style         |= CS_HREDRAW | CS_VREDRAW;
        wc.lpfnWndProc    = ogWindowProc;
        wc.cbClsExtra     = 0;
        wc.cbWndExtra     = 0;
        wc.hInstance      = ogDisplay.Instance;
        wc.hIcon          = LoadIcon( ogDisplay.Instance, _T( "GLUT_ICON" ) );
        if( !wc.hIcon )
            wc.hIcon      = LoadIcon( NULL, IDI_WINLOGO );

        wc.hCursor        = LoadCursor( NULL, IDC_ARROW );
        wc.hbrBackground  = NULL;
        wc.lpszMenuName   = NULL;
        wc.lpszClassName  = _T( OPENGLUT_STRING );

        /* Register the window class */
        atom = RegisterClass( &wc );
        assert( atom );
    }

    /* The screen dimensions can be obtained via GetSystemMetrics() calls */
    ogDisplay.ScreenWidth  = GetSystemMetrics( SM_CXSCREEN );
    ogDisplay.ScreenHeight = GetSystemMetrics( SM_CYSCREEN );

    {
        HWND desktop = GetDesktopWindow( );
        HDC  context = GetDC( desktop );

        ogDisplay.ScreenWidthMM  = GetDeviceCaps( context, HORZSIZE );
        ogDisplay.ScreenHeightMM = GetDeviceCaps( context, VERTSIZE );

        ReleaseDC( desktop, context );
    }

#endif

    ogState.Initialised = GL_TRUE;
}

/*
 * Perform the OpenGLUT deinitialization...
 */
void ogDeinitialize( void )
{
    SOG_Timer *timer;

    if( !ogState.Initialised )
    {
        ogWarning(
            "ogDeinitialize(): "
            "No valid initialization has been performed."
        );
        return;
    }

    /* If there was a menu created, destroy the rendering context */
    if( ogStructure.MenuContext )
    {
        free( ogStructure.MenuContext );
        ogStructure.MenuContext = NULL;
    }

    ogDestroyStructure( );

    while( timer = ogState.Timers.First )
    {
        ogListRemove( &ogState.Timers, &timer->Node );
        free( timer );
    }

    while( timer = ogState.FreeTimers.First )
    {
        ogListRemove( &ogState.FreeTimers, &timer->Node );
        free( timer );
    }

    ogJoystickShutdown();

    ogState.Initialised = GL_FALSE;

    ogState.Position.X = -1;
    ogState.Position.Y = -1;
    ogState.Position.Use = GL_FALSE;

    ogState.Size.X = 300;
    ogState.Size.Y = 300;
    ogState.Size.Use = GL_TRUE;

    ogState.DisplayMode = GLUT_RGBA | GLUT_SINGLE | GLUT_DEPTH;

    ogState.ForceDirectContext  = GL_FALSE;
    ogState.TryDirectContext    = GL_TRUE;
    ogState.ForceIconic         = GL_FALSE;
    ogState.UseCurrentContext   = GL_FALSE;
    ogState.GLDebugSwitch       = GL_FALSE;
    ogState.XSyncSwitch         = GL_FALSE;
    ogState.ActionOnWindowClose = GLUT_ACTION_EXIT;
    ogState.ExecState           = GLUT_EXEC_STATE_INIT;

    ogState.KeyRepeat       = GLUT_KEY_REPEAT_ON;
    ogState.Modifiers       = 0xffffffff;

    ogState.GameModeSize.X  = 640;
    ogState.GameModeSize.Y  = 480;
    ogState.GameModeDepth   =  16;
    ogState.GameModeRefresh =  72;

    ogState.Time.Set = GL_FALSE;

    ogListInit( &ogState.Timers );
    ogListInit( &ogState.FreeTimers );

    ogState.IdleCallback = NULL;
    ogState.MenuStateCallback = ( OGCBMenuState )NULL;
    ogState.MenuStatusCallback = ( OGCBMenuStatus )NULL;

    ogState.SwapCount   = 0;
    ogState.SwapTime    = 0;
    ogState.FPSInterval = 0;

    if( ogState.ProgramName )
    {
        free( ogState.ProgramName );
        ogState.ProgramName = NULL;
    }


#if TARGET_HOST_UNIX_X11
    /* Ask that X-client data we have created be destroyed on display close. */
    XSetCloseDownMode( ogDisplay.Display, DestroyAll );

    /* Close display connection; destroy all windows we have created so far. */
    XCloseDisplay( ogDisplay.Display );
#endif
}

/*
 * Everything inside the following #if is copied from the X sources.
 *
 * (NB: "The X sources" are presumably something like the standard
 *  XFree86 v4.4 release.  Unfortunately, the importer neglected
 *  to say, so that will probably remain a mystery.  Since the
 *  code is embedded in an OpenGLUT source file, it was reformatted
 *  to OpenGLUT style.  Should we ever wish to refresh this
 *  function from another X server in the future, it should probably
 *  be stuck in a SEPARATE file, and should be ATTRIBUTED properly so
 *  that we can figure out where the heck it came from (specific
 *  versions, people), and allowed to stay in its own style.)
 */
#if TARGET_HOST_WIN32 || TARGET_HOST_WINCE

/*

Copyright 1985, 1986, 1987,1998  The Open Group

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation.

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE OPEN GROUP BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of The Open Group shall
not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization
from The Open Group.

*/

#define NoValue         0x0000
#define XValue          0x0001
#define YValue          0x0002
#define WidthValue      0x0004
#define HeightValue     0x0008
#define AllValues       0x000F
#define XNegative       0x0010
#define YNegative       0x0020

/*
 *    XParseGeometry parses strings of the form
 *   "=<width>x<height>{+-}<xoffset>{+-}<yoffset>", where
 *   width, height, xoffset, and yoffset are unsigned integers.
 *   Example:  "=80x24+300-49"
 *   The equal sign is optional.
 *   It returns a bitmask that indicates which of the four values
 *   were actually found in the string.  For each value found,
 *   the corresponding argument is updated;  for each value
 *   not found, the corresponding argument is left unchanged.
 */

static int
ReadInteger(char *string, char **NextString)
{
    register int Result = 0;
    int Sign = 1;

    if( *string == '+' )
        string++;
    else if( *string == '-' )
    {
        string++;
        Sign = -1;
    }
    for( ; ( *string >= '0' ) && ( *string <= '9' ); string++ )
        Result = ( Result * 10 ) + ( *string - '0' );
    *NextString = string;
    if( Sign >= 0 )
        return Result;
    else
        return -Result;
}

static int XParseGeometry(
    const char *string,
    int *x,
    int *y,
    unsigned int *width,    /* RETURN */
    unsigned int *height    /* RETURN */
)
{
    int mask = NoValue;
    register char *strind;
    unsigned int tempWidth = 0, tempHeight = 0;
    int tempX = 0, tempY = 0;
    char *nextCharacter;

    if( ( string == NULL ) || ( *string == '\0' ) )
        return mask;
    if( *string == '=' )
        string++;  /* ignore possible '=' at beg of geometry spec */

    strind = ( char * )string;
    if( *strind != '+' && *strind != '-' && *strind != 'x' )
    {
        tempWidth = ReadInteger(strind, &nextCharacter);
        if( strind == nextCharacter )
            return 0;
        strind = nextCharacter;
        mask |= WidthValue;
    }

    if( *strind == 'x' || *strind == 'X' )
    {
        strind++;
        tempHeight = ReadInteger( strind, &nextCharacter );
        if( strind == nextCharacter )
            return 0;
        strind = nextCharacter;
        mask |= HeightValue;
    }

    if( ( *strind == '+' ) || ( *strind == '-' ) )
    {
        if( *strind == '-' )
        {
            strind++;
            tempX = -ReadInteger( strind, &nextCharacter );
            if( strind == nextCharacter )
                return 0;
            strind = nextCharacter;
            mask |= XNegative;
        }
        else
        {
            strind++;
            tempX = ReadInteger(strind, &nextCharacter);
            if( strind == nextCharacter )
                return 0;
            strind = nextCharacter;
        }
        mask |= XValue;
        if( ( *strind == '+' ) || ( *strind == '-' ) )
        {
            if( *strind == '-' )
            {
                strind++;
                tempY = -ReadInteger( strind, &nextCharacter );
                if( strind == nextCharacter )
                    return 0;
                strind = nextCharacter;
                mask |= YNegative;
            }
            else
            {
                strind++;
                tempY = ReadInteger( strind, &nextCharacter );
                if( strind == nextCharacter )
                    return 0;
                strind = nextCharacter;
            }
            mask |= YValue;
        }
    }

    /*
     * If strind isn't at the end of the string the it's an invalid
     * geometry specification.
     */
    if( *strind != '\0' )
        return 0;

    if( mask & XValue )
        *x = tempX;
    if( mask & YValue )
        *y = tempY;
    if( mask & WidthValue )
        *width = tempWidth;
    if( mask & HeightValue )
        *height = tempHeight;
    return mask;
}
#endif

/* -- INTERFACE FUNCTIONS -------------------------------------------------- */

/*!
    \fn
    \brief    Initialize OpenGLUT data structures.
    \ingroup  mainloop
    \param    pargc    Pointer to something like main()'s \a argc.
    \param    argv     Something like main()'s \a argv.

              This function should be called once, near the start of
              any GLUT, freeglut, or OpenGLUT program.  It serves two
              vital roles:

               - It allows OpenGLUT to initialize internal structures.
               - It allows OpenGLUT to process command-line arguments
                 to control the initial window position, etc.

              You should take note of the interaction between
              glutInit() and the related functions such as
              glutInitWindowPosition().  OpenGLUT always uses
              the most recent configuration information, so
              if you call glutInit(), then glutInitWindowPosition(),
              you prevent the user from controlling the initial
              window position via a command-line parameter.

              glutInit() will remove from \a pargc, \a argv
              any parameters that it
              recognizes in the command line.  The following
              command-line parameters are suported:

               - \a -display <i>display-id</i>
                 This allows connection to an alternate X server.
               - \a -geometry <i>geometry-spec</i>
                 This takes width, height, and
                 window position.  The position is given as
                 a signed value (negative values being distance
                 from the far boundary of the screen).  For example,
                 a window geometry of 5x7+11-17 is 5 pixels
                 wide, 7 pixels tall, 11 pixels from the left,
                 and 17 pixels from the bottom edge of the screen.
               - \a -direct Insist on only OpenGL direct rendering.
                 Direct rendering is normally requested but indirect
                 is normally accepted.
                 \a -direct is not always available.
                 See \a -indirect.
               - \a -indirect Attempt only indirect OpenGL rendering.
                 \a -indirect is always available.
                 See \a -direct.
               - \a -iconic Open the window in iconized form.
               - \a -gldebug Print any detected OpenGL errors via
                 glutReportErrors().  Presently
                 done at the bottom of glutMainLoopEvent().
               - \a -sync Synchronize the window system communications
                 heavily.

              Additionally, this function checks whether the
              environment variable \a GLUT_FPS is defined (only on
              UNIX_X11); if so, OpenGLUT will periodically print
              the average number of times per second that your program calls
              glutSwapBuffers().

    \note     You really should always call this, even if you are
              a WIN32 user.  It provides a way for the user to
              directly inform OpenGLUT about preferences without
              the application needing to explicitly deal with
              those issues.  This is also where OpenGLUT retrieves
              your program's name to help disambiguate error and
              warning messages it may be forced to emit.
    \note     Option \a -sync sets a flag,
              but is not actually used at this time.
    \note     Lots of code does XFlush() on the X server, regardless
              of whether \a -sync is specified.  Much of that appears to
              be required in order to support direct client invocation
              of  glutMainLoopEvent(), regrettably.
              However, if one calls glutMainLoop(), instead, we might
              avoid gratuitous XFlush() calls.  (That last sentence
              isn't particularly germain to this function, but there's
              no better place to make this remark at this time.)
              Even for glutMainLoopEvent(), we may be able to coalesce
              many XFlush() calls.
    \see      glutInitWindowPosition(), glutInitWindowSize(),
              glutInitDisplayMode(), glutInitDisplayString(),
              glutCreateWindow(), glutDisplayFunc(),
              glutMainLoop(), glutMainLoopEvent(), glutReportErrors(),
              glutSwapBuffers()
*/
void OGAPIENTRY glutInit( int *pargc, char **argv )
{
    char *displayName = NULL;
    char *geometry = NULL;
    int i, j, argc = *pargc;

    if( ogState.Initialised )
        ogError( "Illegal glutInit() reinitialization attempt." );

    if( pargc && *pargc && argv && *argv && **argv )
    {
        ogState.ProgramName = ogStrDup( *argv );

        if( !ogState.ProgramName )
            ogError( "Could not allocate space for the program's name." );
    }

    ogCreateStructure( );

    ogElapsedTime( );

    /* check if GLUT_FPS env var is set */
#if !TARGET_HOST_WINCE
    {
        const char *fps = getenv( "GLUT_FPS" );
        if( fps )
        {
            int interval;
            sscanf( fps, "%d", &interval );

            if( interval <= 0 )
                ogState.FPSInterval = 5000;  /* 5000 millisecond default */
            else
                ogState.FPSInterval = interval;
        }
    }

    displayName = getenv( "DISPLAY");

    for( i = 1; i < argc; i++ )
    {
        if( strcmp( argv[ i ], "-display" ) == 0 )
        {
            if( ++i >= argc )
                ogError( "-display option must be followed by display name." );

            displayName = argv[ i ];

            argv[ i - 1 ] = NULL;
            argv[ i     ] = NULL;
            ( *pargc ) -= 2;
        }
        else if( strcmp( argv[ i ], "-geometry" ) == 0 )
        {
            if( ++i >= argc )
                ogError(
                    "-geometry option must be followed by window "
                    "geometry settings."
                );

            geometry = argv[ i ];

            argv[ i - 1 ] = NULL;
            argv[ i     ] = NULL;
            ( *pargc ) -= 2;
        }
        else if( strcmp( argv[ i ], "-direct" ) == 0)
        {
            if( ! ogState.TryDirectContext )
                ogError(
                    "option ambiguity, -direct and -indirect "
                    "cannot be both specified."
                );

            ogState.ForceDirectContext = GL_TRUE;
            argv[ i ] = NULL;
            ( *pargc )--;
        }
        else if( strcmp( argv[ i ], "-indirect" ) == 0 )
        {
            if( ogState.ForceDirectContext )
                ogError(
                    "option ambiguity, -direct and -indirect "
                    "cannot be both specified."
                );

            ogState.TryDirectContext = GL_FALSE;
            argv[ i ] = NULL;
            (*pargc)--;
        }
        else if( strcmp( argv[ i ], "-iconic" ) == 0 )
        {
            ogState.ForceIconic = GL_TRUE;
            argv[ i ] = NULL;
            ( *pargc )--;
        }
        else if( strcmp( argv[ i ], "-gldebug" ) == 0 )
        {
            ogState.GLDebugSwitch = GL_TRUE;
            argv[ i ] = NULL;
            ( *pargc )--;
        }
        else if( strcmp( argv[ i ], "-sync" ) == 0 )
        {
            ogState.XSyncSwitch = GL_TRUE;
            argv[ i ] = NULL;
            ( *pargc )--;
        }
    }

    /* Compact {argv}. */
    for( i = j = 1; i < *pargc; i++, j++ )
    {
        /* Guaranteed to end because there are {*pargc} arguments left */
        while( argv[ j ] == NULL )
            j++;
        if( i != j )
            argv[ i ] = argv[ j ];
    }
#endif

    /*
     * Have the display created now. If there wasn't a "-display"
     * in the program arguments, we will use the DISPLAY environment
     * variable for opening the X display (see code above):
     */
    ogInitializeDisplay( displayName );

    /*
     * Geometry parsing deffered until here because we may need the screen
     * size.
     */

    if( geometry )
    {
        int mask = XParseGeometry( geometry,
                                   &ogState.Position.X, &ogState.Position.Y,
                                   &ogState.Size.X, &ogState.Size.Y );

        if( ( mask & ( WidthValue | HeightValue ) ) ==
            ( WidthValue | HeightValue ) )
            ogState.Size.Use = GL_TRUE;

        if( mask & XNegative )
            ogState.Position.X += ogDisplay.ScreenWidth - ogState.Size.X;

        if( mask & YNegative )
            ogState.Position.Y += ogDisplay.ScreenHeight - ogState.Size.Y;

        if( ( mask & ( XValue | YValue ) ) == ( XValue | YValue ) )
            ogState.Position.Use = GL_TRUE;
    }
}

/*!
    \fn
    \brief    Requests future windows to open at a given position.
    \ingroup  window
    \param    x    X coordinate.
    \param    y    Y coordinate.

              This function allows you to request an initial position
              for future windows.

    \see      glutPositionWindow(), glutInit(), glutInitWindowSize(),
              glutInitDisplayMode(), glutInitDisplayString(), glutGet()
*/
void OGAPIENTRY glutInitWindowPosition( int x, int y )
{
    ogState.Position.X = x;
    ogState.Position.Y = y;

    if( ( x >= 0 ) && ( y >= 0 ) )
        ogState.Position.Use = GL_TRUE;
    else
        ogState.Position.Use = GL_FALSE;
}

/*!
    \fn
    \brief    Requests future windows to open at a given width/height..
    \ingroup  window
    \param    width    Width of future windows.
    \param    height   Height of future windows.

              This function allows you to request initial dimensions
              for future windows.

              There is a callback function to inform you of the new
              window shape (whether initially opened, changed by
              your glutReshapeWindow() request, or changed directly
              by the user).
    \see      glutReshapeWindow(), glutInit(), glutInitWindowPosition(),
              glutInitDisplayMode(), glutInitDisplayString(),
              glutReshapeFunc(), glutGet()
*/
void OGAPIENTRY glutInitWindowSize( int width, int height )
{
    ogState.Size.X = width;
    ogState.Size.Y = height;

    if( ( width > 0 ) && ( height > 0 ) )
        ogState.Size.Use = GL_TRUE;
    else
        ogState.Size.Use = GL_FALSE;
}

/*!
    \fn
    \brief    Set the window creation display mode.
    \ingroup  window
    \param    displayMode    Requested display mode bitmask.

              glutInitDisplayMode() allows you to control
              the mode for subsequent OpenGLUT windows.

              Allowable \a displayMode is a combination of:

              - \a GLUT_RGB \n
                   Red, green, blue framebuffer.

              - \a GLUT_RGBA \n
                   Red, green, blue, alpha framebuffer.

              - \a GLUT_INDEX \n
                   Indexed color framebuffer.

              - \a GLUT_SINGLE \n
                   Single-buffered mode.

              - \a GLUT_DOUBLE \n
                   Double-buffered mode.

              - \a GLUT_ACCUM \n
                   Accumulation buffer.

              - \a GLUT_ALPHA \n
                   Alpha channel.

              - \a GLUT_DEPTH \n
                   Depth buffering.

              - \a GLUT_STENCIL \n
                   Stencil buffering.

              - \a GLUT_MULTISAMPLE \n
                   Multisampling mode. (not always available)

              - \a GLUT_STEREO \n
                   Left and right framebuffers.

              - \a GLUT_LUMINANCE \n
                   Greyscale color mode.

              Additionally, the following <i>experimental</i> features
              are implemented:

              - \a GLUT_OFFSCREEN \n
                   Offscreen windows are very much
                   like onscreen windows that have been dragged off of the
                   edge of the screen.  The biggest issue is that offscreen
                   windows do not support subwindows.  Other than that,
                   onscreen windows that are dragged off of the edge may not
                   store graphics that you render (while \a GLUT_OFFSCREEN
                   windows do), and there is no way to drag an offscreen
                   window onscreen for user interaction.

              - \a GLUT_BORDERLESS \n
                   Borderless windows are very experimental,
                   and their precise behavior is not set in stone.
                   See also glutCreateMenuWindow().

              The following are <i>defaults</i>:

              - \a GLUT_RGB
              - \a GLUT_SINGLE

    \bug      \a GLUT_OFFSCREEN windows do not work with nVidia
              cards/drivers.  (Both Win32 and X11)

    \bug      \a GLUT_BORDERLESS seems to vary by the window manager on X11,
              though twm (for example) performs very similarly to WIN32.
              But KDE's window manager (for example) does not let you send
              keystrokes to borderless windows without OpenGLUT hacks.

    \note     Some display mode features were introduced by
              OpenGLUT.

    \note     Not all features or combinations of features are
              valid for all platforms.

    \note     There is no way to change the display mode of an open window.

    \see      glutCreateMenuWindow(),
              glutInit(), glutInitWindowSize(),
              glutInitWindowPosition(), glutInitDisplayString(),
              glutSwapBuffers()
*/
void OGAPIENTRY glutInitDisplayMode( unsigned int displayMode )
{
    ogState.DisplayMode = displayMode;
}


/* -- INIT DISPLAY STRING PARSING ------------------------------------------ */

/*
 * A handy macro for counting items in arrays.
 */
#define NUMBEROF(a)  (sizeof(a)/sizeof(*a))

/*
 * To add a new element, simply add its og_* symbol in this enum list,
 * then add an ENTRY() line in the array below, and finally add a
 * handling case statement in the switch() for glutInitDisplayString().
 * The order does not matter at this point.
 */
enum
{
    og_alpha,
    og_acca,
    og_acc,
    og_blue,
    og_buffer,
    og_conformant,
    og_depth,
    og_double,
    og_green,
    og_index,
    og_num,
    og_red,
    og_rgba,
    og_rgb,
    og_luminance,
    og_stencil,
    og_single,
    og_stereo,
    og_samples,
    og_slow,
    og_win32pdf,
    og_win32pfd,
    og_xvisual,
    og_xstaticgray,
    og_xgrayscale,
    og_xstaticcolor,
    og_xpseudocolor,
    og_xtruecolor,
    og_xdirectcolor
};

/*
 * A simple name:value association.  Make an array out of them
 * for a dictionary.
 */
struct disp_tags
{
    int tag;
    char *name;
};

#define ENTRY(t) {og_ ## t, #t}
static struct disp_tags tags[ ] =
{
    ENTRY( alpha ),
    ENTRY( acca ),
    ENTRY( acc ),
    ENTRY( blue ),
    ENTRY( buffer ),
    ENTRY( conformant ),
    ENTRY( depth ),
    ENTRY( double ),
    ENTRY( green ),
    ENTRY( index ),
    ENTRY( num ),
    ENTRY( red ),
    ENTRY( rgba ),
    ENTRY( rgb ),
    ENTRY( luminance ),
    ENTRY( stencil ),
    ENTRY( single ),
    ENTRY( stereo ),
    ENTRY( samples ),
    ENTRY( slow ),
    ENTRY( win32pdf ),
    ENTRY( win32pfd ),
    ENTRY( xvisual ),
    ENTRY( xstaticgray ),
    ENTRY( xgrayscale ),
    ENTRY( xstaticcolor ),
    ENTRY( xpseudocolor ),
    ENTRY( xtruecolor ),
    ENTRY( xdirectcolor )
};
#undef ENTRY

/*
 * We do some processing of the array the first time we are invoked,
 * so we need a record of whether we have been invoked.
 */
static int tags_initialized;

/*
 * A comparator for our {disp_tags} structure.  This comparator
 * uses the string {name} fields with a conventional strcmp().
 */
static int oghCompareTags( const void *_a, const void *_b )
{
    const struct disp_tags *a = _a, *b = _b;
    return strcmp( a->name, b->name );
}

/*!
    \fn
    \brief    Set the window creation display mode.
    \ingroup  window
    \param    displayMode    Requested display mode string.

              glutInitDisplayString() permits you to define a display
              mode for subsequent windows that you open.  In most
              regards, control is at least as fine as with
              glutInitDisplaymode().

              The \a displayMode parameter is case-sensitive, and
              tokens are separated by ASCII TABs (\\t) and SPACEs.

              - \a index \n
                   Enables \a GLUT_INDEX.

              - \a luminance \n
                   Enables \a GLUT_LUMINANCE.
                   Enables \a GLUT_STENCIL.

              - \a red \n
                   Number of red channel bits.

              - \a green \n
                   Number of green channel bits.

              - \a blue \n
                   Number of blue channel bits.

              - \a alpha \n
                   Number of alpha channel bits.
                   Enables \a GLUT_ALPHA.

              - \a rgb \n
                   Number of \a RGB channel bits, no aplha bits.
                   Enables \a GLUT_RGB.

              - \a rgba \n
                   Number of \a RGBA channel bits.
                   Enables \a GLUT_RGBA.

              - \a depth \n
                   Number of depth buffer bits.

              - \a stencil \n
                   Number of stencil buffer bits.

              - \a double \n
                   Enables \a GLUT_DOUBLE.

              - \a single \n
                   Enables \a GLUT_SINGLE.

              - \a stereo \n
                   Enables \a GLUT_STERO.

              - \a acca \n
                   Number of \a RGBA accumulation bits.
                   Enables \a GLUT_ACCUM.

              - \a acc \n
                   Number of \a RGB accumulation bits.
                   Enables \a GLUT_ACCUM.

              - \a samples \n
                   Number of samples for GLX's \a SGIS_Multisample.
                   Enables \a GLUT_MULTISAMPLE.

              - \a buffer \n
                   [TODO] Sets bits in index mode?

              - \a conformant \n
                   [TODO] Conformant with what?
                   Enables \a GLUT_DEPTH.

              - \a slow \n
                   [TODO] Indicates if a frame-buffer is slow.

              - \a num \n
                   [TODO] Appears to select a frame-buffer configuration
                   by number from an unspecified list.  Probably
                   very non-portable.

                   A special capability  name indicating where the
                   value represents the Nth frame buffer configuration
                   matching the description string


              - \a win32pdf \n
                   Win32 specific: Pixel Format Descriptor

              - \a win32pfd \n
                   Win32 specific: Pixel Format Descriptor

              - \a xvisual \n
                   X11 specific: X Visual

              - \a xstaticgray \n
                   X11 specific: "staticgray" mode.

              - \a xgrayscale \n
                   X11 specific: "grayscale" mode.

              - \a xstaticcolor \n
                   X11 specific: "staticcolor" mode.

              - \a xpseudocolor \n
                   X11 specific: "pseudocolor" mode.

              - \a xtruecolor \n
                   X11 specific: "trueolor" mode.

              - \a xdirectcolor \n
                   X11 specific: "directcolor" mode.

    \note     Conflicting modes, such as \a single and \a double
              have the same interaction as for glutInitDisplayMode().

    \todo     \a GLUT_BORDERLESS and \a GLUT_OFFSCREEN are not represented.

    \todo     Not all features appear to be implemented.  In particular,
              numeric parameters and comparator specifications are lacking.
              See GLUT 3.7 sources for example.

    \todo     PyOpenGL
              <a href="http://pyopengl.sourceforge.net/documentation/manual/glutInitDisplayString.3GLUT.html">
              glutInitDisplayString</a> documentation.

    \see      glutInit(), glutInitWindowPosition(), glutInitWindowSize(),
              glutInitDisplayMode()
*/
void OGAPIENTRY glutInitDisplayString( const char *displayMode )
{
    int glut_state_flag = 0;
    char *token;
    char *buffer = NULL;

    /*
      We need to sort the {tags} array so that we can pass it off
      to bsearch().  bsearch() is used more because of convenience
      than speed, but we don't turn our nose up at speed.

      The list is sorted on first use so that we don't have to
      manually maintain the sort order.
     */
    if( !tags_initialized )
        qsort( tags, NUMBEROF( tags ), sizeof *tags, oghCompareTags );
    tags_initialized = 1;

    if( !displayMode )
        ogError( "glutInitDisplayString() was passed a NULL pointer." );
    buffer = ogStrDup( displayMode );
    if( !buffer )
        ogError( "glutInitDisplayString() was unable to allocate a buffer." );
    token = strtok( buffer, " \t" );
    while( token )
    {
        struct disp_tags *tag = NULL;
        struct disp_tags target;

        target.name = token;
        tag = bsearch(
            &target, tags, NUMBEROF( tags ), sizeof *tags,  oghCompareTags
        );
        if( !tag )
            ogWarning( "display token not recognized: %s.", token );
        else
            switch( tag->tag )
            {
            case og_alpha:     glut_state_flag |= GLUT_ALPHA;       break;
            case og_acc:       glut_state_flag |= GLUT_ACCUM;       break;
            case og_depth:     glut_state_flag |= GLUT_DEPTH;       break;
            case og_double:    glut_state_flag |= GLUT_DOUBLE;      break;
            case og_index:     glut_state_flag |= GLUT_INDEX;       break;
            case og_rgba:      glut_state_flag |= GLUT_RGBA;        break;
            case og_rgb:       glut_state_flag |= GLUT_RGB;         break;
            case og_luminance: glut_state_flag |= GLUT_LUMINANCE;   break;
            case og_stencil:   glut_state_flag |= GLUT_STENCIL;     break;
            case og_single:    glut_state_flag |= GLUT_SINGLE;      break;
            case og_stereo:    glut_state_flag |= GLUT_STEREO;      break;
            case og_samples:   glut_state_flag |= GLUT_MULTISAMPLE; break;

                /*
                  The rest of these are unimplemented at this time.
                */
            case og_acca:       case og_blue:  case og_buffer:
            case og_conformant: case og_green: case og_num:
            case og_red:        case og_slow:
                break;

                /*
                  WIN32-only options.  If we ever implement these, we
                  should add #if brackets.
                */
            case og_win32pdf: case og_win32pfd:
                break;

                /*
                  UNIX_X11 options.  If we ever implement these, we
                  should add #if brackets.
                */
            case og_xvisual:      case og_xstaticgray:  case og_xgrayscale:
            case og_xstaticcolor: case og_xpseudocolor: case og_xtruecolor:
            case og_xdirectcolor:
                break;

            default:
                assert(
                    0 &&
                    "If you see this, OpenGLUT has defined a DisplayMode\n"
                    "string, but has not even implemented a stub for it.\n"
                    "Please report this BUG to the OpenGLUT project.\n"
                );
                break;
            }

        token = strtok( NULL, " \t" );
    }

    free( buffer );

    /* The display mode will be used when creating a new OpenGL context... */
    ogState.DisplayMode = glut_state_flag;
}

