/*!
    \file  og_gamemode.c
    \brief The game mode handling code.
*/
/*
 * Portions copyright (C) 2004, the OpenGLUT project contributors.
 * OpenGLUT branched from freeglut in February, 2004.
 *
 * Copyright (c) 1999-2000 Pawel W. Olszta. All Rights Reserved.
 * Written by Pawel W. Olszta, <olszta@sourceforge.net>
 * Creation date: Thu Dec 16 1999
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

/* -- PRIVATE FUNCTIONS ---------------------------------------------------- */

/*
 * Remembers the current visual settings, so that
 * we can change them and restore later...
 */
static void oghRememberState( void )
{
#if TARGET_HOST_UNIX_X11

    /*
     * This highly depends on the XFree86 extensions,
     * not approved as X Consortium standards
     */
#   ifdef X_XF86VidModeGetModeLine


    /*
     * Remember the current ViewPort location of the screen to be able to
     * restore the ViewPort on LeaveGameMode():
     */
    XF86VidModeGetViewPort(
        ogDisplay.Display,
        ogDisplay.Screen,
        &ogDisplay.DisplayViewPortX,
        &ogDisplay.DisplayViewPortY
    );

    /*
     * Remember the current pointer location before going fullscreen
     * for restoring it later:
     */
    {
        Window junk_window;
        unsigned int mask;

        XQueryPointer(
            ogDisplay.Display, ogDisplay.RootWindow,
            &junk_window, &junk_window,
            &ogDisplay.DisplayPointerX, &ogDisplay.DisplayPointerY,
            &ogDisplay.DisplayPointerX, &ogDisplay.DisplayPointerY, &mask
        );
    }

    /*
     * Query the current display settings:
     */
    ogDisplay.DisplayModeValid =
        XF86VidModeGetModeLine(
            ogDisplay.Display,
            ogDisplay.Screen,
            &ogDisplay.DisplayModeClock,
            &ogDisplay.DisplayMode
        );

    if( !ogDisplay.DisplayModeValid )
        ogWarning( "Runtime use of XF86VidModeGetModeLine failed." );

#   else
    /*
     * XXX warning oghRememberState: missing XFree86 video mode extensions,
     * XXX game mode will not change screen resolution when activated
     */
#   endif

#elif TARGET_HOST_WIN32 || TARGET_HOST_WINCE

    /*    DEVMODE devMode; */

    /*
     * Grab the current desktop settings...
     */

    /* hack to get around my stupid cross-gcc headers */
#   define FREEGLUT_ENUM_CURRENT_SETTINGS -1

    EnumDisplaySettings( NULL, FREEGLUT_ENUM_CURRENT_SETTINGS,
                         &ogDisplay.DisplayMode );

    /*
     * Make sure we will be restoring all settings needed
     */
    ogDisplay.DisplayMode.dmFields |=
        DM_PELSWIDTH | DM_PELSHEIGHT | DM_BITSPERPEL | DM_DISPLAYFREQUENCY;

#endif
}

/*
 * Restores the previously remembered visual settings
 */
static void oghRestoreState( void )
{
#if TARGET_HOST_UNIX_X11

#   ifdef X_XF86VidModeGetAllModeLines
    /*
     * Restore the remembered pointer position:
     */
    XWarpPointer(
        ogDisplay.Display, None, ogDisplay.RootWindow, 0, 0, 0, 0,
        ogDisplay.DisplayPointerX, ogDisplay.DisplayPointerY
    );

    /*
     * This highly depends on the XFree86 extensions,
     * not approved as X Consortium standards
     */

    if( ogDisplay.DisplayModeValid )
    {
        XF86VidModeModeInfo** displayModes;
        int i, displayModesCount;

        XF86VidModeGetAllModeLines(
            ogDisplay.Display,
            ogDisplay.Screen,
            &displayModesCount,
            &displayModes
        );

        /*
         * Check every of the modes looking for one that matches our demands.
         * If we find one, switch to it and restore the remembered viewport.
         */
        for( i = 0; i < displayModesCount; i++ )
            if(displayModes[ i ]->hdisplay == ogDisplay.DisplayMode.hdisplay &&
               displayModes[ i ]->vdisplay == ogDisplay.DisplayMode.vdisplay &&
               displayModes[ i ]->dotclock == ogDisplay.DisplayModeClock )
            {
                XF86VidModeSwitchToMode(
                    ogDisplay.Display,
                    ogDisplay.Screen,
                    displayModes[ i ]
                );
                XF86VidModeSetViewPort(
                     ogDisplay.Display,
                     ogDisplay.Screen,
                     ogDisplay.DisplayViewPortX,
                     ogDisplay.DisplayViewPortY
                );

                /*
                 * For the case this would be the last X11 call the application
                 * calls exit() we've to flush the X11 output queue to have the
                 * commands sent to the X server before the application exits.
                 */
                XFlush( ogDisplay.Display );

                return;
            }
    }

#   else
    /*
     * XXX warning oghRestoreState: missing XFree86 video mode extensions,
     * XXX game mode will not change screen resolution when activated
     */
#   endif

#elif TARGET_HOST_WIN32 || TARGET_HOST_WINCE

    /*
     * Restore the previously rememebered desktop display settings
     */
    ChangeDisplaySettings( &ogDisplay.DisplayMode, 0 );

#endif
}

/*
 * On UNIX_X11, the following function is only referenced if we have
 * the {X_F86VidModeGetAllModeLins} symbol defined from XFree86.
 * Since it is a {static} function, we must protect it by #ifdef
 * to protect against warnings.
 */
#if defined( X_XF86VidModeGetAllModeLines ) || !TARGET_HOST_UNIX_X11
/*
 * Checks the display mode settings against user's preferences
 */
static GLboolean oghCheckDisplayMode(
    int width, int height, int depth, int refresh
)
{
    /*
     * The desired values should be stored in ogState structure...
     */
    return ( width == ogState.GameModeSize.X ) &&
           ( height == ogState.GameModeSize.Y ) &&
           ( depth == ogState.GameModeDepth ) &&
           (refresh == ogState.GameModeRefresh );
}
#endif

/*
 * Changes the current display mode to match user's settings
 */
static GLboolean oghChangeDisplayMode( GLboolean haveToTest )
{
#if TARGET_HOST_UNIX_X11

    /*
     * This highly depends on the XFree86 extensions,
     * not approved as X Consortium standards
     */
#   ifdef X_XF86VidModeGetAllModeLines

    /*
     * This is also used by applcations which check modes by calling
     * glutGameModeGet(GLUT_GAME_MODE_POSSIBLE), so allow the check:
     */
    if( haveToTest || ogDisplay.DisplayModeValid )
    {
        XF86VidModeModeInfo** displayModes;
        int i, displayModesCount;

        XF86VidModeGetAllModeLines(
            ogDisplay.Display,
            ogDisplay.Screen,
            &displayModesCount,
            &displayModes
        );

        /*
         * Check every of the modes looking for one that matches our demands
         */
        for( i = 0; i < displayModesCount; i++ )
            if( oghCheckDisplayMode(
                    displayModes[ i ]->hdisplay,
                    displayModes[ i ]->vdisplay,
                    ogState.GameModeDepth,
                    ogState.GameModeRefresh
                ) )
            {
                if( haveToTest )
                    return GL_TRUE;
                /*
                 * OK, this is the display mode we have been looking for...
                 */
                XF86VidModeSwitchToMode(
                    ogDisplay.Display,
                    ogDisplay.Screen,
                    displayModes[ i ]
                );
                return GL_TRUE;
            }
    }

    /*
     * Something must have went wrong
     */
    return GL_FALSE;

#   else
    /*
     * XXX warning oghChangeDisplayMode: missing XFree86 video mode extensions,
     * XXX game mode will not change screen resolution when activated
     * XXX
     * XXX For compatibility with Solaris, et al, we simply lie and
     * XXX claim to succeed.  (^&  This lets us use game mode for
     * XXX focus-grabbing.
     */
    return GL_TRUE;
#   endif

#elif TARGET_HOST_WIN32 || TARGET_HOST_WINCE

    unsigned int    displayModes = 0, mode = 0xffffffff;
    GLboolean success = GL_FALSE;
    /* HDC      desktopDC; */
    DEVMODE  devMode;

    /*
     * Enumerate the available display modes
     * Try to get a complete match
     */
    while( EnumDisplaySettings( NULL, displayModes, &devMode ) )
    {
        /*
         * Does the enumerated display mode match the user's preferences?
         */
        if( oghCheckDisplayMode(
                devMode.dmPelsWidth,  devMode.dmPelsHeight,
                devMode.dmBitsPerPel, devMode.dmDisplayFrequency
            ) )
        {
            mode = displayModes;
            break;
        }
        displayModes++;
    }

    if( mode == 0xffffffff )
    {
        /* then try without Display Frequency */
        displayModes = 0;

        /*
         * Enumerate the available display modes
         */
        while( EnumDisplaySettings( NULL, displayModes, &devMode ) )
        {
            /* then try without Display Frequency */
            if( oghCheckDisplayMode( devMode.dmPelsWidth,
                                     devMode.dmPelsHeight,
                                     devMode.dmBitsPerPel,
                                     ogState.GameModeRefresh ) )
            {
                mode = displayModes;
                break;
            }
            displayModes++;
        }
    }

    /*
     * Did we find a matching display mode?
     */
    if( mode != 0xffffffff )
    {
        int retVal = DISP_CHANGE_SUCCESSFUL;

        /*
         * Mark the values we want to modify in the display change call
         */
        devMode.dmFields |=
            DM_PELSWIDTH | DM_PELSHEIGHT | DM_BITSPERPEL | DM_DISPLAYFREQUENCY;

        retVal = ChangeDisplaySettings( &devMode, haveToTest ? CDS_TEST : 0 );

        /*
         * I don't know if it's really needed, but looks nice:
         */
        success = (retVal == DISP_CHANGE_SUCCESSFUL) ||
            (retVal == DISP_CHANGE_NOTUPDATED);

        if( !haveToTest && success )
        {
            ogState.GameModeSize.X  = devMode.dmPelsWidth;
            ogState.GameModeSize.Y  = devMode.dmPelsHeight;
            ogState.GameModeDepth   = devMode.dmBitsPerPel;
            ogState.GameModeRefresh = devMode.dmDisplayFrequency;
        }
    }

    return success;

#endif
}


/* -- INTERFACE FUNCTIONS -------------------------------------------------- */

/*!
    \fn
    \brief    Set the game mode display string
    \ingroup  gamemode
    \param    string    A configuration parameter as a string.

              Sets the gamemode status according to an undocumented string.

              Glancing at old GLUT 3.7, the freeglut codebase that we
              inherited does not implement more than a single
              GLUT gamemode ``criteria''. It may not even do that
              much correctly.

              In principle, this code lets you set the video dimensions,
              rendering depth, and video refresh rate.  In practice,
              the combination that you request may be unsupportable
              and the target host may even refuse to honor any such
              changes.

              If you use this function but do not set all options,
              the following defaults may be substituted for some
              values:

               - \a width 640
               - \a height 480
               - \a depth 16
               - \a refresh 72

    \todo     Documentation
    \see      glutGameModeString(), glutEnterGameMode(), glutLeaveGameMode(),
              glutGameModeGet()
*/
void OGAPIENTRY glutGameModeString( const char *string )
{
    int width = 640, height = 480, depth = 16, refresh = 72;

    /*
     * This one seems a bit easier than glutInitDisplayString. The bad thing
     * about it that I was unable to find the game mode string definition, so
     * that I assumed it is: "[width]x[height]:[depth]@[refresh rate]", which
     * appears in all GLUT game mode programs I have seen to date.
     */
    if( sscanf( string, "%ix%i:%i@%i", &width, &height, &depth, &refresh ) !=
        4 )
        if( sscanf( string, "%ix%i:%i", &width, &height, &depth ) != 3 )
            if( sscanf( string, "%ix%i@%i", &width, &height, &refresh ) != 3 )
                if( sscanf( string, "%ix%i", &width, &height ) != 2 )
                    if( sscanf( string, ":%i@%i", &depth, &refresh ) != 2 )
                        if( sscanf( string, ":%i", &depth ) != 1 )
                            if( sscanf( string, "@%i", &refresh ) != 1 )
                                ogWarning(
                                    "Unable to parse game mode string `%s'.",
                                    string
                                );

    /*
     * Hopefully it worked, and if not, we still have the default values
     */
    ogState.GameModeSize.X  = width;
    ogState.GameModeSize.Y  = height;
    ogState.GameModeDepth   = depth;
    ogState.GameModeRefresh = refresh;
}

/*!
    \fn
    \brief    Enter game mode
    \ingroup  gamemode

              Any combination of the following may apply:

               - Resolution change, possibly to the requested
                 resolution, possibly to a "nearest match".
               - Refresh frequency change.
               - A window with a drawable area equal to the requested
                 screen resolution will be opened.
               - The mouse may be restricted to operate within
                 your window.

    \note     Varies in behavior; X users can disable the resolution
              change by an OpenGLUT compile-time option.
    \todo     Documentation
    \todo     OpenGLUT may be unable to restore the original settings
              (this has been observed on WIN32).

    \see      glutGameModeString(), glutEnterGameMode(), glutLeaveGameMode(),
              glutGameModeGet()
*/
int OGAPIENTRY glutEnterGameMode( void )
{
    if( ogStructure.GameMode )
        ogAddToWindowDestroyList( ogStructure.GameMode );
    else
        oghRememberState( );

    if( ! oghChangeDisplayMode( GL_FALSE ) )
    {
        ogWarning( "Failed to change screen settings." );
        return FALSE;
    }

    ogStructure.GameMode = ogCreateWindow(
        NULL, "OpenGLUT", 0, 0,
        ogState.GameModeSize.X, ogState.GameModeSize.Y, OG_CW_GAMEMODE
    );

    ogStructure.GameMode->State.IsGameMode   = GL_TRUE;
    ogStructure.GameMode->State.NeedToResize = GL_TRUE;
    ogStructure.GameMode->State.Width        = ogState.GameModeSize.X;
    ogStructure.GameMode->State.Height       = ogState.GameModeSize.Y;

#if TARGET_HOST_UNIX_X11

    /* Move the window up to the topleft corner */
    XMoveWindow( ogDisplay.Display, ogStructure.Window->Window.Handle, 0, 0 );

    /*
     * Sync needed to avoid a real race, the Xserver must have really created
     * the window before we can grab the pointer into it:
     */
    XSync( ogDisplay.Display, False );

    /* Move the Pointer to the middle of the fullscreen window */
    XWarpPointer(
        ogDisplay.Display,
        None,
        ogDisplay.RootWindow,
        0, 0, 0, 0,
        ogState.GameModeSize.X/2, ogState.GameModeSize.Y/2
    );

    /*
     * Grab the pointer to confine it into the window after the calls to
     * XWrapPointer() which ensure that the pointer really enters the window.
     *
     * We also need to wait here until XGrabPointer() returns GrabSuccess,
     * otherwise the new window is not viewable yet and if the next function
     * (XSetInputFocus) is called with a not yet viewable window, it will exit
     * the application which we have to aviod, so wait until it's viewable:
     */
    while( GrabSuccess != XGrabPointer(
               ogDisplay.Display, ogStructure.GameMode->Window.Handle,
               TRUE,
               ButtonPressMask | ButtonReleaseMask | ButtonMotionMask
               | PointerMotionMask,
               GrabModeAsync, GrabModeAsync,
               ogStructure.GameMode->Window.Handle, None, CurrentTime
           ) )
        usleep( 100 );

    /*
     * Change input focus to the new window. This will exit the application
     * if the new window is not viewable yet, see the XGrabPointer loop above.
     */
    XSetInputFocus(
        ogDisplay.Display,
        ogStructure.GameMode->Window.Handle,
        RevertToNone,
        CurrentTime
    );

#   ifdef X_XF86VidModeSetViewPort

    if( ogDisplay.DisplayModeValid )
    {
        int x, y;
        Window child;

        /* Change to viewport to the window topleft edge: */
        XF86VidModeSetViewPort( ogDisplay.Display, ogDisplay.Screen, 0, 0 );

        /*
         * Final window repositioning: It could be avoided using an undecorated
         * window using override_redirect, but this * would possily require
         * more changes and investigation.
         */

        /* Get the current postion of the drawable area on screen */
        XTranslateCoordinates(
            ogDisplay.Display,
            ogStructure.Window->Window.Handle,
            ogDisplay.RootWindow,
            0, 0, &x, &y,
            &child
        );

        /* Move the decorataions out of the topleft corner of the display */
        XMoveWindow( ogDisplay.Display, ogStructure.Window->Window.Handle,
                     -x, -y);
    }

#endif

    /* Grab the keyboard, too */
    XGrabKeyboard(
        ogDisplay.Display,
        ogStructure.GameMode->Window.Handle,
        FALSE,
        GrabModeAsync, GrabModeAsync,
        CurrentTime
    );

#endif

    return TRUE;
}

/*!
    \fn
    \brief    Leave game mode, returning to normal desktop mode
    \ingroup  gamemode

    \todo     Documentation
    \see      glutGameModeString(), glutEnterGameMode(), glutLeaveGameMode(),
              glutGameModeGet()
*/

void OGAPIENTRY glutLeaveGameMode( void )
{
    if( ogStructure.GameMode )
    {
        ogStructure.GameMode->State.IsGameMode = GL_FALSE;
        ogAddToWindowDestroyList( ogStructure.GameMode );
        ogStructure.GameMode = NULL;
#if TARGET_HOST_UNIX_X11
        XUngrabPointer( ogDisplay.Display, CurrentTime );
        XUngrabKeyboard( ogDisplay.Display, CurrentTime );
#endif
        oghRestoreState();
    }
}

/*!
    \fn
    \brief    Return the value of a game mode parameter
    \ingroup  gamemode
    \param    pname     The parameter value to be returned

              \a pname is one of:

              - \a GLUT_GAME_MODE_ACTIVE \n
                   Return non-zero if we are presently in gamemode.

              - \a GLUT_GAME_MODE_POSSIBLE \n
                   Return whether the requested gamemode settings are viable.
                   (May also actually change the mode?)  Does not necessarily
                   tell you whether entering gamemode will have any effect.

              - \a GLUT_GAME_MODE_WIDTH \n
                   Return the game mode width. (in pixels)

              - \a GLUT_GAME_MODE_HEIGHT \n
                   Return the game mode height. (in pixels)

              - \a GLUT_GAME_MODE_PIXEL_DEPTH \n
                   Return the game mode pixel depth. (in bits)

              - \a GLUT_GAME_MODE_REFRESH_RATE \n
                   Return the game mode vertical refresh frequency. (in Hz)

              - \a GLUT_GAME_MODE_DISPLAY_CHANGED \n
                   Return non-zero if we are presently in gamemode.
                   (Same as \a GLUT_GAME_MODE_ACTIVE.)

              If \a pname is unknown, a warning is printed and a value of
              -1 is returned.

    \todo     Documentation
    \todo     OpenGLUT intrepretation of GLUT_GAME_MODE_DISPLAY_CHANGED
    \see      glutGameModeString(), glutEnterGameMode(), glutLeaveGameMode(),
              glutGameModeGet()
*/

int OGAPIENTRY glutGameModeGet( GLenum pname )
{
    int ret = -1;

    switch( pname )
    {
    case GLUT_GAME_MODE_ACTIVE:
        ret = !!ogStructure.GameMode;
        break;

    case GLUT_GAME_MODE_POSSIBLE:
        ret = oghChangeDisplayMode( GL_TRUE );
        break;

    case GLUT_GAME_MODE_WIDTH:
        ret = ogState.GameModeSize.X;
        break;

    case GLUT_GAME_MODE_HEIGHT:
        ret = ogState.GameModeSize.Y;
        break;

    case GLUT_GAME_MODE_PIXEL_DEPTH:
        ret = ogState.GameModeDepth;
        break;

    case GLUT_GAME_MODE_REFRESH_RATE:
        ret = ogState.GameModeRefresh;
        break;

    case GLUT_GAME_MODE_DISPLAY_CHANGED:
        /* This is true if the game mode has been activated successfully. */
        ret = !!ogStructure.GameMode;
        break;

    default:
        ogWarning( "Unknown gamemode get: %d", pname );
        break;
    }

    return ret;
}
