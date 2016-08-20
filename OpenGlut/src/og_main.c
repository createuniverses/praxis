/*!
    \file  og_main.c
    \brief The windows message processing methods.
*/
/*
 * Portions copyright (C) 2004, the OpenGLUT project contributors.
 * OpenGLUT branched from freeglut in February, 2004.
 *
 * Copyright (c) 1999-2000 Pawel W. Olszta. All Rights Reserved.
 * Written by Pawel W. Olszta, <olszta@sourceforge.net>
 * Creation date: Fri Dec 3 1999
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

#include <limits.h>
#if TARGET_HOST_UNIX_X11
#    include <sys/types.h>
#    include <sys/time.h>
#    include <unistd.h>
#    include <errno.h>
#    include <sys/stat.h>
#elif TARGET_HOST_WIN32
#elif TARGET_HOST_WINCE
     /*! \todo This should be in og_internal.h. */
     typedef struct GXDisplayProperties GXDisplayProperties;
     type def struct GXKeyList GXKeyList;
#    include <gx.h>
     typedef struct GXKeyList ( *GXGETDEFAULTKEYS )( int );
     typedefd int ( *GXOPENINPUT )( void );
     struct GXKeyList gxKeyList;
#endif

#ifndef MAX
#define MAX(a,b) (((a)>(b)) ? (a) : (b))
#endif

#ifndef MIN
#define MIN(a,b) (((a)<(b)) ? (a) : (b))
#endif

#ifdef __PRAXIS_WINDOWS__
HWND g_AppHWND;
#endif


/*
 * TODO BEFORE THE STABLE RELEASE:
 *
 * There are some issues concerning window redrawing under X11, and maybe
 * some events are not handled. The Win32 version lacks some more features,
 * but seems acceptable for not demanding purposes.  (FIXED???)
 *
 * Need to investigate why the X11 version breaks out with an error when
 * closing a window (using the window manager, not glutDestroyWindow)...
 * (If you "Kill" the window, you get "X connection to ... broken
 *  (explicit kill or server shutdown)".  If you "Delete" the window,
 *  you get no message.  Does this qualify now as "FIXED"???)
 */

/* -- PRIVATE VARIABLES ---------------------------------------------------- */

/* -- PRIVATE FUNCTIONS ---------------------------------------------------- */

/*
 * Handle a window configuration change. When no reshape
 * callback is hooked, the viewport size is updated to
 * match the new window size.
 *
 * XXX Change to take {SOG_Window *window} instead, and get the {handle}
 * XXX from the {window}?  Would need to rename the function then...
 */
static void oghReshapeWindowByHandle( SOG_WindowHandleType handle,
                                      int width, int height )
{
    SOG_Window *current_window = ogStructure.Window;

    SOG_Window *window = ogWindowByHandle( handle );
    if( !window )
        return;
    assert( !( window->State.IsOffscreen ) );

    window->State.NeedToResize = GL_FALSE;

#if TARGET_HOST_UNIX_X11

    XResizeWindow( ogDisplay.Display, window->Window.Handle,
                   width, height );
    XFlush( ogDisplay.Display ); /*! \todo Shouldn't need this */

#elif TARGET_HOST_WIN32 || TARGET_HOST_WINCE
#if !TARGET_HOST_WINCE
    {
        RECT rect;
        int x, y, w, h;
        DWORD flags;

        /*
         * For windowed mode, get the current position of the
         * window and resize taking the size of the frame
         * decorations into account.
         */

        GetWindowRect( window->Window.Handle, &rect );

        x = rect.left;
        y = rect.top;
        w = width;
        h = height;

        if( !window->Parent || window->IsClientMenu)
        {
            if( ! window->IsUnmanaged && !window->State.IsGameMode )
            {
                flags = GetWindowLong(window->Window.Handle, GWL_STYLE);
                if(flags & WS_OVERLAPPEDWINDOW)
                {
                    w += GetSystemMetrics( SM_CXSIZEFRAME ) * 2;
                    h += GetSystemMetrics( SM_CYSIZEFRAME ) * 2 +
                        GetSystemMetrics( SM_CYCAPTION );
#ifdef SM_CXPADDEDBORDER
                    w += GetSystemMetrics( SM_CXPADDEDBORDER ) * 2;
                    h += GetSystemMetrics( SM_CXPADDEDBORDER ) * 2;
#endif
                }

//                w += GetSystemMetrics( SM_CXBORDER ) * 2;
//                h += GetSystemMetrics( SM_CYBORDER ) * 2;
//                w += GetSystemMetrics( SM_CXSIZEFRAME ) * 2;
//                h += GetSystemMetrics( SM_CYSIZEFRAME ) * 2;
            }
        }
        else
        {
            GetWindowRect( window->Parent->Window.Handle, &rect );
            x -= rect.left + GetSystemMetrics( SM_CXSIZEFRAME ) * 2;
            y -= rect.top  + GetSystemMetrics( SM_CYSIZEFRAME ) * 2 +
                GetSystemMetrics( SM_CYCAPTION );
        }
        /*
         * SWP_NOACTIVATE      Do not activate the window
         * SWP_NOOWNERZORDER   Do not change position in z-order
         * SWP_NOSENDCHANGING  Supress WM_WINDOWPOSCHANGING message
         * SWP_NOZORDER        Retains the current Z order (ignore 2nd param)
         */

        SetWindowPos(
            window->Window.Handle,
            HWND_TOP, x, y, w, h,
            SWP_NOACTIVATE | SWP_NOOWNERZORDER | SWP_NOSENDCHANGING |
            SWP_NOZORDER
        );
    }
#endif
    /*!
         \note Should update {window->State.OldWidth, window->State.OldHeight}
               to keep in lockstep with UNIX_X11 code.  (Actually, I think
               that we can probably just remove the .Old* members, now.)
    */
    if( FETCH_WCB( *window, Reshape ) )
        INVOKE_WCB( *window, Reshape, ( width, height ) );
    else
    {
        ogSetWindow( window );
        glViewport( 0, 0, width, height );
    }

#endif

    /*
     * Force a window redraw.  In Windows at least this is only a partial
     * solution:  if the window is increasing in size in either dimension,
     * the already-drawn part does not get drawn again and things look funny.
     * But without this we get this bad behaviour whenever we resize the
     * window.
     */
    /*!
       \bug Shouldn't the client's callback invoke this if it is needed?
            Or does GLUT also do this?  This seems *wrong*.
    */
    window->State.Redisplay = GL_TRUE;

    if( window->IsMenu )
        ogSetWindow( current_window );
}


#if TARGET_HOST_WIN32 || TARGET_HOST_WINCE
/*!
 * Calls a window's redraw method. This is used when
 * a redraw is forced by the incoming window messages.
 *
 * \note Presently only used by WIN32.  Causes unreferenced
 * function warnings/errors on UNIX_X11.  We could remove the
 * {static} qualifier, or tuck a bogus reference to the function
 * somewhere.  However, the best options seem to be either to
 * make this the "baseline" redisplay invoker that is always
 * used, or to completely eliminate it from OpenGLUT.
 */
static void oghRedrawWindowByHandle( SOG_WindowHandleType handle )
{
    SOG_Window *window = ogWindowByHandle( handle );
    if( !window )   /* XXX should assert() on this, and/or {handle} */
        return;

    /* XXX When can the below ever fail? When can it possibly matter? */
    if( !( FETCH_WCB( *window, Display ) ) )
        return;

    window->State.Redisplay = GL_FALSE;
    if( !( window->State.Visible ) )
        return;

    if( window->State.NeedToResize )
    {
        SOG_Window *current_window = ogStructure.Window;

        ogSetWindow( window );

        oghReshapeWindowByHandle(
            window->Window.Handle,
            window->State.Width,
            window->State.Height
        );
        ogSetWindow( current_window );
    }
    INVOKE_WCB( *window, Display, ( ) );
}
#endif

/*
 * A static helper function to execute display callback for a window
 */
static void oghcbDisplayWindow( SOG_Window *window,
                                SOG_Enumerator *enumerator )
{
    if( window->State.NeedToResize )
    {
        SOG_Window *current_window = ogStructure.Window;

        ogSetWindow( window );

        oghReshapeWindowByHandle(
            window->Window.Handle,
            window->State.Width,
            window->State.Height
        );

        ogSetWindow( current_window );
    }

    if( window->State.Redisplay &&
        window->State.Visible )
    {
        SOG_Window *current_window = ogStructure.Window;

        /* XXX Set this FALSE even if the window is not visible? */
        window->State.Redisplay = GL_FALSE;

        INVOKE_WCB( *window, Display, ( ) );
        ogSetWindow( current_window );
    }

    ogEnumSubWindows( window, oghcbDisplayWindow, enumerator );
}

/*
 * Make all windows perform a display call
 */
static void oghDisplayAll( void )
{
    SOG_Enumerator enumerator;

    enumerator.found = GL_FALSE;
    enumerator.data  =  NULL;

    ogEnumWindows( oghcbDisplayWindow, &enumerator );
}

/*
 * Window enumerator callback to check for the joystick polling code
 */
static void oghcbCheckJoystickPolls( SOG_Window *window,
                                     SOG_Enumerator *enumerator )
{
    long int checkTime = ogElapsedTime( );

    if( window->State.JoystickLastPoll + window->State.JoystickPollRate <=
        checkTime )
    {
        ogJoystickPollWindow( window );
        window->State.JoystickLastPoll = checkTime;
    }

    ogEnumSubWindows( window, oghcbCheckJoystickPolls, enumerator );
}

/*
 * Check all windows for joystick polling
 */
static void oghCheckJoystickPolls( void )
{
    SOG_Enumerator enumerator;

    enumerator.found = GL_FALSE;
    enumerator.data  =  NULL;

    ogEnumWindows( oghcbCheckJoystickPolls, &enumerator );
}

/*
 * Check the global timers
 */
static void oghCheckTimers( void )
{
    long checkTime = ogElapsedTime( );

    while( ogState.Timers.First )
    {
        SOG_Timer *timer = ogState.Timers.First;

        if( timer->TriggerTime > checkTime )
            break;

        ogListRemove( &ogState.Timers, &timer->Node );
        ogListAppend( &ogState.FreeTimers, &timer->Node );

        timer->Callback( timer->ID );
    }
}

/*
 * Calls the OS time function on an object of type
 * {ogTimeType}.
 */
void oghSetTime( ogTimeType *t )
{
#if TARGET_HOST_UNIX_X11
    gettimeofday( t, NULL );
#elif TARGET_HOST_WIN32
    *t = timeGetTime( );
#elif TARGET_HOST_WINCE
    *t = GetTickCount( );
#endif
}
/*
 * Computes a millisecond delta between two times of type
 * {ogTimeType}.
 */
long oghGetTimeDelta( ogTimeType *a, ogTimeType *b )
{
    long ret;
#if TARGET_HOST_UNIX_X11
    ret =  ( b->tv_usec - a->tv_usec ) / 1000;
    ret += ( b->tv_sec  - a->tv_sec  ) * 1000;
#elif TARGET_HOST_WIN32 || TARGET_HOST_WINCE
    ret = *b - *a;
#endif
    return ret;
}
/*
 * Elapsed Time
 */
long ogElapsedTime( void )
{
    ogTimeType now;

    if( !ogState.Time.Set )
    {
        oghSetTime( &( ogState.Time.Value ) );
        ogState.Time.Set = GL_TRUE;
    }
    oghSetTime( &now );
    return oghGetTimeDelta( &( ogState.Time.Value ), &now );
}

/*
 * Error Messages.
 */
void ogError( const char *fmt, ... )
{
    va_list ap;

    va_start( ap, fmt );

    fprintf( stderr, "OpenGLUT " );
    if( ogState.ProgramName )
        fprintf( stderr, "(%s): ", ogState.ProgramName );
    vfprintf( stderr, fmt, ap );
    fprintf( stderr, "\n" );

    va_end( ap );

    if( ogState.Initialised )
        ogDeinitialize( );

    exit( EXIT_FAILURE );
}

void ogWarning( const char *fmt, ... )
{
    va_list ap;

    va_start( ap, fmt );

    fprintf( stderr, "OpenGLUT " );
    if( ogState.ProgramName )
        fprintf( stderr, "(%s): ", ogState.ProgramName );
    vfprintf( stderr, fmt, ap );
    fprintf( stderr, "\n" );

    va_end( ap );
}

/*
 * Indicates whether Joystick events are being used by ANY window.
 *
 * The current mechanism is to walk all of the windows and ask if
 * there is a joystick callback.  We have a short-circuit early
 * return if we find any joystick handler registered.
 *
 * The real way to do this is to make use of the glutTimer() API
 * to more cleanly re-implement the joystick API.  Then, this code
 * and all other "joystick timer" code can be yanked.
 *
 */
static void ogCheckJoystickCallback( SOG_Window *w, SOG_Enumerator *e)
{
    if( FETCH_WCB( *w, Joystick ) )
    {
        e->found = GL_TRUE;
        e->data = w;
    }
    ogEnumSubWindows( w, ogCheckJoystickCallback, e );
}
static int ogHaveJoystick( void )
{
    SOG_Enumerator enumerator;
    enumerator.found = GL_FALSE;
    enumerator.data = NULL;
    ogEnumWindows( ogCheckJoystickCallback, &enumerator );
    return !!enumerator.data;
}
static void ogHavePendingRedisplaysCallback( SOG_Window *w, SOG_Enumerator *e)
{
    if( w->State.Redisplay )
    {
        e->found = GL_TRUE;
        e->data = w;
    }
    ogEnumSubWindows( w, ogHavePendingRedisplaysCallback, e );
}
static int ogHavePendingRedisplays(void)
{
    SOG_Enumerator enumerator;
    enumerator.found = GL_FALSE;
    enumerator.data = NULL;
    ogEnumWindows( ogHavePendingRedisplaysCallback, &enumerator );
    return !!enumerator.data;
}
/*
 * Returns the number of GLUT ticks (milliseconds) till the next timer event.
 */
static long ogNextTimer( void )
{
    long ret = INT_MAX;
    SOG_Timer *timer = ogState.Timers.First;

    if( timer )
        ret = timer->TriggerTime - ogElapsedTime();
    if( ret < 0 )
        ret = 0;

    return ret;
}
/*
 * Does the magic required to relinquish the CPU until something interesting
 * happens.
 */
static void ogSleepForEvents( void )
{
    long msec;

    if( ogState.IdleCallback || ogHavePendingRedisplays( ) )
        return;

    msec = ogNextTimer( );
    if( ogHaveJoystick( ) )     /* XXX Use GLUT timers for joysticks... */
        msec = MIN( msec, 10 ); /* XXX Dumb; forces granularity to .01sec */

#if TARGET_HOST_UNIX_X11
    /*
     * Possibly due to aggressive use of XFlush() and friends,
     * it is possible to have our socket drained but still have
     * unprocessed events.  (Or, this may just be normal with
     * X, anyway?)  We do non-trivial processing of X events
     * after the event-reading loop, in any case, so we
     * need to allow that we may have an empty socket but non-
     * empty event queue.
     */
    if( ! XPending( ogDisplay.Display ) )
    {
        fd_set fdset;
        int err;
        int socket;
        struct timeval wait;

        socket = ConnectionNumber( ogDisplay.Display );
        FD_ZERO( &fdset );
        FD_SET( socket, &fdset );
        wait.tv_sec = msec / 1000;
        wait.tv_usec = ( msec % 1000 )* 1000;
        err = select( socket+1, &fdset, NULL, NULL, &wait );

        if( -1 == err )
            ogWarning( "OpenGLUT select() error: %d", errno );
    }
#elif TARGET_HOST_WIN32 || TARGET_HOST_WINCE
    MsgWaitForMultipleObjects( 0, NULL, FALSE, msec, QS_ALLEVENTS );
#endif
}

#if TARGET_HOST_UNIX_X11
/*
 * Returns GLUT modifier mask for an XEvent.
 */
static int ogGetXModifiers( XEvent *event )
{
    int ret = 0;

    if( event->xkey.state & ( ShiftMask | LockMask ) )
        ret |= GLUT_ACTIVE_SHIFT;
    if( event->xkey.state & ControlMask )
        ret |= GLUT_ACTIVE_CTRL;
    if( event->xkey.state & Mod1Mask )
        ret |= GLUT_ACTIVE_ALT;

    return ret;
}
#endif

/*
 * This function is to be called whenever an OpenGLUT window is
 * closed by the user.  Should work for all ports.
 */
static void oghTakeActionOnWindowClose( void )
{
    switch( ogState.ActionOnWindowClose )
    {
    case GLUT_ACTION_EXIT:
        ogDeinitialize( );
        exit( EXIT_SUCCESS );
        break;

    case GLUT_ACTION_GLUTMAINLOOP_RETURNS:
        ogState.ExecState = GLUT_EXEC_STATE_STOP;
        if( ogState.InMainLoop )
            longjmp( ogState.BackToMainLoop, 1 );
        break;

    case GLUT_ACTION_CONTINUE_EXECUTION:
        /* NOP */
        break;

    default:
        ogError(
            "Unknown action on window close: %d",
            ogState.ActionOnWindowClose
        );
        break;
    }
}


/* -- INTERFACE FUNCTIONS -------------------------------------------------- */

/*
 * These definitions and functions before glutMainLoopEvent()
 * are temporarily depoted here while I sort out glutMainLoopEvent().
 *
 * Yes, I know, it's all run together.  That's the red flag that this
 * stuff is in flux.  It won't stay like this forever.
 */
typedef enum EOG_EventClass
{
    OG_E_UNKNOWN, /* Unknown event; in this case, dispatch natively */
    OG_E_CREATE,  /* Window creation; have new window dimensions */
    OG_E_COUNT    /* Dummy event; keep as LAST event class */
} EOG_EventClass;
typedef struct SOG_Event
{
#if TARGET_HOST_UNIX_X11
    XEvent         rawEvent;
#elif TARGET_HOST_WIN32
    MSG            rawEvent;
#endif
    EOG_EventClass class;
} SOG_Event;
int oghPendingWindowEvents( SOG_Event *event )
{
#if TARGET_HOST_UNIX_X11
    return XPending( ogDisplay.Display );
#elif TARGET_HOST_WIN32 || TARGET_HOST_WINCE
    return PeekMessage( &( event->rawEvent ), NULL, 0, 0, PM_NOREMOVE );
#endif
}
SOG_Event *oghGetWindowEvent( SOG_Event *event )
{
#if TARGET_HOST_UNIX_X11
    XNextEvent( ogDisplay.Display, &( event->rawEvent ) );
#elif TARGET_HOST_WIN32 || TARGET_HOST_WINCE
    /*
     * WIN32 can apparently report close-window events this way?
     *
     * As I understand it, TranslateMessage() does not alter our
     * {event}, but apparently causes some behind-the-scenes magic
     * to occur.  It may be that it induces a second event in
     * the dispatch.
     */
    if( !GetMessage( &( event->rawEvent ), NULL, 0, 0 ) )
        oghTakeActionOnWindowClose( );
    TranslateMessage( &( event->rawEvent ) );
#endif
    return event;
}

void oghDispatchEvent( SOG_Event *ev )
{
#if TARGET_HOST_WIN32 || TARGET_HOST_WINCE

    DispatchMessage( &( ev->rawEvent ) );

#elif TARGET_HOST_UNIX_X11

    XEvent *event = &( ev->rawEvent );

    SOG_Window *window;
    /* This code was used constantly, so here it goes into a definition: */
#define GETWINDOW(a)                              \
    window = ogWindowByHandle( event->a.window ); \
    if( window == NULL )                          \
        break;
#define GETMOUSE(a)                               \
    window->State.MouseX = event->a.x;            \
    window->State.MouseY = event->a.y;

    switch( event->type )
    {
    case ClientMessage:
        /* Destroy the window when the WM_DELETE_WINDOW message arrives */
        if( ( Atom )event->xclient.data.l[ 0 ] == ogDisplay.DeleteWindow )
        {
            GETWINDOW( xclient );
            ogDestroyWindow( window );
            oghTakeActionOnWindowClose( );
        }
        break;

        /*
         * CreateNotify causes a configure-event so that sub-windows are
         * handled compatibly with GLUT.  Otherwise, your sub-windows
         * (in freeglut/OpenGLUT) will not get an initial reshape event,
         * which can break things.
         *
         * GLUT presumably does this because it generally tries to treat
         * sub-windows the same as windows.
         *
         * CreateNotify only happens if there is a window manager present.
         * We always get a MapNotify, I think, and we can't really do
         * much with the window until it has been mapped, so we could
         * just about as well remove the CreateNotify.
         *
         */
    case CreateNotify:
    case ConfigureNotify:
    case MapNotify:
        GETWINDOW( xconfigure );
        {
            int width = 0;
            int height = 0;

            switch( event->type )
            {
            case CreateNotify:
                width = event->xcreatewindow.width;
                height = event->xcreatewindow.height;
                break;
            case ConfigureNotify:
                width = event->xconfigure.width;
                height = event->xconfigure.height;
                break;
            case MapNotify:
                {
                    int dummy;
                    unsigned udummy;
                    unsigned uwidth, uheight;
                    Window root;

                    Status status;

                    status = XGetGeometry(
                        ogDisplay.Display, window->Window.Handle,
                        &root,
                        &dummy, &dummy,  /* x, y, */
                        &uwidth, &uheight,
                        &udummy, &udummy /* border, depth */
                    );
                    width = ( int )uwidth;
                    height = ( int )uheight;
                }

                break;
            default:
                ogError( "Impossible code has been reached.");
                break;
            }

            if( ( width != window->State.OldWidth ) ||
                ( height != window->State.OldHeight ) )
            {
                window->State.OldWidth = width;
                window->State.OldHeight = height;
                if( FETCH_WCB( *window, Reshape ) )
                    INVOKE_WCB( *window, Reshape, ( width, height ) );
                else
                {
                    ogSetWindow( window );
                    glViewport( 0, 0, width, height );
                }
                glutPostRedisplay( );
            }
        }
        break;

    case DestroyNotify:
        /*
         * This is sent to confirm the XDestroyWindow call.
         *
         * XXX WHY is this commented out?  Should we re-enable it?
         */
        /* ogAddToWindowDestroyList ( window ); */
        break;

    case Expose:
        /*
         * Partial exposes are not of particular interest to us...
         *
         * Potentially we could do some culling in single-buffered
         * mode, but the API provides no way to pass this information
         * back to the application - so we mark the entire window
         * for redisplay.
         *
         */
        if( event->xexpose.count == 0 )
        {
            GETWINDOW( xexpose );
            ogSetWindow( window );
            glutPostRedisplay( );
        }
        break;

    case MappingNotify:
        /*
         * Have the client's keyboard knowledge updated (xlib.ps,
         * page 206, says that's a good thing to do)
         */
        XRefreshKeyboardMapping( ( XMappingEvent * )event );
        break;

    case EnterNotify:
    case LeaveNotify:
        GETWINDOW( xcrossing );
        GETMOUSE( xcrossing );  /* XXX Why do we need the mouse info? */

        /*
         * XXX This hack allows borderless windows to be used
         * XXX as main windows.  We should find
         * XXX a better way so that window managers know how
         * XXX to assign the input focus, rather than taking
         * XXX control away from the window manager.
         */
        if( window->IsBorderless )
        {
            if( LeaveNotify == event->type )
                /* Restore the focus state as the mouse leaves */
                XSetInputFocus(
                    ogDisplay.Display,
                    window->Window.PrevFocus,
                    window->Window.PrevFocusReturnTo,
                    CurrentTime
                );
            else
            {
                /* Save the current focus state for later      */
                XGetInputFocus(
                    ogDisplay.Display,
                    &window->Window.PrevFocus,
                    &window->Window.PrevFocusReturnTo
                );

                /* Set the current focus to the window         */
                XSetInputFocus(
                    ogDisplay.Display,
                    window->Window.Handle,
                    RevertToNone, CurrentTime
                );
            }
        }

        INVOKE_WCB(
            *window,
            Entry,
            ( ( EnterNotify == event->type ) ? GLUT_ENTERED : GLUT_LEFT )
        );
        break;

    case MotionNotify:
        GETWINDOW( xmotion );
        GETMOUSE( xmotion );

        if( window->ActiveMenu )
        {
            if( window == window->ActiveMenu->ParentWindow )
            {
                window->ActiveMenu->Window->State.MouseX =
                    event->xmotion.x_root - window->ActiveMenu->X;
                window->ActiveMenu->Window->State.MouseY =
                    event->xmotion.y_root - window->ActiveMenu->Y;
            }
            window->ActiveMenu->Window->State.Redisplay = GL_TRUE;
            ogSetWindow( window->ActiveMenu->ParentWindow );

            break;
        }

        /*
         * XXX For more than 5 buttons, just check {event.xmotion.state},
         * XXX rather than a host of bit-masks?  Or maybe we need to
         * XXX track ButtonPress/ButtonRelease events in our own
         * XXX bit-mask?
         */
#define BUTTON_MASK \
  ( Button1Mask | Button2Mask | Button3Mask | Button4Mask | Button5Mask )
        if( event->xmotion.state & BUTTON_MASK )
            INVOKE_WCB( *window, Motion, ( event->xmotion.x,
                                           event->xmotion.y ) );
        else
            INVOKE_WCB( *window, Passive, ( event->xmotion.x,
                                            event->xmotion.y ) );
        break;

    case ButtonRelease:
    case ButtonPress:
        {
            GLboolean pressed = GL_TRUE;
            int button;

            if( event->type == ButtonRelease )
                pressed = GL_FALSE;

            /*
             * A mouse button has been pressed or released. Traditionally,
             * break if the window was found within the OpenGLUT structures.
             */
            GETWINDOW( xbutton );
            GETMOUSE( xbutton );

            /*
             * An X button (at least in XFree86) is numbered from 1.
             * A GLUT button is numbered from 0.
             * Old GLUT passed through buttons other than just the first
             * three, though it only gave symbolic names and official
             * support to the first three.
             */
            button = event->xbutton.button - 1;

            /*
             * XXX See the Menu module discussion for the button
             * XXX policy re. menus.
             */
            /* Window has an active menu, it absorbs any mouse click */
            if( window->ActiveMenu )
            {
                if( window == window->ActiveMenu->ParentWindow )
                {
                    window->ActiveMenu->Window->State.MouseX =
                        event->xbutton.x_root - window->ActiveMenu->X;
                    window->ActiveMenu->Window->State.MouseY =
                        event->xbutton.y_root - window->ActiveMenu->Y;
                }

                /* In the menu, invoke the callback and deactivate the menu*/
                if( ogCheckActiveMenu( window->ActiveMenu->Window,
                                       window->ActiveMenu ) )
                {
                    /*
                     * Save the current window and menu and set the current
                     * window to the window whose menu this is
                     */
                    SOG_Window *save_window = ogStructure.Window;
                    SOG_Menu *save_menu = ogStructure.Menu;
                    SOG_Window *parent_window =
                        window->ActiveMenu->ParentWindow;
                    ogSetWindow( parent_window );
                    ogStructure.Menu = window->ActiveMenu;

                    /* Execute the menu callback */
                    ogExecuteMenuCallback( window->ActiveMenu );
                    ogDeactivateMenu( parent_window );

                    /* Restore the current window and menu */
                    ogSetWindow( save_window );
                    ogStructure.Menu = save_menu;
                }
                else if( pressed )
                    /*
                     * Outside the menu, deactivate if it's a downclick
                     *
                     * XXX This isn't enough.  A downclick outside of
                     * XXX the interior of our OpenGLUT windows should also
                     * XXX deactivate the menu.  This is more complicated.
                     */
                    ogDeactivateMenu( window->ActiveMenu->ParentWindow );

                /*
                 * XXX Why does an active menu require a redisplay at
                 * XXX this point?  If this can come out cleanly, then
                 * XXX it probably should do so; if not, a comment should
                 * XXX explain it.
                 */
                window->State.Redisplay = GL_TRUE;
                break;
            }

            /* No active menu, let's check whether we need to activate one. */
            if( ( 0 <= button ) &&
                ( FREEGLUT_MAX_MENUS > button ) &&
                ( window->Menu[ button ] ) &&
                pressed )
            {
                /* XXX Posting a Redisplay seems bogus. */
                window->State.Redisplay = GL_TRUE;
                ogSetWindow( window );
                ogActivateMenu( window, button );
                break;
            }

            /*
             * Check if there is a mouse or mouse wheel callback hooked to the
             * window.
             *
             * XXX The callback invocation should do this check for us.
             * XXX Why are we redundantly doing it ourselves?
             */
            if( ! FETCH_WCB( *window, Mouse ) &&
                ! FETCH_WCB( *window, MouseWheel ) )
                break;

            ogState.Modifiers = ogGetXModifiers( event );

            /* Choose between mouse-button or mouse-wheel reporting. */
            if( ( glutDeviceGet( GLUT_NUM_MOUSE_BUTTONS ) > button ) ||
                ( !FETCH_WCB( *window, MouseWheel ) ) )
                INVOKE_WCB( *window, Mouse, ( button,
                                              pressed ? GLUT_DOWN : GLUT_UP,
                                              event->xbutton.x,
                                              event->xbutton.y )
                );
            else
            {
                /*
                 * Map 4 and 5 to wheel zero; EVEN to +1, ODD to -1
                 *  "  6 and 7 "    "   one; ...
                 *
                 * XXX This *should* be behind some variables/macros,
                 * XXX since the order and numbering isn't certain
                 * XXX See XFree86 configuration docs (even back in the
                 * XXX 3.x days, and especially with 4.x).
                 *
                 * XXX Note that {button} has already been decremeted
                 * XXX in mapping from X button numbering to GLUT.
                 */
                int wheel_number =
                    ( button - glutDeviceGet( GLUT_NUM_MOUSE_BUTTONS ) ) / 2;
                int direction = -1;
                if( button % 2 )
                    direction = 1;

                if( pressed )
                    INVOKE_WCB( *window, MouseWheel, ( wheel_number,
                                                       direction,
                                                       event->xbutton.x,
                                                       event->xbutton.y )
                    );
            }

            /* Trash the modifiers state */
            ogState.Modifiers = 0xffffffff;
        }
        break;

    case KeyRelease:
    case KeyPress:
        {
            OGCBKeyboard keyboard_cb;
            OGCBSpecial special_cb;

            GETWINDOW( xkey );
            GETMOUSE( xkey );

            if( KeyPress == event->type )
            {
                //printf("KeyPress: %d, %d\n", event->xkey.keycode, event->xkey.state);
                //fflush(stdout);
            }
            else
            {
                //printf("KeyRelease: %d, %d\n", event->xkey.keycode, event->xkey.state);
                //fflush(stdout);
            }

            /* Detect repeated keys if configured globally or per-window */

            if(
                ogState.KeyRepeat != GLUT_KEY_REPEAT_OFF &&
                window->State.IgnoreKeyRepeat != GL_TRUE
            )
                window->State.KeyRepeating = GL_FALSE;
            else if( event->type == KeyRelease )
            {
                /*
                 * Look at X11 keystate to detect repeat mode.
                 * While X11 says the key is actually held down, we'll
                 * ignore KeyRelease/KeyPress pairs.
                 */

                /* 32*8 = 256, the range of keycodes for XQueryKeymap */
                char keys[ 32 ];
                int key_byte = event->xkey.keycode >> 3;
                int key_mask = 1 << ( event->xkey.keycode & 7 );
                XQueryKeymap( ogDisplay.Display, keys );
                if( 256 > event->xkey.keycode )
                {
                    if( keys[ key_byte ] & key_mask )
                        window->State.KeyRepeating = GL_TRUE;
                    else
                        window->State.KeyRepeating = GL_FALSE;
                }
            }

            /* Cease processing this event if it is auto repeated */

            if( window->State.KeyRepeating )
                break;

            if( KeyPress == event->type )
            {
                keyboard_cb = FETCH_WCB( *window, Keyboard );
                special_cb  = FETCH_WCB( *window, Special  );
            }
            else
            {
                keyboard_cb = FETCH_WCB( *window, KeyboardUp );
                special_cb  = FETCH_WCB( *window, SpecialUp  );
            }

            /* Is there a keyboard/special callback hooked for this window? */
            if( keyboard_cb || special_cb )
            {
                XComposeStatus composeStatus;
                char asciiCode[ 32 ];
                KeySym keySym;
                int len;

                /* Check for the key codes associated with the event: */
                len = XLookupString( &( event->xkey ), asciiCode,
                                     sizeof( asciiCode ), &keySym,
                                     &composeStatus
                );

                if( KeyPress == event->type )
                {
                    //printf("KeyPress CB: asciiCode[0] = %d, keySym = %d\n", asciiCode[0],keySym);
                    //fflush(stdout);

                    /* 65056 is Shift Tab, XK_ISO_Left_Tab
                     */

                    // keyboard
                    // special

                    ogSetWindow( window );
                    ogState.Modifiers = ogGetXModifiers( event );
                    keyboard_cb( event->xkey.keycode, // keySym,
                                 event->xkey.x, event->xkey.y
                    );
                    ogState.Modifiers = 0xffffffff;

                    if(asciiCode[0] != 0)
                    {
                        ogSetWindow( window );
                        ogState.Modifiers = ogGetXModifiers( event );
                        special_cb( asciiCode[0], event->xkey.x, event->xkey.y );
                        ogState.Modifiers = 0xffffffff;
                    }
                }
                else
                {
                    //printf("KeyRelease CB: asciiCode[0] = %d, keySym = %d\n", asciiCode[0],keySym);
                    //fflush(stdout);

                    // keyboard

                    ogSetWindow( window );
                    ogState.Modifiers = ogGetXModifiers( event );
                    keyboard_cb( event->xkey.keycode, // keySym,
                                 event->xkey.x, event->xkey.y
                    );
                    ogState.Modifiers = 0xffffffff;
                }

#if 0

                /* printf("GLUT Key, keySym = %d, len = %d\n", keySym, len);
                fflush(stdout); */

                int special = -1;

                switch( keySym )
                {
                case XK_F1:     special = GLUT_KEY_F1;     break;
                case XK_F2:     special = GLUT_KEY_F2;     break;
                case XK_F3:     special = GLUT_KEY_F3;     break;
                case XK_F4:     special = GLUT_KEY_F4;     break;
                case XK_F5:     special = GLUT_KEY_F5;     break;
                case XK_F6:     special = GLUT_KEY_F6;     break;
                case XK_F7:     special = GLUT_KEY_F7;     break;
                case XK_F8:     special = GLUT_KEY_F8;     break;
                case XK_F9:     special = GLUT_KEY_F9;     break;
                case XK_F10:    special = GLUT_KEY_F10;    break;
                case XK_F11:    special = GLUT_KEY_F11;    break;
                case XK_F12:    special = GLUT_KEY_F12;    break;

                case XK_Left:   special = GLUT_KEY_LEFT;   break;
                case XK_Right:  special = GLUT_KEY_RIGHT;  break;
                case XK_Up:     special = GLUT_KEY_UP;     break;
                case XK_Down:   special = GLUT_KEY_DOWN;   break;

                //case XK_ISO_Left_Tab:
                //case XK_Tab:    special = GLUT_KEY_TAB;    break;

                    // asciiCode[0] is being set to 0
                    // for some modified ascii keys.
                    // I'm setting it back.
                case XK_ISO_Left_Tab:
                case XK_Tab:    asciiCode[0] = 9;    break;
                    // Because its set to 0 instead of 9 if you're
                    // holding shift tab instead of just tab.
                    // I guess that makes sense because it is no
                    // longer an ascii character...
                    // shift of other keys changes their ascii character
                    // after all

                case XK_space: asciiCode[0] = 32; break;


                case XK_KP_Prior:
                case XK_Prior:  special = GLUT_KEY_PAGE_UP; break;
                case XK_KP_Next:
                case XK_Next:   special = GLUT_KEY_PAGE_DOWN; break;
                case XK_KP_Home:
                case XK_Home:   special = GLUT_KEY_HOME;   break;
                case XK_KP_End:
                case XK_End:    special = GLUT_KEY_END;    break;
                case XK_KP_Insert:
                case XK_Insert: special = GLUT_KEY_INSERT; break;
                }

                if(special==-1)
                {
                    if( keyboard_cb )
                    {
                        /* printf("GLUT Key, asciiCode[0] = %d, keySym = %d\n", asciiCode[0],keySym);
                         * fflush(stdout);
                         *
                         * 65056 is Shift Tab, XK_ISO_Left_Tab
                         */


                        ogSetWindow( window );
                        ogState.Modifiers = ogGetXModifiers( event );
                        keyboard_cb( asciiCode[ 0 ],
                                     event->xkey.x, event->xkey.y
                        );
                        ogState.Modifiers = 0xffffffff;
                    }
                }
                else
                {
                    /*
                     * Execute the callback (if one has been specified),
                     * given that the special code seems to be valid...
                     */
                    if( special_cb )
                    {
                        /*printf("GLUT SPECIAL, special = %d\n", special);
                        fflush(stdout);*/

                        ogSetWindow( window );
                        ogState.Modifiers = ogGetXModifiers( event );
                        special_cb( special, event->xkey.x, event->xkey.y );
                        ogState.Modifiers = 0xffffffff;
                    }
                }
#endif
            }
            break;
        }

    case ReparentNotify:
        break; /* XXX Should disable this event */

    case UnmapNotify:
        /* NOP */
        break;

    case SelectionClear:
        printf("SelectionClear\n");
        fflush(stdout);
        break;

    case SelectionRequest:
    {
        XSelectionRequestEvent *req;
        XEvent sevent;
        int seln_format;
        unsigned long nbytes;
        unsigned long overflow;
        unsigned char *seln_data;

        req = &event->xselectionrequest;

        printf("SelectionRequest (requestor = %ld, target = %ld)\n",
            req->requestor, req->target);
        fflush(stdout);

#if 1
        sevent.xany.type = SelectionNotify;
        sevent.xselection.selection = req->selection;
        sevent.xselection.target = None;
        sevent.xselection.property = None;
        sevent.xselection.requestor = req->requestor;
        sevent.xselection.time = req->time;
        if (XGetWindowProperty(ogDisplay.Display, DefaultRootWindow(ogDisplay.Display),
                XA_CUT_BUFFER0, 0, INT_MAX/4, False, req->target,
                &sevent.xselection.target, &seln_format, &nbytes,
                &overflow, &seln_data) == Success) {
            Atom XA_TARGETS = XInternAtom(ogDisplay.Display, "TARGETS", 0);
            if (sevent.xselection.target == req->target) {
                XChangeProperty(ogDisplay.Display, req->requestor, req->property,
                    sevent.xselection.target, seln_format, PropModeReplace,
                    seln_data, nbytes);
                sevent.xselection.property = req->property;
            } else if (XA_TARGETS == req->target) {
                Atom SupportedFormats[] = { sevent.xselection.target, XA_TARGETS };
                XChangeProperty(ogDisplay.Display, req->requestor, req->property,
                    XA_ATOM, 32, PropModeReplace,
                    (unsigned char*)SupportedFormats,
                    sizeof(SupportedFormats)/sizeof(*SupportedFormats));
                sevent.xselection.property = req->property;
            }
            XFree(seln_data);
        }
        XSendEvent(ogDisplay.Display, req->requestor, False, 0, &sevent);
        XSync(ogDisplay.Display, False);
#endif
    }
        break;

    case SelectionNotify:
    {
        printf("SelectionNotify (requestor = %ld, target = %ld)\n",
            event->xselection.requestor, event->xselection.target);
        fflush(stdout);
    }
        break;

    case VisibilityNotify:
        GETWINDOW( xvisibility );
        /* XXX INVOKE_WCB() does this check for us. */
        if( !FETCH_WCB( *window, WindowStatus ) )
            break;
        ogSetWindow( window );

        /*
         * Sending this event, the X server can notify us that the window
         * has just acquired one of the three possible visibility states:
         * VisibilityUnobscured, VisibilityPartiallyObscured or
         * VisibilityFullyObscured
         */
        switch( event->xvisibility.state )
        {
        case VisibilityUnobscured:
            INVOKE_WCB( *window, WindowStatus, ( GLUT_FULLY_RETAINED ) );
            window->State.Visible = GL_TRUE;
            break;

        case VisibilityPartiallyObscured:
            INVOKE_WCB( *window, WindowStatus,
                        ( GLUT_PARTIALLY_RETAINED ) );
            window->State.Visible = GL_TRUE;
            break;

        case VisibilityFullyObscured:
            INVOKE_WCB( *window, WindowStatus, ( GLUT_FULLY_COVERED ) );
            window->State.Visible = GL_FALSE;
            break;

        default:
            ogWarning( "Unknown X visibility state: %d",
                       event->xvisibility.state );
            break;
        }
        break;

    default:
        ogWarning( "Unknown X event type: %d", event->type );
        break;
    }
#endif
}


/*!
    \fn
    \brief    Dispatches all pending events.
    \ingroup  mainloop

              The general outline of this function is to first drain
              the queue of windowsystem events, in most cases dispatching
              each as it is found.  After the queue is empty, we check
              for timer-based events, coalesced window events (e.g.,
              redisplays), and windows that need to be closed.

              The cross-reference section for this function's
              documentation should ideally contain every
              callback, but the list would be tediously long and
              prone to omissions.

    \note     Does not necessarily dispatch events that are received
              <i>after</i> this function starts processing.
    \note     At first glance, this function may not seem to afford any
              new capability that you couldn't get with an idle callback
              or glutLeaveMainLoop().  However there are other GLUT-like
              libraries that may have their own window event processing
              loops.  Having glutMainLoopEvent() allows you to ask
              OpenGLUT to do its work in a batch, then return to whatever
              processing the other library (or libraries) require.
    \see      glutIdleFunc(), glutLeaveMainLoop(), glutMainLoop()
*/
void OGAPIENTRY glutMainLoopEvent( void )
{
    SOG_Event event;
    freeglut_assert_ready; /* XXX Looks like assert() abuse... */

    while( oghPendingWindowEvents( &event ) )
    {
        oghGetWindowEvent( &event );
        oghDispatchEvent( &event );
    }

    if( ogState.Timers.First )
        oghCheckTimers( );
    oghCheckJoystickPolls( );
    oghDisplayAll( );

    ogCloseWindows( );
    if( ogState.GLDebugSwitch )
        glutReportErrors( );
}

/*!
    \fn
    \brief    The standard GLUT event loop entry point.
    \ingroup  mainloop

              This is the main driving force for an event-driven
              OpenGLUT program.  It alternates between calling
              glutMainLoopEvent() to process pending events and then
              either sleeping or calling your idle function
              (see glutIdleFunc()).

              This function <i>can</i> return, but GLUT's version
              of this function never returned.  And you must
              do special things to OpenGLUT to cause OpenGLUT's
              version to return.

              The cross-reference section for this function's
              documentation should ideally contain every
              callback, but the list would be tediously long and
              prone to omissions.

    \bug      Talking to other message systems (e.g., network layers)
              can be a bit bothersome under the GLUT event model.
    \internal
    \note     For OpenGLUT developers' internal documentation:
              Runs until the \a ExecState changes to \a GLUT_EXEC_STATE_STOP.
    \see      glutMainLoopEvent(), glutLeaveMainLoop(), glutIdleFunc()
*/
void OGAPIENTRY glutMainLoop( void )
{
#if TARGET_HOST_WIN32 || TARGET_HOST_WINCE
    SOG_Window *window = ( SOG_Window * )ogStructure.Windows.First;
#endif

    freeglut_assert_ready;

#if TARGET_HOST_WIN32 || TARGET_HOST_WINCE
    /*
     * Processing before the main loop:  If there is a window which is open and
     * which has a visibility callback, call it.  I know this is an ugly hack,
     * but I'm not sure what else to do about it.  Ideally we should leave
     * something uninitialized in the create window code and initialize it in
     * the main loop, and have that initialization create a "WM_ACTIVATE"
     * message.  Then we would put the visibility callback code in the
     * "case WM_ACTIVATE" block below.         - John Fay -- 10/24/02
     */
    while( window )
    {
        if( FETCH_WCB( *window, Visibility ) )
        {
            SOG_Window *current_window = ogStructure.Window;

            INVOKE_WCB( *window, Visibility, ( window->State.Visible ) );
            ogSetWindow( current_window );
        }

        window = ( SOG_Window * )window->Node.Next;
    }
#endif

    ogState.ExecState = GLUT_EXEC_STATE_RUNNING;
    while( ogState.ExecState == GLUT_EXEC_STATE_RUNNING )
    {
        SOG_Window *window;

        ogState.InMainLoop = 1;
        if( !setjmp( ogState.BackToMainLoop ) )
            glutMainLoopEvent( );

        /*
         * Step through the list of windows, seeing if there are any
         * that are not menus
         */
        for( window = ( SOG_Window * )ogStructure.Windows.First;
             window;
             window = ( SOG_Window * )window->Node.Next )
            if( !( window->IsMenu ) )
                break;

        if( ! window )
            ogState.ExecState = GLUT_EXEC_STATE_STOP;
        else
        {
            if( ogState.IdleCallback )
                ogState.IdleCallback( );

            ogSleepForEvents( );
        }
    }

    /*
     * When this loop terminates, destroy the display, state and structure
     * of a OpenGLUT session, so that another glutInit() call can happen
     */
    ogDeinitialize( );

    ogState.InMainLoop = 0;
}

/*!
    \fn
    \brief    Breaks out of OpenGLUT's glutMainLoop()
    \ingroup  mainloop

              This function allows you to unilaterally tell OpenGLUT
              that you are done and wish to exit.  This is useful if
              you have also told OpenGLUT to return to you rather than
              to call exit() directly.

    \internal
    \todo     Could use longjmp(); see oghTakeActionOnWindowClose().
              This would let us terminate the entire loop immediately.
    \see      glutMainLoop(), exit()
*/
void OGAPIENTRY glutLeaveMainLoop( void )
{
    ogState.ExecState = GLUT_EXEC_STATE_STOP;
}


#if TARGET_HOST_WIN32
/*
 * Determine a GLUT modifer mask based on MS-WINDOWS system info.
 */
int ogGetWin32Modifiers( void )
{
    return
        ( ( ( GetKeyState( VK_LSHIFT   ) < 0 ) ||
            ( GetKeyState( VK_RSHIFT   ) < 0 )) ? GLUT_ACTIVE_SHIFT : 0 ) |
        ( ( ( GetKeyState( VK_LCONTROL ) < 0 ) ||
            ( GetKeyState( VK_RCONTROL ) < 0 )) ? GLUT_ACTIVE_CTRL  : 0 ) |
        ( ( ( GetKeyState( VK_LMENU    ) < 0 ) ||
            ( GetKeyState( VK_RMENU    ) < 0 )) ? GLUT_ACTIVE_ALT   : 0 );
}

void oglcbDoMMWOMDONE(DWORD dwParam1);

int g_nPanBegin = 0;
int g_nPanStartPos = 0;

#define PRAXIS_GESTURE_SUPPORT
#undef PRAXIS_GESTURE_SUPPORT

#ifdef PRAXIS_GESTURE_SUPPORT
LRESULT DecodeGesture(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    // Create a structure to populate and retrieve the extra message info.
    GESTUREINFO gi;
    BOOL bResult;
    BOOL bHandled = FALSE;
    int delta = 0;
    int deltaabs=0;
    int i = 0;

    SOG_Window *window = ogWindowByHandle( hWnd );

    ZeroMemory(&gi, sizeof(GESTUREINFO));

    gi.cbSize = sizeof(GESTUREINFO);

    bResult  = GetGestureInfo((HGESTUREINFO)lParam, &gi);
    bHandled = FALSE;


    if (bResult){
        // now interpret the gesture
        switch (gi.dwID){
           case GID_BEGIN:
            g_nPanStartPos = gi.ptsLocation.y;
               //g_nPanBegin = 1;
               break;
           case GID_ZOOM:
               // Code for zooming goes here
               bHandled = TRUE;
               break;
           case GID_PAN:
            // Code for panning goes here
               if(1)//(g_nPanBegin>1)
               {
                   delta=g_nPanStartPos-gi.ptsLocation.y;

                   deltaabs = delta;
                   if(deltaabs<0) deltaabs *= -1;
#if 1
                   if(delta<-5)
                       delta=-1;
                   else if(delta>5)
                       delta=1;
                   else
                       delta=0;
#endif

                   //for(i=0;i<deltaabs;i++)
                   {
                   if(delta!=0)
                   {
                       if( FETCH_WCB( *window, MouseWheel ) )
                           INVOKE_WCB( *window, MouseWheel,
                                   ( 8,
                                     delta,
                                     window->State.MouseX,
                                     window->State.MouseY
                                   )
                       );
                       g_nPanStartPos = gi.ptsLocation.y;
                   }
                   }
               }

               bHandled = TRUE;
               break;
           case GID_ROTATE:
               // Code for rotation goes here
               bHandled = TRUE;
               break;
           case GID_TWOFINGERTAP:
               // Code for two-finger tap goes here
               bHandled = TRUE;
               break;
           case GID_PRESSANDTAP:
               // Code for roll over goes here
               bHandled = TRUE;
               break;
           default:
               // A gesture was not recognized
               break;
        }
    }else{
        DWORD dwErr = GetLastError();
        if (dwErr > 0){
            //MessageBoxW(hWnd, L"Error!", L"Could not retrieve a GESTUREINFO structure.", MB_OK);
        }
    }
    if (bHandled){
        return 0;
    }else{
        return DefWindowProc(hWnd, message, wParam, lParam);
    }
}
#endif


/*
 * The window procedure for handling Win32 events
 */
LRESULT CALLBACK ogWindowProc( HWND hWnd, UINT uMsg, WPARAM wParam,
                               LPARAM lParam )
{
    SOG_Window *window = ogWindowByHandle( hWnd );
    PAINTSTRUCT ps;
    LONG lRet = 1;

	int nLo = LOWORD(lParam);
	int nHi = HIWORD(lParam);

    if( !window && ( uMsg != WM_CREATE ) )
        return DefWindowProc( hWnd, uMsg, wParam, lParam );

    /*
    {
       static FILE *f;
       if( !f )
           f = fopen( "c:\\OpenGLUT.log", "w" );
       f = stdout;
       if( ( uMsg != WM_SETCURSOR ) &&
           ( uMsg != WM_NCHITTEST ) &&
           ( uMsg != WM_MOUSEMOVE ) &&
           f )
       {
           fprintf(
               f,
               "Window %3d message <%04x> %12d %12d\n", window?window->ID:0,
               uMsg, wParam, lParam
           );
           fflush( f );
       }
    }
    */

    switch( uMsg )
    {
    case MM_WOM_DONE:
        oglcbDoMMWOMDONE(lParam);
        break;
    case WM_CREATE:
        /* The window struct is passed as the creation structure paramter. */
        window = ( SOG_Window * )
            ( ( ( LPCREATESTRUCT )lParam )->lpCreateParams );
        assert( window != NULL );

        g_AppHWND = hWnd;

        window->Window.Handle = hWnd;
        window->Window.Device = GetDC( hWnd );
        if( window->IsMenu )
        {
            unsigned int current_DisplayMode = ogState.DisplayMode;
            ogState.DisplayMode = GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH;
#if !TARGET_HOST_WINCE
            ogSetupPixelFormat( window, GL_FALSE, PFD_MAIN_PLANE );
#endif
            ogState.DisplayMode = current_DisplayMode;

            if( ogStructure.MenuContext )
                wglMakeCurrent( window->Window.Device,
                                ogStructure.MenuContext->Context
                );
            else
            {
                ogStructure.MenuContext =
                    (SOG_MenuContext *)malloc( sizeof( SOG_MenuContext ) );
                ogStructure.MenuContext->Context =
                    wglCreateContext( window->Window.Device );
            }

            /* window->Window.Context = wglGetCurrentContext ();   */
            window->Window.Context = wglCreateContext( window->Window.Device );
        }
        else
        {
#if !TARGET_HOST_WINCE
            ogSetupPixelFormat( window, GL_FALSE, PFD_MAIN_PLANE );
#endif

            if( !ogState.UseCurrentContext )
                window->Window.Context =
                    wglCreateContext( window->Window.Device );
            else
            {
                window->Window.Context = wglGetCurrentContext( );
                if( !window->Window.Context )
                    window->Window.Context =
                        wglCreateContext( window->Window.Device );
            }
        }
        /*
         * XXX These do not seem to really be required.  I had to
         * XXX remove them to keep glutCreateMenuWindow() windows
         * XXX from being forced to resize (overriding the dimensions
         * XXX passed to glutCreateMenuWindow()).
         *
         * window->State.NeedToResize = GL_TRUE;
         */
         //window->State.NeedToResize = GL_TRUE;
         window->State.Width  = ogState.Size.X;
         window->State.Height = ogState.Size.Y;

        ReleaseDC( window->Window.Handle, window->Window.Device );
#if TARGET_HOST_WINCE
        /* Take over button handling */
        {
            HINSTANCE dxDllLib = LoadLibrary( _T( "gx.dll" ) );
            if( dxDllLib )
            {
                GXGetDefaultKeys_ = ( GXGETDEFAULTKEYS )GetProcAddress(
                    dxDllLib, _T( "?GXGetDefaultKeys@@YA?AUGXKeyList@@H@Z" )
                );
                GXOpenInput_ = (GXOPENINPUT )GetProcAddress(
                    dxDllLib, _T( "?GXOpenInput@@YAHXZ" )
                );
            }
            if( GXOpenInput_ )
                GXOpenInput_( );
            if( GXGetDefaultKeys_ )
                gxKeyList = GXGetDefaultKeys_( GX_LANDSCAPEKEYS );
        }
#endif
        break;

    case WM_SIZE:
        /*
         * If the window is visible, then it is the user manually resizing it.
         * If it is not, then it is the system sending us a dummy resize with
         * zero dimensions on a "glutIconifyWindow" call.
         */
        if( window->State.Visible )
        {
            window->State.NeedToResize = GL_TRUE;
#if TARGET_HOST_WINCE
            window->State.Width  = HIWORD(lParam);
            window->State.Height = LOWORD(lParam);
#else
            window->State.Width  = LOWORD(lParam);
            window->State.Height = HIWORD(lParam);
#endif
			if(0)
			{
				SOG_Window *current_window = ogStructure.Window;

				ogSetWindow( window );

				oghReshapeWindowByHandle(
					window->Window.Handle,
					window->State.Width,
					window->State.Height
				);

				ogSetWindow( current_window );
			}
			//InvalidateRect(hWnd, 0, TRUE);
			//UpdateWindow(hWnd);
        }
        break;

	//case WM_MOVE:
	//	{
	//		int nXPos = (int)(short) LOWORD(lParam);
	//		int nYPos = (int)(short) HIWORD(lParam);

	//		//InvalidateRect(hWnd, 0, TRUE);
	//		//UpdateWindow(hWnd);

	//		//int nBlah = 0;
	//		//nBlah++;
	//	}
	//	break;

#if 0
    case WM_SETFOCUS:
        /* printf("WM_SETFOCUS: %p\n", window ); */
        lRet = DefWindowProc( hWnd, uMsg, wParam, lParam );
        break;

    case WM_ACTIVATE:
        if( LOWORD( wParam ) != WA_INACTIVE )
        {
            /* glutSetCursor( ogStructure.Window->State.Cursor ); */
            /* printf("WM_ACTIVATE: glutSetCursor( %p, %d)\n", window,
                      window->State.Cursor ); */
            glutSetCursor( window->State.Cursor );
        }

        lRet = DefWindowProc( hWnd, uMsg, wParam, lParam );
        break;
#endif

        /*
         * XXX Why not re-use some common code with the glutSetCursor()
         * XXX function (or perhaps invoke glutSetCursor())?
         * XXX That is, why are we duplicating code, here, from
         * XXX glutSetCursor()?  The WIN32 code should be able to just
         * XXX call glutSetCursor() instead of defining two macros
         * XXX and implementing a nested case in-line.
         */
    case WM_SETCURSOR:
        /* Set the cursor AND change it for this window class. */
#define MAP_CURSOR(a,b)                 \
    case a:                             \
    SetCursor( LoadCursor( NULL, b ) ); \
    break;

        /* Nuke the cursor AND change it for this window class. */
#define ZAP_CURSOR(a,b) \
    case a:             \
    SetCursor( NULL );  \
    break;

        if( LOWORD( lParam ) == HTCLIENT )
            switch( window->State.Cursor )
            {
                MAP_CURSOR( GLUT_CURSOR_RIGHT_ARROW, IDC_ARROW     );
                MAP_CURSOR( GLUT_CURSOR_LEFT_ARROW,  IDC_ARROW     );
                MAP_CURSOR( GLUT_CURSOR_INFO,        IDC_HELP      );
                MAP_CURSOR( GLUT_CURSOR_DESTROY,     IDC_CROSS     );
                MAP_CURSOR( GLUT_CURSOR_HELP,        IDC_HELP      );
                MAP_CURSOR( GLUT_CURSOR_CYCLE,       IDC_SIZEALL   );
                MAP_CURSOR( GLUT_CURSOR_SPRAY,       IDC_CROSS     );
                MAP_CURSOR( GLUT_CURSOR_WAIT,        IDC_WAIT      );
                MAP_CURSOR( GLUT_CURSOR_TEXT,        IDC_UPARROW   );
                MAP_CURSOR( GLUT_CURSOR_CROSSHAIR,   IDC_CROSS     );
                /* MAP_CURSOR( GLUT_CURSOR_NONE,        IDC_NO         ); */
                ZAP_CURSOR( GLUT_CURSOR_NONE,        NULL          );

            default:
                MAP_CURSOR( GLUT_CURSOR_UP_DOWN,     IDC_ARROW     );
            }
        else
            lRet = DefWindowProc( hWnd, uMsg, wParam, lParam );
        break;

    case WM_SHOWWINDOW:
        window->State.Visible = GL_TRUE;
        window->State.Redisplay = GL_TRUE;
        break;

    case WM_PAINT:
        /* Turn on the visibility in case it was turned off somehow */
        BeginPaint( hWnd, &ps );

		//window->State.NeedToResize = GL_FALSE;
        window->State.Visible = GL_TRUE;
        window->State.Redisplay = GL_TRUE;

		{
			SOG_Window *current_window = ogStructure.Window;

			ogSetWindow( window );

			if( FETCH_WCB( *window, Reshape ) )
				INVOKE_WCB( *window, Reshape, ( window->State.Width, window->State.Height ) );

			INVOKE_WCB( *window, Display, ( ) );

			ogSetWindow( current_window );
		}

        EndPaint( hWnd, &ps );
        break;

    case WM_CLOSE:
        ogDestroyWindow( window );
        if( ogState.ActionOnWindowClose != GLUT_ACTION_CONTINUE_EXECUTION )
            PostQuitMessage( 0 );
        oghTakeActionOnWindowClose( );
        break;

    case WM_DESTROY:
        /* The window already got destroyed, so don't bother with it. */
        return 0;

    case WM_MOUSEMOVE:
    {
#if TARGET_HOST_WINCE
        window->State.MouseX = 320-HIWORD( lParam );
        window->State.MouseY = LOWORD( lParam );
#else
        // recasting unsigned short to signed short corrects discontinuity
        // when mouse moves past left or top border
        window->State.MouseX = (signed short)(LOWORD( lParam ));
        window->State.MouseY = (signed short)(HIWORD( lParam ));
#endif

        if( window->ActiveMenu )
        {
            window->State.Redisplay = GL_TRUE;
            ogSetWindow( window->ActiveMenu->ParentWindow );
            break;
        }

        ogState.Modifiers = ogGetWin32Modifiers( );

        if( ( wParam & MK_LBUTTON ) ||
            ( wParam & MK_MBUTTON ) ||
            ( wParam & MK_RBUTTON ) )
            INVOKE_WCB( *window, Motion, ( window->State.MouseX,
                                           window->State.MouseY ) );
        else
            INVOKE_WCB( *window, Passive, ( window->State.MouseX,
                                            window->State.MouseY ) );

        ogState.Modifiers = 0xffffffff;
    }
    break;

    case WM_LBUTTONDOWN:
    case WM_MBUTTONDOWN:
    case WM_RBUTTONDOWN:
    case WM_LBUTTONUP:
    case WM_MBUTTONUP:
    case WM_RBUTTONUP:
    {
        GLboolean pressed = GL_TRUE;
        int button;

#if TARGET_HOST_WINCE
        window->State.MouseX = 320-HIWORD( lParam );
        window->State.MouseY = LOWORD( lParam );
#else
        window->State.MouseX = LOWORD( lParam );
        window->State.MouseY = HIWORD( lParam );
#endif

        switch( uMsg )
        {
        case WM_LBUTTONDOWN:
            pressed = GL_TRUE;
            button = GLUT_LEFT_BUTTON;
            break;
        case WM_MBUTTONDOWN:
            pressed = GL_TRUE;
            button = GLUT_MIDDLE_BUTTON;
            break;
        case WM_RBUTTONDOWN:
            pressed = GL_TRUE;
            button = GLUT_RIGHT_BUTTON;
            break;
        case WM_LBUTTONUP:
            pressed = GL_FALSE;
            button = GLUT_LEFT_BUTTON;
            break;
        case WM_MBUTTONUP:
            pressed = GL_FALSE;
            button = GLUT_MIDDLE_BUTTON;
            break;
        case WM_RBUTTONUP:
            pressed = GL_FALSE;
            button = GLUT_RIGHT_BUTTON;
            break;
        default:
            pressed = GL_FALSE;
            button = -1;
            break;
        }

	/*
	 * XXX Perhaps should do the capture BEFORE callback (here),
	 * XXX and release AFTER callback.
	 *
	 * XXX Should track mouse-button status.
	 */
        if( pressed )
            SetCapture( window->Window.Handle );
        else
            ReleaseCapture( );

#if !TARGET_HOST_WINCE
        if( GetSystemMetrics( SM_SWAPBUTTON ) )
            if( button == GLUT_LEFT_BUTTON )
                button = GLUT_RIGHT_BUTTON;
            else
                if( button == GLUT_RIGHT_BUTTON )
                    button = GLUT_LEFT_BUTTON;
#endif

        if( button == -1 )
            return DefWindowProc( hWnd, uMsg, lParam, wParam );

        /*
         * XXX See the Menu module discussion for the button
         * XXX policy re. menus.
         */
        /* Window has an active menu, it absorbs any mouse click */
        if( window->ActiveMenu )
        {
            /* Outside the menu, deactivate the menu if it's a downclick */
            if( ! ogCheckActiveMenu( window, window->ActiveMenu ) )
            {
                if( pressed )
                    ogDeactivateMenu( window->ActiveMenu->ParentWindow );
            }
            else  /* In menu, invoke the callback and deactivate the menu*/
            {
                /*
                 * Save the current window and menu and set the current
                 * window to the window whose menu this is
                 */
                SOG_Window *save_window = ogStructure.Window;
                SOG_Menu *save_menu = ogStructure.Menu;
                SOG_Window *parent_window = window->ActiveMenu->ParentWindow;
                ogSetWindow( parent_window );
                ogStructure.Menu = window->ActiveMenu;

                /* Execute the menu callback */
                ogExecuteMenuCallback( window->ActiveMenu );
                ogDeactivateMenu( parent_window );

                /* Restore the current window and menu */
                ogSetWindow( save_window );
                ogStructure.Menu = save_menu;
            }

            /*
             * Let's make the window redraw as a result of the mouse
             * click and menu activity.
             */
            if( ! window->IsMenu )
                window->State.Redisplay = GL_TRUE;

            break;
        }

        if( window->Menu[ button ] && pressed )
        {
            window->State.Redisplay = GL_TRUE;
            ogSetWindow( window );
            ogActivateMenu( window, button );

            break;
        }

        if( ! FETCH_WCB( *window, Mouse ) )
            break;

        ogSetWindow( window );
        ogState.Modifiers = ogGetWin32Modifiers( );

        INVOKE_WCB(
            *window, Mouse,
            ( button,
              pressed ? GLUT_DOWN : GLUT_UP,
              window->State.MouseX,
              window->State.MouseY
            )
        );

        ogState.Modifiers = 0xffffffff;
    }
    break;

#if PRAXIS_GESTURE_SUPPORT
    case 0x0119: /* WM_GESTURE */
        DecodeGesture(hWnd, uMsg, wParam, lParam);
        break;
#endif

#if 0
    case 0x011A: /* WM_GESTURENOTIFY */
        printf("WM_GESTURENOTIFY\n");
        fflush(stdout);
        break;
#endif

    case 0x020a:
        /* Should be WM_MOUSEWHEEL but my compiler doesn't recognize it */
    {
        /*
         * XXX THIS IS SPECULATIVE -- John Fay, 10/2/03
         * XXX Should use WHEEL_DELTA instead of 120
         */
        int wheel_number = LOWORD( wParam );
        short ticks = ( short )HIWORD( wParam ) / 120;
        int direction = 1;

        if( ticks < 0 )
        {
            direction = -1;
            ticks = -ticks;
        }

        //printf("mw %d,%d,%d\n", wheel_number,ticks,direction);
        //fflush(stdout);

        /*
         * The mouse cursor has moved. Remember the new mouse cursor's position
         */
        /*        window->State.MouseX = LOWORD( lParam ); */
        /* Need to adjust by window position, */
        /*        window->State.MouseY = HIWORD( lParam ); */
        /* change "lParam" to other parameter */

        if( ! FETCH_WCB( *window, MouseWheel ) &&
            ! FETCH_WCB( *window, Mouse ) )
            break;

        ogSetWindow( window );
        ogState.Modifiers = ogGetWin32Modifiers( );

        while( ticks-- )
            if( FETCH_WCB( *window, MouseWheel ) )
                INVOKE_WCB( *window, MouseWheel,
                            ( wheel_number,
                              direction,
                              window->State.MouseX,
                              window->State.MouseY
                            )
                );
            else  /* No mouse wheel, call the mouse button callback twice */
            {
                /*
                 * XXX freeglut appeared to be one-off here---plus had
                 * XXX a hard-coded assumption about buttons.
                 */
                int button = wheel_number*2 +
                    glutDeviceGet( GLUT_NUM_MOUSE_BUTTONS );
                if( direction > 0 )
                    ++button;
                INVOKE_WCB( *window, Mouse,
                            ( button, GLUT_DOWN,
                              window->State.MouseX, window->State.MouseY )
                );
                INVOKE_WCB( *window, Mouse,
                            ( button, GLUT_UP,
                              window->State.MouseX, window->State.MouseX )
                );
            }

        ogState.Modifiers = 0xffffffff;
    }
    break;

    case WM_VSCROLL:
    //printf("vscroll\n");
    //fflush(stdout);
        break;

    case WM_SYSKEYDOWN:
    case WM_KEYDOWN:
    {
        int keypress = -1;
        POINT mouse_pos;

        if( ( ogState.KeyRepeat == GLUT_KEY_REPEAT_OFF ||
              window->State.IgnoreKeyRepeat == GL_TRUE ) &&
            ( HIWORD( lParam ) & KF_REPEAT ) )
            break;

        /*
         * Remember the current modifiers state. This is done here in order
         * to make sure the VK_DELETE keyboard callback is executed properly.
         */
        ogState.Modifiers = ogGetWin32Modifiers( );

        GetCursorPos( &mouse_pos );
        ScreenToClient( window->Window.Handle, &mouse_pos );

        window->State.MouseX = mouse_pos.x;
        window->State.MouseY = mouse_pos.y;

        /* GregS 6-Oct-2014 */
        //printf("WM_KEYDOWN wParam = %#02x\n", wParam);
        //fflush(stdout);

        /* GregS 21-Oct-2015 */
        keypress = wParam;
        INVOKE_WCB( *window, Keyboard,
                    ( keypress,
                      window->State.MouseX, window->State.MouseY )
        );

        ogState.Modifiers = 0xffffffff;

        /* GregS 21-Oct-2015 */
#if 0

        /* Convert the Win32 keystroke codes to GLUTtish way */
#       define KEY(a,b) case a: keypress = b; break;

        switch( wParam )
        {
            KEY( VK_F1,     GLUT_KEY_F1        );
            KEY( VK_F2,     GLUT_KEY_F2        );
            KEY( VK_F3,     GLUT_KEY_F3        );
            KEY( VK_F4,     GLUT_KEY_F4        );
            KEY( VK_F5,     GLUT_KEY_F5        );
            KEY( VK_F6,     GLUT_KEY_F6        );
            KEY( VK_F7,     GLUT_KEY_F7        );
            KEY( VK_F8,     GLUT_KEY_F8        );
            KEY( VK_F9,     GLUT_KEY_F9        );
            KEY( VK_F10,    GLUT_KEY_F10       );
            KEY( VK_F11,    GLUT_KEY_F11       );
            KEY( VK_F12,    GLUT_KEY_F12       );
            KEY( VK_PRIOR,  GLUT_KEY_PAGE_UP   );
            KEY( VK_NEXT,   GLUT_KEY_PAGE_DOWN );
            KEY( VK_HOME,   GLUT_KEY_HOME      );
            KEY( VK_END,    GLUT_KEY_END       );
            KEY( VK_LEFT,   GLUT_KEY_LEFT      );
            KEY( VK_UP,     GLUT_KEY_UP        );
            KEY( VK_RIGHT,  GLUT_KEY_RIGHT     );
            KEY( VK_DOWN,   GLUT_KEY_DOWN      );
            KEY( VK_INSERT, GLUT_KEY_INSERT    );
            /* KEY( VK_TAB,    GLUT_KEY_TAB       );
             * The tab key is ASCII TAB
             */
        case VK_DELETE:
            /* The delete key is ASCII DEL */
            INVOKE_WCB( *window, Keyboard,
                        ( 127, window->State.MouseX, window->State.MouseY )
            );
            break;

        case VK_TAB:
            /* The tab key is ASCII TAB */
            INVOKE_WCB( *window, Keyboard,
                        ( 9, window->State.MouseX, window->State.MouseY )
            );
            break;
        }

#if TARGET_HOST_WINCE
        if(!(lParam & 0x40000000)) /* Prevent auto-repeat */
        {
            if(wParam==(unsigned)gxKeyList.vkRight)
                keypress = GLUT_KEY_RIGHT;
            else if(wParam==(unsigned)gxKeyList.vkLeft)
                keypress = GLUT_KEY_LEFT;
            else if(wParam==(unsigned)gxKeyList.vkUp)
                keypress = GLUT_KEY_UP;
            else if(wParam==(unsigned)gxKeyList.vkDown)
                keypress = GLUT_KEY_DOWN;
            else if(wParam==(unsigned)gxKeyList.vkA)
                keypress = GLUT_KEY_F1;
            else if(wParam==(unsigned)gxKeyList.vkB)
                keypress = GLUT_KEY_F2;
            else if(wParam==(unsigned)gxKeyList.vkC)
                keypress = GLUT_KEY_F3;
            else if(wParam==(unsigned)gxKeyList.vkStart)
                keypress = GLUT_KEY_F4;
        }
#endif

        if( keypress != -1 )
            INVOKE_WCB( *window, Special,
                        ( keypress,
                          window->State.MouseX, window->State.MouseY )
            );

        ogState.Modifiers = 0xffffffff;
#endif
    }
    break;

    case WM_SYSKEYUP:
    case WM_KEYUP:
    {
        int keypress = -1;
        POINT mouse_pos;

        /*
         * Remember the current modifiers state. This is done here in order
         * to make sure the VK_DELETE keyboard callback is executed properly.
         */
        ogState.Modifiers = ogGetWin32Modifiers( );

        GetCursorPos( &mouse_pos );
        ScreenToClient( window->Window.Handle, &mouse_pos );

        window->State.MouseX = mouse_pos.x;
        window->State.MouseY = mouse_pos.y;

        /* GregS 6-Oct-2014 */
        //printf("WM_KEYUP wParam = %#02x\n", wParam);
        //fflush(stdout);

        /* GregS 21-Oct-2014 */
        keypress = wParam;
        INVOKE_WCB( *window, KeyboardUp,
                    ( keypress,
                      window->State.MouseX, window->State.MouseY )
        );

        ogState.Modifiers = 0xffffffff;

        /* GregS 21-Oct-2014 */
#if 0
        /*
         * Convert the Win32 keystroke codes to GLUTtish way.
         * "KEY(a,b)" was defined under "WM_KEYDOWN"
         */

        switch( wParam )
        {
            KEY( VK_F1,     GLUT_KEY_F1        );
            KEY( VK_F2,     GLUT_KEY_F2        );
            KEY( VK_F3,     GLUT_KEY_F3        );
            KEY( VK_F4,     GLUT_KEY_F4        );
            KEY( VK_F5,     GLUT_KEY_F5        );
            KEY( VK_F6,     GLUT_KEY_F6        );
            KEY( VK_F7,     GLUT_KEY_F7        );
            KEY( VK_F8,     GLUT_KEY_F8        );
            KEY( VK_F9,     GLUT_KEY_F9        );
            KEY( VK_F10,    GLUT_KEY_F10       );
            KEY( VK_F11,    GLUT_KEY_F11       );
            KEY( VK_F12,    GLUT_KEY_F12       );
            KEY( VK_PRIOR,  GLUT_KEY_PAGE_UP   );
            KEY( VK_NEXT,   GLUT_KEY_PAGE_DOWN );
            KEY( VK_HOME,   GLUT_KEY_HOME      );
            KEY( VK_END,    GLUT_KEY_END       );
            KEY( VK_LEFT,   GLUT_KEY_LEFT      );
            KEY( VK_UP,     GLUT_KEY_UP        );
            KEY( VK_RIGHT,  GLUT_KEY_RIGHT     );
            KEY( VK_DOWN,   GLUT_KEY_DOWN      );
            KEY( VK_INSERT, GLUT_KEY_INSERT    );

          case VK_DELETE:
              /* The delete key is ASCII DEL. */
              INVOKE_WCB( *window, KeyboardUp,
                          ( 127, window->State.MouseX, window->State.MouseY )
              );
              break;

        default:
        {
#if !TARGET_HOST_WINCE
            BYTE state[ 256 ];
            WORD code[ 2 ];

            GetKeyboardState( state );

            if( ToAscii( wParam, 0, state, code, 0 ) == 1 )
                wParam=code[ 0 ];

            INVOKE_WCB( *window, KeyboardUp,
                        ( ( char )wParam,
                          window->State.MouseX, window->State.MouseY )
            );
#endif
        }
        }

        if( keypress != -1 )
            INVOKE_WCB( *window, SpecialUp,
                        ( keypress,
                          window->State.MouseX, window->State.MouseY )
            );

        ogState.Modifiers = 0xffffffff;
#endif
    }
    break;

    case WM_SYSCHAR:
    case WM_CHAR:
    {
        if( ( ogState.KeyRepeat==GLUT_KEY_REPEAT_OFF ||
              window->State.IgnoreKeyRepeat==GL_TRUE ) &&
            ( HIWORD( lParam ) & KF_REPEAT ) )
            break;

        /* GregS 6-Oct-2014 */
        //printf("WM_CHAR wParam = %#02x\n", wParam);
        //fflush(stdout);

        // TAB will only be handled in the WM_KEYDOWN section,
        // (where it wasn't previously)
        // since holding Ctrl prevents it being noticed here.

        // Ctrl-i gets captured here, so I'm turning off this early out.
        // Side effects:
        //if(wParam == VK_TAB)
        //    break;

        // GregS 21-Oct-2015
        // Special now means Printable
        // Keyboard literally refers to the keyboard
        ogState.Modifiers = ogGetWin32Modifiers( );
        INVOKE_WCB( *window, Special,
                    ( ( char )wParam,
                      window->State.MouseX, window->State.MouseY )
        );
        ogState.Modifiers = 0xffffffff;
    }
    break;

    case WM_CAPTURECHANGED:
        /*
         * User has finished resizing the window, force a redraw */
        /*
         * XXX Probably should instead post a "window resized" event.
         * XXX Let the cliet call glutPostRedisplay() if needed.
         */
        window->State.Redisplay = GL_TRUE;

        /*lRet = DefWindowProc( hWnd, uMsg, wParam, lParam ); */
        break;

        /* Other messages that I have seen and which are not handled already */
    case WM_SETTEXT:  /* 0x000c */
        lRet = DefWindowProc( hWnd, uMsg, wParam, lParam );
        /* Pass it on to "DefWindowProc" to set the window text */
        break;

    case WM_GETTEXT:  /* 0x000d */
        /* Ideally we would copy the title of the window into "lParam" */
        /* strncpy ( (char *)lParam, "Window Title", wParam );
           lRet = ( wParam > 12 ) ? 12 : wParam;  */
        /* the number of characters copied */
        lRet = DefWindowProc( hWnd, uMsg, wParam, lParam );
        break;

    case WM_GETTEXTLENGTH:  /* 0x000e */
        /* Ideally we would get the length of the title of the window */
        lRet = 12;
        /* the number of characters in "Window Title\0" (see above) */
        break;

    case WM_ERASEBKGND:  /* 0x0014 */
        lRet = DefWindowProc( hWnd, uMsg, wParam, lParam );
        break;

#if !TARGET_HOST_WINCE
    case WM_SYNCPAINT:  /* 0x0088 */
        /* Another window has moved, need to update this one */
        window->State.Redisplay = GL_TRUE;
        lRet = DefWindowProc( hWnd, uMsg, wParam, lParam );
        /* Help screen says this message must be passed to "DefWindowProc" */
        break;

    case WM_NCPAINT:  /* 0x0085 */
        /* Need to update the border of this window */
        lRet = DefWindowProc( hWnd, uMsg, wParam, lParam );
        /* Pass it on to "DefWindowProc" to repaint a standard border */
        break;

    case WM_SYSCOMMAND:  /* 0x0112 */
            /*
             * XXX This is 90% unimplemented.  I see no reason not to
             * XXX collapse all of the identically-handled cases
             * XXX into one case statement until there is code to
             * XXX implement the features.
             *
             * We have received a system command message.  Try to
             * act on it. The commands are passed in through the
             * {lParam} parameter.
             *
             * Note that at least for SC_SIZE (SC_MOVE?), there is
             * a whole sub-class of different events, differentiated
             * by the low-order 4 bits.  For the SC_SIZE case,
             * the structure of the low order bits can be decoded
             * pictorially with:
             *
             *  4 3 5
             *  1   2
             *  7 6 8
             *
             * (Think: "Left is +1, right is +2, up is +3, down is +6",
             *  so this is a ternary combination, with 00 (middle)
             *  undefined so far as I know; likewise 0x9 through 0xf.)
             *
             * SC_MOVE has low-order bit setting of 2 if you click in the
             * title bar.  No idea what this means or what other bits
             * might be set.
             *
             * SC_MOUSEMENU has sub-class event bits 3, at least.  No idea
             * what those mean.  0xf090 appears to be for the menu you can
             * bring down from the upper left.
             */
            switch ( wParam & ~0xf )
            {
            case SC_SIZE: case SC_MOVE:
                break;

            case SC_MINIMIZE:
                /* User has clicked on the "-" to minimize the window */
                /* Turn off the visibility */
                window->State.Visible = GL_FALSE;

                break;

            case SC_MAXIMIZE: case SC_NEXTWINDOW: case SC_PREVWINDOW:
                break;

            case SC_CLOSE:
                /* Followed very closely by a WM_CLOSE message */
                break;

            case SC_VSCROLL:
            //printf("vscroll\n");
            //fflush(stdout);
                break;
            case SC_HSCROLL:
            //printf("Hscroll\n");
            //fflush(stdout);
                break;
            case SC_MOUSEMENU:
            case SC_KEYMENU: case SC_ARRANGE: case SC_RESTORE:
            case SC_TASKLIST: case SC_SCREENSAVE: case SC_HOTKEY:
                break;

            default:
                ogWarning(
                    "Unknown wParam type %d (0x%x)", wParam, wParam
                );
                break;
            }
#endif

        /* We need to pass the message on to the operating system as well */
        lRet = DefWindowProc( hWnd, uMsg, wParam, lParam );
        break;

    default:
        /* Handle unhandled messages */
        lRet = DefWindowProc( hWnd, uMsg, wParam, lParam );
        break;
    }

    return lRet;
}
#endif
