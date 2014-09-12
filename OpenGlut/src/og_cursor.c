/*!
    \file  og_cursor.c
    \brief Mouse cursor
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

#if TARGET_HOST_UNIX_X11
  #include <X11/cursorfont.h>
#endif


/* -- INTERNAL FUNCTIONS --------------------------------------------------- */

#if TARGET_HOST_UNIX_X11

static int ogGetCursorError( const Cursor cursor )
{
    int ret = 0;
    char buf[ 256 ];

    switch( cursor )
    {
    case BadAlloc:
    case BadFont:
    case BadMatch:
    case BadPixmap:
    case BadValue:
        XGetErrorText( ogDisplay.Display, cursor, buf, sizeof buf );
        ogWarning( "Error in setting cursor:\n %s.", buf );
        ret = cursor;
        break;
    default:
        /* no error */
        break;
    }

    return ret;
}

#endif


/* -- INTERFACE FUNCTIONS -------------------------------------------------- */

/*!
    \fn
    \brief    Set the cursor image to be used for the current window
    \ingroup  window
    \param    cursorID   Name of desired cursor.

              For the <i>current window</i>, sets the mouse-cursor to
              one of a set of predefined images.  The GLUT symbolic constant
              IDs are:

               - \a GLUT_CURSOR_RIGHT_ARROW
               - \a GLUT_CURSOR_LEFT_ARROW
               - \a GLUT_CURSOR_INFO
               - \a GLUT_CURSOR_DESTROY
               - \a GLUT_CURSOR_HELP
               - \a GLUT_CURSOR_CYCLE
               - \a GLUT_CURSOR_SPRAY
               - \a GLUT_CURSOR_WAIT
               - \a GLUT_CURSOR_TEXT
               - \a GLUT_CURSOR_CROSSHAIR
               - \a GLUT_CURSOR_UP_DOWN
               - \a GLUT_CURSOR_LEFT_RIGHT
               - \a GLUT_CURSOR_TOP_SIDE
               - \a GLUT_CURSOR_BOTTOM_SIDE
               - \a GLUT_CURSOR_LEFT_SIDE
               - \a GLUT_CURSOR_RIGHT_SIDE
               - \a GLUT_CURSOR_TOP_LEFT_CORNER
               - \a GLUT_CURSOR_TOP_RIGHT_CORNER
               - \a GLUT_CURSOR_BOTTOM_RIGHT_CORNER
               - \a GLUT_CURSOR_BOTTOM_LEFT_CORNER

              Additionally, there are the following special cases:

              \a GLUT_CURSOR_FULL_CROSSHAIR This cursor, where supported,
              draws a crosshair the full width and height of the display.
              It may be mapped by OpenGLUT to the \a GLUT_CURSOR_CROSSHAIR,
              however.

              \a GLUT_CURSOR_NONE Turn the mouse cursor invisibile.

              \a GLUT_CURSOR_INHERIT Take the cursor that the parent
              window provides.


    \note     The X branch of OpenGLUT does not do thorough error checking.
    \note     The X branch of OpenGLUT always converts \a FULL_CROSSHAIR
              to \a CROSSHAIR.
              This is acceptable, but if a host system supports a fullscreen
              crosshair, it would be nice to support that.
    \note     Out of range \a cursorID values generate warnings.
    \note     Has no visible effect if the <i>current window</i> is
              of type \a GLUT_OFFSCREEN .
    \bug      Some \a cursorID values are not yet supported on WIN32.
*/
void OGAPIENTRY glutSetCursor( int cursorID )
{
    int error = 0;
    freeglut_assert_ready;
    freeglut_assert_window;

    if( GL_FALSE == ogStructure.Window->State.IsOffscreen )
    {
#if TARGET_HOST_UNIX_X11
        Cursor cursor = None;
        Pixmap no_cursor = None ;  /* Used for GLUT_CURSOR_NONE */

#define MAP_CURSOR(a,b)                                     \
    case a:                                                 \
        cursor = XCreateFontCursor( ogDisplay.Display, b ); \
        break;

        if( GLUT_CURSOR_FULL_CROSSHAIR == cursorID )
            cursorID = GLUT_CURSOR_CROSSHAIR;

        switch( cursorID )
        {
            MAP_CURSOR( GLUT_CURSOR_RIGHT_ARROW, XC_right_ptr);
            MAP_CURSOR( GLUT_CURSOR_LEFT_ARROW,  XC_left_ptr);
            MAP_CURSOR( GLUT_CURSOR_INFO,        XC_hand1);
            MAP_CURSOR( GLUT_CURSOR_DESTROY,     XC_pirate);
            MAP_CURSOR( GLUT_CURSOR_HELP,        XC_question_arrow);
            MAP_CURSOR( GLUT_CURSOR_CYCLE,       XC_exchange);
            MAP_CURSOR( GLUT_CURSOR_SPRAY,       XC_spraycan);
            MAP_CURSOR( GLUT_CURSOR_WAIT,        XC_watch);
            MAP_CURSOR( GLUT_CURSOR_TEXT,        XC_xterm);
            MAP_CURSOR( GLUT_CURSOR_CROSSHAIR,   XC_crosshair);
            MAP_CURSOR( GLUT_CURSOR_UP_DOWN,     XC_sb_v_double_arrow);
            MAP_CURSOR( GLUT_CURSOR_LEFT_RIGHT,  XC_sb_h_double_arrow);
            MAP_CURSOR( GLUT_CURSOR_TOP_SIDE,    XC_top_side);
            MAP_CURSOR( GLUT_CURSOR_BOTTOM_SIDE, XC_bottom_side);
            MAP_CURSOR( GLUT_CURSOR_LEFT_SIDE,   XC_left_side);
            MAP_CURSOR( GLUT_CURSOR_RIGHT_SIDE,  XC_right_side);
            MAP_CURSOR( GLUT_CURSOR_TOP_LEFT_CORNER,     XC_top_left_corner);
            MAP_CURSOR( GLUT_CURSOR_TOP_RIGHT_CORNER,    XC_top_right_corner);
            MAP_CURSOR( GLUT_CURSOR_BOTTOM_RIGHT_CORNER,
                        XC_bottom_right_corner);
            MAP_CURSOR( GLUT_CURSOR_BOTTOM_LEFT_CORNER, XC_bottom_left_corner);
            /* MAP_CURSOR( GLUT_CURSOR_NONE,        XC_bogosity); */

        case GLUT_CURSOR_NONE:
        {
            /*
             * Note that we *never* change {no_cursor_bits} from anything
             * but all-zeros.  It is our image and mask.  We also apparently
             * need to pick a color for foreground/background---but what
             * one we pick doesn't matter for GLUT_CURSOR_NONE.
             */
            static char no_cursor_bits[ 32 ];
            XColor black;
            no_cursor = XCreatePixmapFromBitmapData( ogDisplay.Display,
                                                     ogDisplay.RootWindow,
                                                     no_cursor_bits,
                                                     16, 16,
                                                     1, 0, 1 );
            XParseColor( ogDisplay.Display,
                         DefaultColormap( ogDisplay.Display,
                                          DefaultScreen( ogDisplay.Display ) ),
                         "black",
                         &black );
            cursor = XCreatePixmapCursor( ogDisplay.Display,
                                          no_cursor, no_cursor,
                                          &black, &black,
                                          0, 0 );
            break;
        }

        case GLUT_CURSOR_INHERIT:
            break;

        default:
            ogWarning( "Unknown cursor type: %d", cursorID );
            return;
        }

        error = ogGetCursorError( cursor );

        if( GLUT_CURSOR_INHERIT == cursorID )
            XUndefineCursor( ogDisplay.Display,
                             ogStructure.Window->Window.Handle );
        else
        {
            XDefineCursor( ogDisplay.Display,
                           ogStructure.Window->Window.Handle, cursor );
            XFreeCursor( ogDisplay.Display, cursor );
            if( GLUT_CURSOR_NONE == cursorID )
                XFreePixmap( ogDisplay.Display, no_cursor );
        }

#elif TARGET_HOST_WIN32 || TARGET_HOST_WINCE

    /* This is a temporary solution only... */
    /* Set the cursor AND change it for this window class. */
#       define MAP_CURSOR(a,b)                                   \
        case a:                                                  \
            SetCursor( LoadCursor( NULL, b ) );                  \
            SetClassLong( ogStructure.Window->Window.Handle,     \
                          GCL_HCURSOR,                           \
                          ( LONG )LoadCursor( NULL, b ) );       \
        break;

    /* Nuke the cursor AND change it for this window class. */
#       define ZAP_CURSOR(a,b)                                   \
        case a:                                                  \
            SetCursor( NULL );                                   \
            SetClassLong( ogStructure.Window->Window.Handle,     \
                          GCL_HCURSOR, ( LONG )NULL );           \
        break;

        switch( cursorID )
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
            /* MAP_CURSOR( GLUT_CURSOR_NONE,        IDC_NO        ); */
            ZAP_CURSOR( GLUT_CURSOR_NONE,        NULL           );

        default:
            MAP_CURSOR( GLUT_CURSOR_UP_DOWN,     IDC_ARROW     );
        }
#endif
    }
    if( !error )
        ogStructure.Window->State.Cursor = cursorID;
}

/*!
    \fn
    \brief    Moves the mouse pointer to given window coordinates.
    \ingroup  window
    \param    x        Window X coord for mouse.
    \param    y        Window Y coord for mouse.

              glutWarpPointer() moves the mouse pointer to window-relative
              coordinates given by \a x and \a y.

    \note     \a x and \a y are relative to current window.
    \note     Not applicable for \a GLUT_OFFSCREEN windows.
    \note     Warping means moving, just as if the user had manually
              moved the mouse.  This can generate mouse-motion callbacks.
              If your callback then moves the pointer again, you may
              end up in an endless loop.  There is some discussion about
              changing this, but at present this is just a caveat for
              you, the user, to be aware of.

*/
void OGAPIENTRY glutWarpPointer( int x, int y )
{
    freeglut_assert_ready;
    freeglut_assert_window;

    if( GL_FALSE == ogStructure.Window->State.IsOffscreen )
    {
#if TARGET_HOST_UNIX_X11

        XWarpPointer(
            ogDisplay.Display,
            None,
            ogStructure.Window->Window.Handle,
            0, 0, 0, 0,
            x, y
        );
        XFlush( ogDisplay.Display ); /*! \note Ideally, shouldn't XFlush() */

#elif TARGET_HOST_WIN32 || TARGET_HOST_WINCE

        POINT coords;
        coords.x = x;
        coords.y = y;

        /* ClientToScreen() translates {coords} for us. */
        ClientToScreen( ogStructure.Window->Window.Handle, &coords );
        SetCursorPos( coords.x, coords.y );

#endif
    }
}
