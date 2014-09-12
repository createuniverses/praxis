/*!
    \file  og_display.c
    \brief Display message posting, context buffer swapping.
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


/* -- INTERFACE FUNCTIONS -------------------------------------------------- */

/*!
    \fn
    \brief    Mark the current window as needing a redisplay.
    \ingroup  window

              Whenever circumstances indicate that your window is
              in need of being redisplayed, you may call glutPostRedisplay()
              to tell OpenGLUT that you want to redraw your graphics.
              Multiple calls to this function may be coalesced by OpenGLUT
              to avoid excessive invocation of your drawing support.

              The ultimate effect of this function is to call your
              Display callback for the <i>current window</i>.

    \see      glutPostWindowRedisplay(), glutPostOverlayRedisplay(),
              glutPostWindowOverlayRedisplay(), glutSwapBuffers(),
              glutDisplayFunc()
*/
void OGAPIENTRY glutPostRedisplay( void )
{
    freeglut_assert_ready;
    freeglut_assert_window;
    ogStructure.Window->State.Redisplay = GL_TRUE;
}

/*!
    \fn
    \brief    Swaps the buffers for the current window.
    \ingroup  window

              This function signals to OpenGLUT that you are done drawing
              to the <i>current window</i> for now.  If your window is
              double-buffered (\a GLUT_DOUBLE param to glutInitDisplayMode()),
              then OpenGLUT will swap the front buffer with the back buffer.

              This also computes your current frame-rate and prints the result
              on \a stderr if indicated by the \a GLUT_FPS environment
              variable.  The computed value is not necessarily the
              total frame rate, if you have multiple windows, as the
              statistic is the total number of buffer-swaps for the
              entire program.

    \note     This function has no effect if your window is \a GLUT_SINGLE .
    \note     Frame rate is only calculated for double-buffered windows.
    \todo     How does this interact with overlays?
    \todo     Consider making \a GLUT_FPS keep per-window stats in a
              multi-window program.
    \see      glutPostRedisplay(), glutPostOverlayRedisplay(),
              glutPostWindowRedisplay(), glutPostWindowOverlayRedisplay(),
              glutInitDisplaymode()
*/
void OGAPIENTRY glutSwapBuffers( void )
{
    freeglut_assert_ready;
    freeglut_assert_window;

    glFlush( );
    if( ! ogStructure.Window->Window.DoubleBuffered )
        return;

#if TARGET_HOST_UNIX_X11
    glXSwapBuffers( ogDisplay.Display, ogStructure.Window->Window.Handle );
#elif TARGET_HOST_WIN32 || TARGET_HOST_WINCE
    if( !( ogStructure.Window->State.IsOffscreen ) )
        SwapBuffers( ogStructure.Window->Window.Device );
#endif

    /* GLUT_FPS env var support */
    if( ogState.FPSInterval )
    {
        GLint t = glutGet( GLUT_ELAPSED_TIME );
        ogState.SwapCount++;
        if( ogState.SwapTime == 0 )
            ogState.SwapTime = t;
        else if( t - ogState.SwapTime > ogState.FPSInterval )
        {
            float time = 0.001f * ( t - ogState.SwapTime );
            float fps = ( float )ogState.SwapCount / time;
            fprintf( stderr,
                     "OpenGLUT: %d frames in %.2f seconds = %.2f FPS\n",
                     ogState.SwapCount, time, fps );
            ogState.SwapTime = t;
            ogState.SwapCount = 0;
        }
    }
}

/*!
    \fn
    \brief    Mark an indicated window as needing a redisplay.
    \ingroup  window
    \param    windowID    The OpenGLUT window id to be affected.

              Similar to glutPostRedisplay(), except that instead
              of affecting the <i>current window</i>, this function
              affects an arbitrary window, indicated by the
              \a windowID parameter.

    \see      glutPostRedisplay(), glutPostOverlayRedisplay(),
              glutPostWindowOverlayRedisplay(), glutSwapBuffers(),
              glutDisplayFunc(), glutCreateWindow(), glutCreateSubWindow()
*/
void OGAPIENTRY glutPostWindowRedisplay( int windowID )
{
    SOG_Window *window;

    freeglut_assert_ready;
    window = ogWindowByID( windowID );
    if( window )
        window->State.Redisplay = GL_TRUE;
}
