/*!
    \file  og_videoresize.c
    \brief Video resizing
*/

/*
 * Portions copyright 2004, the OpenGLUT project contributors.
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


/* -- INTERFACE FUNCTIONS -------------------------------------------------- */

/*!
    \fn
    \brief    Undocumented, unimplemented.
    \ingroup  videomode
    \param    eWhat    An enumerated tag.

              This function is undocumented.  This function is
              unimplemeneted (presently).

              From GLUT 3.7's implementation, the following is a
              list of symbols that GLUT 3.7 supported.  These are
              not necessarily defined in OpenGLUT at this time,
              nor is support presently available for them:

               - \a GLUT_VIDEO_RESIZE_POSSIBLE (feature presence)
               - \a GLUT_VIDEO_RESIZE_IN_USE
               - \a GLUT_VIDEO_RESIZE_X_DELTA
               - \a GLUT_VIDEO_RESIZE_Y_DELTA
               - \a GLUT_VIDEO_RESIZE_WIDTH_DELTA
               - \a GLUT_VIDEORESIZE_HEIGHT_DELTA
               - \a GLUT_VIDEO_REISZE_X
               - \a GLUT_VIDEO_RESIZE_Y
               - \a GLUT_VIDEO_RESIZE_WIDTH
               - \a GLUT_VIDEO_RESIZE_HEIGHT

              A -1 should be returned for unimplemented features, and
              a warning printed.

    \see      glutVideoResizeGet(), glutSetupVideoResizing(),
              glutStopVideoResizing(), glutVideoResize(), glutVideoPan()
*/
int  OGAPIENTRY glutVideoResizeGet( GLenum eWhat )
{
    ogWarning( "glutVideoResizeGet(): Unimplemented." );
    return -1;
}

/*!
    \fn
    \brief    Undocumented, unimplemented.
    \ingroup  videomode

              This function is undocumented.  This function is
              unimplemeneted (presently).

    \see      glutVideoResizeGet(), glutSetupVideoResizing(),
              glutStopVideoResizing(), glutVideoResize(), glutVideoPan()
*/
void OGAPIENTRY glutSetupVideoResizing( void )
{
}

/*!
    \fn
    \brief    Undocumented, unimplemented.
    \ingroup  videomode

              This function is undocumented.  This function is
              unimplemeneted (presently).

    \see      glutVideoResizeGet(), glutSetupVideoResizing(),
              glutStopVideoResizing(), glutVideoResize(), glutVideoPan()
*/
void OGAPIENTRY glutStopVideoResizing( void )
{
}

/*!
    \fn
    \brief    Undocumented, unimplemented.
    \ingroup  videomode
    \param    x    A horizontal position.
    \param    y    A vertical position.
    \param    w    A width.
    \param    h    A height.

              This function is undocumented.  This function is
              unimplemeneted (presently).

              This function appears to allow setting the video
              display rectangle to a sub-rectangle of the
              hardware buffer.  \a x and \a y are apparently the
              upper left corner, and \a w and \a h are the width
              and height of the rectangle.

              It is not clear what should be done if the values
              are invalid (negative \a x, for example).

    \see      glutVideoResizeGet(), glutSetupVideoResizing(),
              glutStopVideoResizing(), glutVideoResize(), glutVideoPan()
*/
void OGAPIENTRY glutVideoResize( int x, int y, int w, int h )
{
}

/*!
    \fn
    \brief    Undocumented, unimplemented.
    \ingroup  videomode
    \param    x    A horizontal position.
    \param    y    A vertical position.
    \param    w    A width.
    \param    h    A height.

              This function is undocumented.  This function is
              unimplemeneted (presently).

              Appears to be for moving the video display about
              (panning) in a much larger graphic area than
              will fit on the screen at current resolution.

    \see      glutVideoResizeGet(), glutSetupVideoResizing(),
              glutStopVideoResizing(), glutVideoResize(), glutVideoPan()
*/
void OGAPIENTRY glutVideoPan( int x, int y, int w, int h )
{
}






