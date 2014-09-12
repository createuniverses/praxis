/*!
    \file  og_overlay.c
    \brief Overlay management
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


/* -- INTERFACE FUNCTIONS -------------------------------------------------- */

/*!
    \fn
    \brief    Creates an overlay.
    \ingroup  overlays

              Creates an overlay associated with the <i>current window</i>.

    \note     Unimplemented.
    \note     Old GLUT would terminate the program when an overlay
              was not possible.  freeglut ignores the request;
              OpenGLUT ignores the request as well.
    \see      glutRemoveOverlay(),
              glutUseLayer(), glutPostOverlayRedisplay(),
              glutPostWindowOverlayRedisplay(),
              glutShowOverlay(), glutHideOverlay()
*/
void OGAPIENTRY glutEstablishOverlay( void )
{
}

/*!
    \fn
    \brief    Removes an overlay.
    \ingroup  overlays

              Removes the overlay associated with the <i>current window</i>.

    \note     Unimplemented.
    \see      glutEstablishOverlay(),
              glutUseLayer(), glutPostOverlayRedisplay(),
              glutPostWindowOverlayRedisplay(),
              glutShowOverlay(), glutHideOverlay()
*/
void OGAPIENTRY glutRemoveOverlay( void )
{
}

/*!
    \fn
    \brief    Allows you to switch between normal and layer mode.
    \ingroup  overlays
    \param    layer    Whether to be in a layer or in the normal window.

              By default, OpenGLUT operates in "normal" mode, with
              respect to layers.  If you have a layer open and wish
              to operate on the layer, you must use glutUseLayer().

              \a layer can take on the following values, indicating
              the layer mode to use:

               - \a GLUT_NORMAL
               - \a GLUT_LAYER

    \note     Unimplemented.
    \note     It is unclear what the consequences are if you are in
              \a GLUT_OVERLAY mode and switch to another (or the same)
              window via glutSetWindow().  What if the target has
              a layer?  What if it doesn't?
    \see      glutEstablishOverlay(), glutRemoveOverlay(),
              glutPostOverlayRedisplay(),
              glutPostWindowOverlayRedisplay(),
              glutShowOverlay(), glutHideOverlay()
*/
void OGAPIENTRY glutUseLayer( GLenum layer )
{
}

/*!
    \fn
    \brief    Posts a redispaly against the layer for the current window.
    \ingroup  overlays

              For the <i>current window</i>, tells OpenGLUT that you
              wish to have your OverlayDisplay callback invoked.

    \note     Unimplemented.
    \see      glutEstablishOverlay(), glutRemoveOverlay(),
              glutUseLayer(),
              glutPostWindowOverlayRedisplay(),
              glutShowOverlay(), glutHideOverlay(), glutOverlayDisplayFunc(),
              glutPostRedisplay()
*/
void OGAPIENTRY glutPostOverlayRedisplay( void )
{
}

/*!
    \fn
    \brief    Posts a redisplay to the indicated window's layer.
    \ingroup  overlays
    \param    ID    A window ID.

              This function lets you post an overlay update to the overlay
              of any arbitrary window.

    \note     Unimplemented.
    \see      glutEstablishOverlay(), glutRemoveOverlay(),
              glutUseLayer(), glutPostOverlayRedisplay(),
              glutShowOverlay(), glutHideOverlay(), glutOverlayDisplayFunc(),
              glutPostRedisplay()
*/
void OGAPIENTRY glutPostWindowOverlayRedisplay( int ID )
{
}

/*!
    \fn
    \brief    Make an overlay visible.
    \ingroup  overlays

              Causes a previously-hidden overlay to become
              apparent.  Applies to the <i>current window</i>.

    \note     Unimplemented.
    \see      glutEstablishOverlay(), glutRemoveOverlay(),
              glutUseLayer(), glutPostOverlayRedisplay(),
              glutPostWindowOverlayRedisplay(),
              glutHideOverlay()
*/
void OGAPIENTRY glutShowOverlay( void )
{
}

/*!
    \fn
    \brief    Make an overlay invisible.
    \ingroup  overlays

              Causes a visible overlay to become invisible.
              Applies to the <i>current window</i>.

    \note     Unimplemented.
    \see      glutEstablishOverlay(), glutRemoveOverlay(),
              glutUseLayer(), glutPostOverlayRedisplay(),
              glutPostWindowOverlayRedisplay(),
              glutShowOverlay()
*/
void OGAPIENTRY glutHideOverlay( void )
{
}
