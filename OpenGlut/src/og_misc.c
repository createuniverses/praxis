/*!
    \file  og_misc.c
    \brief Miscellaneous functions
*/

/*
 * Portions copyright (C) 2004, the OpenGLUT project contributors.
 * OpenGLUT branched from freeglut in February, 2004.
 *
 * Copyright (c) 1999-2000 Pawel W. Olszta. All Rights Reserved.
 * Written by Pawel W. Olszta, <olszta@sourceforge.net>
 * Creation date: Thu Dec 9 1999
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

/* -- INTERNAL FUNCTIONS --------------------------------------------------- */

/*
 * Similar to the de facto strdup(), but since strdup() is not
 * actually a standard C function, we provide our own.
 */
char *ogStrDup( const char *str )
{
    char *ret = malloc( strlen( str ) + 1 );
    char *dst = ret;
    if( ret )
        while( *dst++ = *str++ )
            ;
    return ret;
}

/* -- INTERFACE FUNCTIONS -------------------------------------------------- */

/*!
    \fn
    \brief    Determine if an OpenGL extension is available.
    \ingroup  opengl
    \param    extension    A string-name of an extension.

              Returns 0 if OpenGLUT cannot determine that the requested
              extension is definitely present.  Only checks extensions
              from glGetString().

    \todo     Wouldn't this be simpler and clearer if we used strtok()?
    \todo     Consider an ogWarning() if there is no current rendering
              context.
    \todo     This is an ugly bit of code with 3 return statements,
              one of which is never reached.
    \see      glGetString()
*/
int OGAPIENTRY glutExtensionSupported( const char *extension )
{
    const char *extensions, *start;
    const int len = strlen( extension );

    freeglut_assert_ready;
    /* Check for current window and thus a current context */
    if( !ogStructure.Window )
        return 0;

    if( strchr( extension, ' ' ) )
        return 0;
    start = extensions = ( const char * )glGetString( GL_EXTENSIONS );

    if( !extensions )
        return 0;

    while( 1 )
    {
        const char *p = strstr( extensions, extension );
        if (!p)
            return 0;

        /* Check that we matched at a word boundary */
        if( ( p == start || ' ' == p[ -1 ] ) &&
            ( ' ' == p[ len ] || 0 == p[ len ] ) )
            return 1;
        /* skip the false match and continue */
        extensions = p + len;
    }

    return 0;
}

/*!
    \fn
    \brief    Reports all available OpenGL errors.
    \ingroup  opengl

              Displays as an OpenGLUT warning every OpenGL error
              that OpenGL remembers giving to us and which
              we have not processed.  Uses gluErrorString().

              This is forcibly done by OpenGLUT periodically if
              "-gldebug" is one of the strings passed into
              glutInit() via \argv.

    \see      gluErrorString(), glutInit()
*/
void OGAPIENTRY glutReportErrors( void )
{
    GLenum error;
    while( ( error = glGetError( ) ) != GL_NO_ERROR )
        ogWarning( "GL error: %s", gluErrorString( error ) );
}

/*!
    \fn
    \brief    Set autorepeat status.
    \ingroup  inputstate
    \param    ignore    Whether to ignore autorepeated keys.

              If \a ignore is non-zero, then auto-repeat is
              disabled for keyboard callbacks for the
              <i>current window</i>.

    \see      glutSetKeyRepeat()
*/
void OGAPIENTRY glutIgnoreKeyRepeat( int ignore )
{
    freeglut_assert_ready;
    freeglut_assert_window;

    ogStructure.Window->State.IgnoreKeyRepeat = ignore ? GL_TRUE : GL_FALSE;
}

/*!
    \fn
    \brief    Sets autorepeat behavior for all OpenGLUT windows.
    \ingroup  inputstate
    \param    repeatMode    On, Off or Default.

              glutSetKeyRepeat() is similar to glutIgnoreKeyRepeat()
              but sets the behavior
              for OpenGLUT in general, rather than for a particular
              window.  The options for \a repeatMode are:

               - \a GLUT_KEY_REPEAT_OFF     \n Turn off repeat for all windows.
               - \a GLUT_KEY_REPEAT_ON      \n Turn on repeat for all windows.
               - \a GLUT_KEY_REPEAT_DEFAULT \n Respect the window's setting.

    \see      glutIgnoreKeyRepeat()
*/
void OGAPIENTRY glutSetKeyRepeat( int repeatMode )
{
    freeglut_assert_ready;

    switch( repeatMode )
    {
    case GLUT_KEY_REPEAT_OFF:
    case GLUT_KEY_REPEAT_ON:
        ogState.KeyRepeat = repeatMode;
        break;

    case GLUT_KEY_REPEAT_DEFAULT:
        ogState.KeyRepeat = GLUT_KEY_REPEAT_ON;
        break;

    default:
        ogError( "Invalid glutSetKeyRepeat mode: %d", repeatMode );
        break;
    }
}

/*!
    \fn
    \brief    Forces a joystick poll and callback.
    \ingroup  input

              Forces the OpenGLUT joystick code to poll your
              joystick(s) and to call your joystick callbacks
              with the result.  The operation completes, including
              callbacks, before glutForceJoystickFunc() returns.

    \bug      The original WINCE import used a #if to turn this function
              off.  That is wrong.  The proper way to handle the joystick
              code is to generate "null" events.  The WINCE code needs
              to be fixed in og_joystick.c, not here.
    \see      glutJoystickFunc()
*/
void OGAPIENTRY glutForceJoystickFunc( void )
{
    freeglut_assert_ready;
    if( ogStructure.Window && FETCH_WCB( *( ogStructure.Window ), Joystick ) )
        ogJoystickPollWindow( ogStructure.Window );
}

/*!
    \fn
    \brief    Sets an indexed color-mode entry.
    \ingroup  colormap
    \param    nColor    The palette entry to change.
    \param    red       New red value for palette entry.
    \param    green     New green value for palette entry.
    \param    blue      New blue value for palette entry.

              glutSetCursor() allows you to set individual color-map entries
              in a \a GLUT_INDEX type of display.  Respects the
              current overlay setting.

    \bug      Unimplemented.
    \see      glutGetColor(), glutCopyColorMap()
*/
void OGAPIENTRY glutSetColor(
    int nColor, GLfloat red, GLfloat green, GLfloat blue
)
{
}

/*!
    \fn
    \brief    Gets an indexed color-mode entry's Red, Green, or Blue value.
    \ingroup  colormap
    \param    color     The palette entry to fetch.
    \param    component Whether to fetch Red, Green, or Blue.

              Allows you to get individual color-map entries
              in a \a GLUT_INDEX type of display.  Respects the
              current layer setting.

              \a component may be any of:

               - \a GLUT_RED
               - \a GLUT_GREEN
               - \a GLUT_BLUE

    \bug      Unimplemented.
    \see      glutSetColor(), glutCopyColorMap()
*/
GLfloat OGAPIENTRY glutGetColor( int color, int component )
{
    return 0.0f;
}

/*!
    \fn
    \brief    Copies a color map between windows.
    \ingroup  colormap
    \param    window    The window to copy <i>to</i>

              Allows you to copy an entire color map from
              one window to another.  This function copies
              <b>from</b> the <i>current window</i>.  It
              copies <b>to</b> the indicated \a window.
              Respects the current layer setting.

    \bug      Unimplemented.
    \see      glutSetColor(), glutGetColor()
*/
void OGAPIENTRY glutCopyColormap( int window )
{
}
