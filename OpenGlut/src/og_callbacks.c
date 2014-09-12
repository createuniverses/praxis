/*!
    \file  og_callbacks.c
    \brief Callback configuration
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

/*
 * This function is used by glutVisibilityFunc() as a callback for
 * another function.
 */
static void oghVisibility( const int status )
{
    int glut_status = GLUT_VISIBLE;

    freeglut_assert_ready;
    if( !ogStructure.Window )
        return;

    if( ( GLUT_HIDDEN == status )  || ( GLUT_FULLY_COVERED == status ) )
        glut_status = GLUT_NOT_VISIBLE;
    INVOKE_WCB( *( ogStructure.Window ), Visibility, ( glut_status ) );
}


/*
 * All of the callbacks setting methods can be generalized to this:
 */
#define SET_CALLBACK(a)                                  \
do                                                       \
{                                                        \
    if( ogStructure.Window == NULL )                     \
        return;                                          \
    SET_WCB( ( *( ogStructure.Window ) ), a, callback ); \
} while( 0 )


/* -- INTERFACE FUNCTIONS -------------------------------------------------- */


/*!
    \fn
    \brief    Sets the Display callback for the current window.
    \ingroup  windowcallback
    \param    callback    Client function for normal redisplay event.

              Sets the display callback for the <i>current window</i>.
              All windows, including subwindows, <b>must</b> have
              a display callback registered.  OpenGLUT will call
              the \a callback function whenever it thinks that the
              window may require updating.

              This callback is bound to the <i>current window</i>.

    \note     Unlike most callbacks, it is illegal to try to disable
              the display callback by setting it to \a NULL .
    \note     Multiple redisplays may be coalesced into a single
              event for invoking the \a callback only once.
    \see      glutPostRedisplay(), glutOverlayDisplayFunc()
*/
void OGAPIENTRY glutDisplayFunc( void( *callback )( void ) )
{
    if( !callback )
        ogError(
            "Fatal error in program.  NULL display callback not "
            "permitted in GLUT 3.0+, freeglut 2.0.1+, "
            "or any version of OpenGLUT."
        );
    SET_CALLBACK( Display );
}

/*!
    \fn
    \brief    Sets the Reshape callback for the current window.
    \ingroup  windowcallback
    \param    callback    Client function for reshape-window event.

              This registers a function with OpenGLUT, which OpenGLUT
              will invoke whenever the window is reshaped or
              resized.  Your callback is only invoked when the host
              window system has actually changed the window size.

              The parameters to your callback are the new width and
              height for your window.

              If you do not provide a reshape callback, OpenGLUT
              will simply call glViewport(0,0,\a w,\a h).

              This callback is bound to the <i>current window</i>.

              To ask OpenGLUT about the present dimensions of the
              <i>current window</i>, you can use glutGet().

    \note     Unlike other callbacks, GLUT has an <b>active</b>
              default behavior if you do not set this.   (Most
              event types passively do nothing if you do not
              specify a callback to handle them.)
    \note     The reshape callback should always be called, if
              registered, when your window is first created.
    \see      glutGet(), glutReshapeWindow()
*/
void OGAPIENTRY glutReshapeFunc( void( *callback )( int w, int h ) )
{
    SET_CALLBACK( Reshape );
}

/*!
    \fn
    \brief    Sets the Keyboard callback for the current window.
    \ingroup  input
    \param    callback    Client function for keyboard event.

              This callback registration allows you to handle
              traditional ASCII keyboard input.  A general rule of
              thumb is that if a key has a common ASCII code,
              then OpenGLUT assigns that code to the key
              and calls the Keyboard \a callback with the ASCII code in
              the \a key parameter.
              For other keys, you must use glutSpecialFunc().
              Not all keys can be reported by OpenGLUT.

              As a convenience, the mouse coordinates, relative
              to your window, are also returned.

              This callback is bound to the <i>current window</i>.

    \note     This function is not very international-friendly.
    \note     Windows created via glutCreateMenuWindow() always cascade
              keyboard and mouse events to their parent.
    \see      glutKeyboardUpFunc(), glutSpecialFunc()
*/
void OGAPIENTRY glutKeyboardFunc(
    void( * callback )( unsigned char key, int x, int y )
)
{
    if( !ogStructure.Window->IsClientMenu )
        SET_CALLBACK( Keyboard );
}

/*!
    \fn
    \brief    Sets the Special callback for the current window
    \ingroup  input
    \param    callback    Client function for keyboard event.

              Registers a \a callback for OpenGLUT to call
              when the user presses "special" keys on the keyboard.

              The special callback handles some additional keys that
              are not covered under plain "keyboard" events.
              The \a key that is passed to the \a callback is one of an
              enumerated set.
              The association to keys on your keyboard should
              be obvious.  Their GLUT symbol names are:

               - \a GLUT_KEY_F1
               - \a GLUT_KEY_F2
               - \a GLUT_KEY_F3
               - \a GLUT_KEY_F4
               - \a GLUT_KEY_F5
               - \a GLUT_KEY_F6
               - \a GLUT_KEY_F7
               - \a GLUT_KEY_F8
               - \a GLUT_KEY_F9
               - \a GLUT_KEY_F10
               - \a GLUT_KEY_F11
               - \a GLUT_KEY_F12
               - \a GLUT_KEY_LEFT
               - \a GLUT_KEY_UP
               - \a GLUT_KEY_RIGHT
               - \a GLUT_KEY_DOWN
               - \a GLUT_KEY_PAGE_UP
               - \a GLUT_KEY_PAGE_DOWN
               - \a GLUT_KEY_HOME
               - \a GLUT_KEY_END
               - \a GLUT_KEY_INSERT

              To receive other keys, see glutKeyboardFunc().

              This callback is bound to the <i>current window</i>.

    \note     Many keys are not included; nor is it possible
              to apply qualifiers such as the Shift or Ctrl key
              to these keys.
    \note     Windows created via glutCreateMenuWindow() always cascade
              keyboard and mouse events to their parent.
    \see      glutSpecialUpFunc(), glutKeyboardFunc()
*/
void OGAPIENTRY glutSpecialFunc( void( *callback )( int key, int x, int y ) )
{
    if( !ogStructure.Window->IsClientMenu )
        SET_CALLBACK( Special );
}

/*!
    \fn
    \brief    Sets the global idle callback.
    \ingroup  idletimer
    \param    callback    Client function for idle event.

              When OpenGLUT's glutMainLoop() is doing nothing else,
              it checks to see if an ``idle'' callback set.
              If so, OpenGLUT invokes that callback.

              This callback is <b>not</b> bound to any window.

    \note     There is at most <b>one</b> idle callback for your entire
              application.
    \see      glutTimerFunc(), glutMainLoop(), glutMainLoopEvent()
*/
void OGAPIENTRY glutIdleFunc( void( *callback )( void ) )
{
    OPENGLUT_REQUIRE_READY ("glutIdleFunc");
    ogState.IdleCallback = callback;
}

/*!
    \fn
    \brief    Sets the Timer callback for the current window.
    \ingroup  idletimer
    \param    msec        Milliseconds till invocation.
    \param    callback    Client function for timer event.
    \param    data        Arbitrary data; passed to \a callback .

              After <b>at least</b> \a msec milliseconds,
              OpenGLUT will call \a callback , passing in your
              user-supplied \a data parameter.  OpenGLUT will
              call your function only once.

              This callback is <b>not</b> bound to any window.

    \note     Unlike most other callbacks, timers only occur once.
    \note     Unlike most other callbacks, you cannot deregister a
              timer callback.
    \note     Unlike most other callbacks, you can register an
              arbitrary number of timers.
    \see      glutIdleFunc(), glutMainLoop(), glutMainLoopEvent()
*/
void OGAPIENTRY glutTimerFunc(
    unsigned int msec, void( *callback )( int data ), int data
)
{
    SOG_Timer *timer, *node;

    OPENGLUT_REQUIRE_READY ("glutTimerFunc");

    if( ( timer = ogState.FreeTimers.Last ) )
        ogListRemove( &ogState.FreeTimers, &timer->Node );
    else if( !( timer = malloc( sizeof( SOG_Timer ) ) ) )
        ogError(
            "Fatal error: "
            "Memory allocation failure in glutTimerFunc()\n"
        );

    timer->Callback    = callback;
    timer->ID          = data;
    timer->TriggerTime = ogElapsedTime() + msec;

    for( node = ogState.Timers.First; node; node = node->Node.Next )
        if( node->TriggerTime > timer->TriggerTime )
            break;

    ogListInsert( &ogState.Timers, &node->Node, &timer->Node );
}

/*!
    \fn
    \brief    Sets the Visibility callback for the current window.
    \ingroup  deprecated
    \param    callback    Client hook for visibility changes.

              OpenGLUT may call this function when your window's
              visbility status has changed.  \a status can take
              on two values: \a GLUT_NOT_VISIBLE or \a GLUT_VISIBLE .
              If any pixel of your window (including descendants) is
              visible, your window is \a GLUT_VISIBLE .

              The callback is bound to the <i>current window</i>.

    \note     This is not a polling mechanism.  You are only informed
              of transitions that OpenGLUT observes while your
              callback is in place.
    \note     This function appears to be superceded by
              glutWindowStatusFunc().
    \note     This callback is mutually exclusive of glutWindowStatusFunc().
    \see      glutWindowStatusFunc()
*/
void OGAPIENTRY glutVisibilityFunc( void( *callback )( int status ) )
{
    SET_CALLBACK( Visibility );

    if( callback )
        glutWindowStatusFunc( oghVisibility );
    else
        glutWindowStatusFunc( NULL );
}

/*!
    \fn
    \brief    Sets the keyboard key release callback for the current window.
    \ingroup  input
    \param    callback    Client hook for ASCII key releases.

              This function provides a way to detect the release of
              a keyboard key.
              The keys are reported exactly as with
              glutKeyboardFunc(), save that the \a callback registered
              via this function is used to report the event.

              This callback is bound to the <i>current window</i>.

    \note     Windows created via glutCreateMenuWindow() always cascade
              keyboard and mouse events to their parent.
    \see      glutKeyboardFunc(), glutSpecialUpFunc()
*/
void OGAPIENTRY glutKeyboardUpFunc(
    void( *callback )( unsigned char key, int x, int y )
)
{
    if( !ogStructure.Window->IsClientMenu )
        SET_CALLBACK( KeyboardUp );
}

/*!
    \fn
    \brief    Sets the special key release callback for the current window
    \ingroup  input
    \param    callback    Client hook for special key releases.

              This function provides a way to detect the release of
              a keyboard \a key.
              The keys are reported exactly as with
              glutSpecialFunc(), save that the \a callback registered
              via this function is used to report the event.

              This callback is bound to the <i>current window</i>.

    \note     Windows created via glutCreateMenuWindow() always cascade
              keyboard and mouse events to their parent.
    \see      glutSpecialFunc(), glutKeyboardUpFunc()
*/
void OGAPIENTRY glutSpecialUpFunc(
    void( *callback )( int key, int x, int y )
)
{
    if( !ogStructure.Window->IsClientMenu )
        SET_CALLBACK( SpecialUp );
}

/*!
    \fn
    \brief    Reports joystick state for the current window.
    \ingroup  input
    \param    callback      Client function for joystick events
    \param    pollInterval  Approximate (minimum) millisecond interval

              The callback is called roughly every \a pollinterval
              milliseconds, and will give the joystick status.

              The \a buttons bitmask is a bit-wise \a OR of:
              - GLUT_JOYSTICK_BUTTON_A
              - GLUT_JOYSTICK_BUTTON_B
              - GLUT_JOYSTICK_BUTTON_C
              - GLUT_JOYSTICK_BUTTON_D

              The axis values are in the range [-1000,1000].

*/
void OGAPIENTRY glutJoystickFunc(
    void( *callback )( unsigned int buttons, int xaxis, int yaxis, int zaxis ),
    int pollInterval
)
{
    ogJoystickInit( );

    SET_CALLBACK( Joystick );
    ogStructure.Window->State.JoystickPollRate = pollInterval;

    ogStructure.Window->State.JoystickLastPoll =
        ogElapsedTime() - ogStructure.Window->State.JoystickPollRate;

    if( ogStructure.Window->State.JoystickLastPoll < 0 )
        ogStructure.Window->State.JoystickLastPoll = 0;
}

/*!
    \fn
    \brief    Sets the mouse-button callback for the current window.
    \ingroup  input
    \param    callback    Client hook for mouse-buttons.

              Whenever a mouse button is pressed or released in an OpenGLUT
              window, OpenGLUT checks if that window has a mouse-button
              (Mouse) callback registered.  If so, OpenGLUT gives the
              event to the handler.  \a button is the button number,
              starting from 0.  \a state is \a GLUT_UP or \a GLUT_DOWN
              to indicate the button's new state.  The other parameters
              are the mouse coordinates.

              Mouse wheel motion can be reported as buttons.  If you
              do not request otherwise, a wheel spun forward will
              act like a button clicking down, immediately followed
              by clicking up.  Spinning the same wheel backward
              will act like a different button clicking.  Mouse wheel
              pseudo-buttons are added after all real buttons.

              While the button is held and the mouse is dragged,
              you receive mouse-motion events (glutMotionFunc()),
              even if the mouse is dragged out of the window.

              This callback is bound to the <i>current window</i>.

    \note     Reporting the wheel as buttons is actually inherited from X.
              freeglut added code to support this on WIN32.  OpenGLUT
              inherited that support from freeglut.
    \note     Old GLUT defines the symbols \a GLUT_LEFT_BUTTON,
              \a GLUT_RIGHT_BUTTON, and \a GLUT_MIDDLE_BUTTON.
              However, mice can have more than 3 buttons, so these
              symbols are deprecated.
    \note     Windows created via glutCreateMenuWindow() always cascade
              keyboard and mouse events to their parent.
    \see      glutMotionFunc(), glutPassiveMotionFunc(), glutMouseWheelFunc()
*/
void OGAPIENTRY glutMouseFunc(
    void( *callback )( int button, int state, int x, int y )
)
{
    if( !ogStructure.Window->IsClientMenu )
        SET_CALLBACK( Mouse );
}

/*!
    \fn
    \brief    Sets the mouse wheel callback for the current window.
    \ingroup  input
    \param    callback    Client hook for mouse wheel events.

              If the mouse wheel is spun over your (sub)window,
              OpenGLUT will, in theory, report this via the MouseWheel
              callback.  \a wheel is the wheel number, \a direction
              is +/- 1, and \a x and \a y are the mouse coordinates.

              If you do not register a wheel callback, wheel events will
              be reported as mouse buttons.

              This callback is bound to the <i>current window</i>.

    \note     Due to lack of information about the mouse, it is impossible
              to implement this correctly on X at this time.  Use of this
              function limits the portability of your application.  (This
              feature <b>does</b> work on X, just not reliably.)  You are
              encouraged to use the standard, reliable mouse-button
              reporting, rather than wheel events.
    \note     Windows created via glutCreateMenuWindow() always cascade
              keyboard and mouse events to their parent.
    \see      glutMouseFunc()
*/
void OGAPIENTRY glutMouseWheelFunc(
    void( *callback )( int wheel, int direction, int x, int y )
)
{
    if( !ogStructure.Window->IsClientMenu )
        SET_CALLBACK( MouseWheel );
}

/*!
    \fn
    \brief    Reports mouse-motion while a button is held.
    \ingroup  input
    \param    callback    Client hook for dragging mouse.

              This function reports the mouse position when the mouse
              is dragged starting from within your window.
              (``Dragging'' occurs when you press one or more mouse
               buttons in one of your OpenGLUT windows, and then
               move the mouse around.)

              This callback is bound to the <i>current window</i>.

    \note     Events are reported until the mouse button is released,
              even if the mouse leaves the window.
    \note     Windows created via glutCreateMenuWindow() always cascade
              keyboard and mouse events to their parent.
    \see      glutMouseFunc(), glutPassiveMotion()
*/
void OGAPIENTRY glutMotionFunc( void( *callback )( int x, int y ) )
{
    if( !ogStructure.Window->IsClientMenu )
        SET_CALLBACK( Motion );
}

/*!
    \fn
    \brief    Sets the non-dragging (gliding?) mouse-motion callback.
    \ingroup  input
    \param    callback    Client mouse-glide hook.

              If you set this callback on a window, then every time that
              OpenGLUT detects a change in the mouse position inside
              that window, with no buttons pressed on that mouse,
              OpenGLUT will invoke \a callback with the window
              coordinates of the mouse.

              This callback is bound to the <i>current window</i>.

    \note     Windows created via glutCreateMenuWindow() always cascade
              keyboard and mouse events to their parent.
    \see      glutMotionFunc(), glutMouseFunc(), glutEntryFunc()
*/
void OGAPIENTRY glutPassiveMotionFunc( void( *callback )( int x, int y ) )
{
    if( !ogStructure.Window->IsClientMenu )
        SET_CALLBACK( Passive );
}

/*!
    \fn
    \brief    Window mouse entry/leave callback.
    \ingroup  input
    \param    callback    Client window-entry hook.

              When the mouse enters or exits a window (or sub-window),
              OpenGLUT can report this transition via the Entry
              callback.  \a state is one of \a GLUT_LEFT or
              \a GLUT_ENTERED respective of whether the mouse
              left or entered the window.

              This callback is bound to the <i>current window</i>.

    \see      glutPassiveMotionFunc()
*/
void OGAPIENTRY glutEntryFunc( void( *callback )( int state ) )
{
    SET_CALLBACK( Entry );
}

/*!
    \fn
    \brief    Window destruction callback.
    \ingroup  windowcallback
    \param    callback    Client window destruction hook.

              When a window is destroyed by user-action in
              traditional GLUT, the application terminates.
              In freeglut and OpenGLUT, the application can
              choose to persist and treat the window close
              event as a normal event.  This callback is
              how that event is transmitted to the application.

              This callback is bound to the <i>current window</i>.

    \note     This function is <b>exactly</b> the same as
              glutWMCloseFunc(), which has been deprecated.
              This function should be used instead.

    \todo     There needs to be some work to rationalize the
              behavior when a window is closed.

    \see      glutDestroyWindow()
*/
void OGAPIENTRY glutCloseFunc( void( *callback )( void ) )
{
    SET_CALLBACK( Destroy );
}

/*!
    \fn
    \brief    Window destruction callback.
    \ingroup  deprecated
    \param    callback    Client window destruction hook.

    \warning  Deprecated - use glutCloseFunc instead.

    \see      glutCloseFunc()
*/
void OGAPIENTRY glutWMCloseFunc( void( *callback )( void ) )
{
    ogWarning("glutWMCloseFunc is depecated - use glutCloseFunc instead.");
    glutCloseFunc( callback );
}

/*!
    \fn
    \brief    Destruction callback for menus.
    \ingroup  menucallback
    \param    callback    Client menu destruction hook.

              When a menu is destroyed, OpenGLUT will call this
              hook, if defined on that menu.

              This callback is <b>not</b> bound to any window.

              This callback <b>is</b> bound to a specific menu.

    \note     For emphasis, we repeat: This callback does
              not bind to any window.
    \todo     What on Earth is rationale for this feature?
              Menus should only be destroyed when we ask them to
              be destroyed, or when their window is destroyed.
              In the former case, we already know that the menu
              is being destroyed because we requested it.  In the
              latter case, it is easy for the client to track
              that if they care, via window-destruction callbacks.
    \see      glutCloseFunc(), glutCreateMenu(), glutDestroyMenu()
*/
void OGAPIENTRY glutMenuDestroyFunc( void( *callback )( void ) )
{
    if( ogStructure.Menu )
        ogStructure.Menu->Destroy = callback;
}

/*!
    \fn
    \brief    Deprecated variant of glutMenuStatusFunc()
    \ingroup  deprecated
    \param    callback    Client menu status hook.

              Broadly, OpenGLUT operates in two modes.  At any
              given time, it is either in menu mode (with a popup
              menu display, possibly with subitems) or it is
              not.

              When moving from non-menu to menu status,
              \a callback (if defined) will be called with
              \a GLUT_MENU_IN_USE .  Conversely, when moving
              from menu to non-menu status,
              \a callback (if defined) will be called with
              \a GLUT_MENU_NOT_IN_USE .

              This callback is bound to <b>both</b> the
              <i>current window</i> and
              the <i>current menu</i>.

    \note     Obsolete.  Depcreated.
    \bug      Your callback is not actually called presently.
    \see      glutMenuStatusFunc()
*/
void OGAPIENTRY glutMenuStateFunc( void( *callback )( int status ) )
{
    OPENGLUT_REQUIRE_READY ("glutMenuStatusFunc");
    ogState.MenuStateCallback = callback;
}

/*!
    \fn
    \brief    Modern variant of glutMenuStateFunc()
    \ingroup  menucallback
    \param    callback    Client menu status hook.

              Broadly, OpenGLUT operates in two modes.  At any
              given time, it is either in menu mode (with a popup
              menu display, possibly with subitems) or it is
              not.

              When moving from non-menu to menu status,
              \a callback (if defined) will be called with
              \a GLUT_MENU_IN_USE .  Conversely, when moving
              from menu to non-menu status,
              \a callback (if defined) will be called with
              \a GLUT_MENU_NOT_IN_USE .

              This differs from glutMenuStateFunc() in that
              \a callback is also given the \a x and \a y
              coordinates of the mouse when the menu state
              transition took place.

              This callback is bound to <b>both</b> the
              <i>current window</i> and
              the <i>current menu</i>.

    \bug      Your callback is not actually called presently.
    \see      glutMenuStateFunc()
*/
void OGAPIENTRY glutMenuStatusFunc(
    void( *callback )( int status, int x, int y )
)
{
    OPENGLUT_REQUIRE_READY ("glutMenuStatusFunc");
    ogState.MenuStatusCallback = callback;
}

/*!
    \fn
    \brief    Defines the display hook for an overlay.
    \ingroup  windowcallback
    \param    callback    Client hook for overlay redisplay.

              \a callback is invoked by OpenGLUT to refresh
              an overlay, if OpenGLUT thinks that this is
              necessary.  Generally, very similar to
              glutDisplayFunc(), save that:

              - The redisplay is triggered differently.
              - OpenGLUT sets the layer for you.
              - You do not need to have, and can deregister,
                this callback.

              This callback is bound to <b>both</b>
              the <i>current window</i> and
              the <i>layer</i>.

    \bug      OpenGLUT does not implement layers at this time.
    \see      glutDisplayFunc(), glutPostOverlayRedisplay(),
              glutEstablishOverlay(), glutUseLayer()
*/
void OGAPIENTRY glutOverlayDisplayFunc( void( *callback )( void ) )
{
    SET_CALLBACK( OverlayDisplay );
}

/*!
    \fn
    \brief    Sets the window status callback.
    \ingroup  windowcallback
    \param    callback    Client window status hook.

              When the visibility status of your window changes,
              OpenGLUT either invokes the \a callback registered
              by this function or the
              Visibility \a callback---or neither, if you have
              not registered either callback.

              This differs from glutVisbilityFunc() in that the callback
              has three states, rather than two, that it may receive.
              These states are:

              - \a GLUT_VISIBLE       (every pixel visible)
              - \a GLUT_FULLY_COVERED (every pixel obscured)
              - \a GLUT_HIDDEN        (intermediate)

              The callback is bound to the <i>current window</i>.

    \note     Makes glutVisibilityFunc() obsolete.
    \see      glutVisibilityFunc()
*/
void OGAPIENTRY glutWindowStatusFunc( void( *callback )( int state ) )
{
    SET_CALLBACK( WindowStatus );
}

/*!
    \fn
    \brief    Sets a spaceball motion callback.
    \ingroup  input
    \param    callback    Client spaceball motion hook.

              This function registers a callback for a spaceball
              to report position.  A spaceball has six axes of freedom
              (three of motion and three of orientation), plus buttons.
              The spaceball allows you to move a control point
              in 3D space with a resolution of +/- 1000 units
              along each of 3 axes.

              The callback is bound to the <i>current window</i>.

    \note     OpenGLUT does not implement spaceball support.
    \see      glutSpaceballRotateFunc(), glutSpaceballButtonFunc(),
*/
void OGAPIENTRY glutSpaceballMotionFunc(
    void( *callback )( int x, int y, int z )
)
{
    SET_CALLBACK( SpaceMotion );
}

/*!
    \fn
    \brief    Sets a spaceball rotation callback.
    \ingroup  input
    \param    callback    Client spaceball rotation hook.

              This function registers a callback for a spaceball
              to report rotation.  A spaceball has six axes of freedom
              (three of motion and three of orientation), plus buttons.
              The spaceball allows you to rotate a control orientation
              +/- 1800 units about each of 3 axes.

              The callback is bound to the <i>current window</i>.

    \note     OpenGLUT does not implement spaceball support.
    \see      glutSpaceballMotionFunc(), glutSpaceballButtonFunc(),
*/
void OGAPIENTRY glutSpaceballRotateFunc(
    void( *callback )( int x, int y, int z )
)
{
    SET_CALLBACK( SpaceRotation );
}

/*!
    \fn
    \brief    Sets a spaceball button callback.
    \ingroup  input
    \param    callback    Client spaceball button hook.

              This function registers a callback for a spaceball
              to report buttons.  A spaceball has six axes of freedom
              (three of motion and three of orientation), plus buttons.
              The spaceball has glutDeviceGet(\a GLUT_NUM_SPACEBALL_BUTTONS)
              and numbers them from 1.  Button state is either
              \a GLUT_UP or \a GLUT_DOWN.

              The callback is bound to the <i>current window</i>.

    \note     OpenGLUT does not implement spaceball support.
    \note     In contrast, mouse buttons are numbered from 0 in
              the GLUT API.  This is a wrinkle.
    \see      glutSpaceballRotateFunc(), glutSpaceballMotiononFunc(),
*/
void OGAPIENTRY glutSpaceballButtonFunc(
    void( *callback )( int button , int state )
)
{
    SET_CALLBACK( SpaceButton );
}

/*!
    \fn
    \brief    Sets a button-box button callback.
    \ingroup  input
    \param    callback    Client buttonbox button hook.

              A dials-and-buttons box has buttons numbered from 1 to
              glutDeviceGet(\a GLUT_NUM_BUTTON_BOX_BUTTONS), inclusive.
              The parameters callback are the \a button and its
              \a state as either \a GLUT_UP or \a GLUT_DOWN .

              The callback is bound to the <i>current window</i>.

    \note     OpenGLUT does not implement button-box support.
    \note     Also in contrast, mouse buttons are numbered from 0 in
              the GLUT API.  This is a wrinkle.
    \see      glutDialsFunc()
*/
void OGAPIENTRY glutButtonBoxFunc( void( *callback )( int button, int state ) )
{
    SET_CALLBACK( ButtonBox );
}

/*!
    \fn
    \brief    Sets a dials-box button callback.
    \ingroup  input
    \param    callback    Client dials-box dial hook.

              A dials-and-buttons box has dials numbered from 1 to
              glutDeviceGet(\a GLUT_NUM_BUTTON_BOX_DIALS), inclusive.
              The parameters to \a callback are the \a dial and its
              \a value the latter being an absolute rotation in
              degrees.

              The callback is bound to the <i>current window</i>.

    \note     OpenGLUT does not implement dials-box support.
    \see      glutButtonBoxFunc()
*/
void OGAPIENTRY glutDialsFunc( void( *callback )( int dial, int value ) )
{
    SET_CALLBACK( Dials );
}

/*!
    \fn
    \brief    Sets a tablet motion callback.
    \ingroup  input
    \param    callback    Client tablet motion hook.

              This function registers a \a callback by which OpenGLUT
              reports a puck or stylus position in the range
              of [0, 2000] along the \a x and \a y axes.

              The callback is bound to the <i>current window</i>.

    \note     OpenGLUT does not implement tablet support.
    \todo     We might want to add support for this sooner or later.
              Although a tablet could also be generalized as a mouse.
              There are relatively cheap AipTek HyperPen tablets, and
              slightly less cheap Wacom tablets on many store shelves.
    \note     This API does not include tilt information.
    \see      glutTabletButtonFunc(), glutMouseFunc()
*/
void OGAPIENTRY glutTabletMotionFunc( void (* callback)( int x, int y ) )
{
    SET_CALLBACK( TabletMotion );
}

/*!
    \fn
    \brief    Sets a tablet button callback.
    \ingroup  input
    \param    callback    Client tablet button hook.

              This function registers a \a callback by which you receive
              reports a tablet button status feature.  Buttons are
              reported with \a button in the range 1 to
              glutDeviceGet(\a GLUT_NUM_TABLET_BUTTONS).  \a state is either
              \a GLUT_UP or \a GLUT_DOWN and \a x and \a y are the
              tablet coordinate (see glutTabletMotionFunc() for the bounds
              of \a x and \a y).

              The callback is bound to the <i>current window</i>.

    \note     OpenGLUT does not implement tablet support.
    \todo     We might want to add support for this sooner or later.
              Although a tablet could also be generalized as a mouse.
              There are relatively cheap AipTek HyperPen tablets, and
              slightly less cheap Wacom tablets on many store shelves.
    \note     Buttons are <b>not</b> pressure-sensitive.
    \see      glutTabletButtonFunc(), glutMouseFunc()
*/
void OGAPIENTRY glutTabletButtonFunc(
    void( *callback )( int button, int state,  int x, int y )
)
{
    SET_CALLBACK( TabletButton );
}
