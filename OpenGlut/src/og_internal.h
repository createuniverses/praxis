/*
    \file  og_internal.h
    \brief The OpenGLUT library private include file.
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

#ifndef OG_INTERNAL_H
#define OG_INTERNAL_H

/* XXX Update these for each release! */
#define VERSION_MAJOR 0
#define VERSION_MINOR 6
#define VERSION_PATCH 3

/*
 * OpenGLUT is meant to be available under all Unix/X11, Win32, and WINCE
 * platforms.  Mac, and Mac OS X are not presently supported, but the
 * joystick code makes reference to them as TARGET_HOST_*s.
 */
#define TARGET_HOST_MACINTOSH   0
#define TARGET_HOST_MAC_OSX     0
#if defined( _WIN32_WCE )
#   define TARGET_HOST_UNIX_X11    0
#   define TARGET_HOST_WIN32       0
#   define TARGET_HOST_WINCE       1
#elif defined( _MSC_VER ) || defined( __MINGW32__ )
#   define TARGET_HOST_UNIX_X11    0
#   define TARGET_HOST_WIN32       1
#   define TARGET_HOST_WINCE       0
#else
#   define TARGET_HOST_UNIX_X11    1
#   define TARGET_HOST_WIN32       0
#   define TARGET_HOST_WINCE       0
#endif

/*
 * Mouse buttons 0 through {FREEGLUT_MAX_MENUS - 1} can be
 * mapped to OpenGLUT menus.  This really should be made
 * dynamic.  As long as we have legacy 3-button limitations
 * for menus, we might as well keep the FREEGLUT_* name as
 * a reminder of the limitation needing removal.
 */
#define FREEGLUT_MAX_MENUS         3

/*
 * Somehow all Win32 include headers depend on these:
 */
#if TARGET_HOST_WIN32
#include <windows.h>
#include <windowsx.h>
#include <mmsystem.h>
#include <tchar.h>
#endif

/*
 * Those files should be available on every platform.
 */
#include <GL/gl.h>
#include <GL/glu.h>
#include <assert.h>
#include <math.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/*
 * The system-dependant include files should go here:
 */
#if TARGET_HOST_UNIX_X11
#    include <GL/glx.h>
#    include <X11/Xlib.h>
#    include <X11/Xatom.h>
#    include <X11/keysym.h>
#    ifdef HAVE_X11_EXTENSIONS_XF86VMODE_H
#        include <X11/extensions/xf86vmode.h>
#    endif
#    include <unistd.h>
#    include <sys/time.h>
#endif

/*
 * M_PI is defined by many UNIX <math.h>, but is not standard.
 */
#ifndef M_PI
#    define M_PI  3.14159265358979323846
#endif

#ifndef TRUE
#    define TRUE  1
#endif

#ifndef FALSE
#    define FALSE  0
#endif

/*
 * There are a few places, especially in WIN32, where we needed to pass
 * in a string name.  Then we expect to get the same thing back out
 * in other places.  If freeglut had provided---and used---this #define,
 * our lives would have been a tiny bit easier.  (^&
 *
 * XXX freeglut used "FREEGLUT" rather than "freeglut" or "FreeGLUT".
 * XXX But generally, freeglut refers to itself as "freeglut" (all
 * XXX lower case).
 * XXX
 * XXX It is possible that it has to be uppercased, but that seems
 * XXX unlikely, so I used the proper capitalization of "OpenGLUT" for
 * XXX our project.
 * XXX
 * XXX This string should only be used where a "magic cookie" string is
 * XXX actually required and must be used consistantly.  Do not use it
 * XXX gratuitously, as it may have to be capitalized or have other
 * XXX requirements that would not suit well for, e.g., error messages.
 */
#ifndef OPENGLUT_STRING
#    define OPENGLUT_STRING "OpenGLUT"
#endif

/* -- GLOBAL TYPE DEFINITIONS ---------------------------------------------- */

/*
 * OpenGLUT callbacks type definitions
 */
typedef void (* OGCBDisplay       )( void );
typedef void (* OGCBReshape       )( int, int );
typedef void (* OGCBVisibility    )( int );
typedef void (* OGCBKeyboard      )( unsigned char, int, int );
typedef void (* OGCBSpecial       )( int, int, int );
typedef void (* OGCBMouse         )( int, int, int, int );
typedef void (* OGCBMouseWheel    )( int, int, int, int );
typedef void (* OGCBMotion        )( int, int );
typedef void (* OGCBPassive       )( int, int );
typedef void (* OGCBEntry         )( int );
typedef void (* OGCBWindowStatus  )( int );
typedef void (* OGCBSelect        )( int, int, int );
typedef void (* OGCBJoystick      )( unsigned int, int, int, int );
typedef void (* OGCBKeyboardUp    )( unsigned char, int, int );
typedef void (* OGCBSpecialUp     )( int, int, int );
typedef void (* OGCBOverlayDisplay)( void );
typedef void (* OGCBSpaceMotion   )( int, int, int );
typedef void (* OGCBSpaceRotation )( int, int, int );
typedef void (* OGCBSpaceButton   )( int, int );
typedef void (* OGCBDials         )( int, int );
typedef void (* OGCBButtonBox     )( int, int );
typedef void (* OGCBTabletMotion  )( int, int );
typedef void (* OGCBTabletButton  )( int, int, int, int );
typedef void (* OGCBDestroy       )( void );

/*
 * The global callbacks type definitions
 */
typedef void (* OGCBIdle          )( void );
typedef void (* OGCBTimer         )( int );
typedef void (* OGCBMenuState     )( int );
typedef void (* OGCBMenuStatus    )( int, int, int );

/*
 * The callback used when creating/using menus
 */
typedef void (* OGCBMenu          )( int );


/*
 * A list structure
 */
typedef struct tagSOG_List SOG_List;
struct tagSOG_List
{
    void *First;
    void *Last;
};

/*
 * A list node structure
 */
typedef struct tagSOG_Node SOG_Node;
struct tagSOG_Node
{
    void *Next;
    void *Prev;
};

/*
 * A helper structure holding two ints and a boolean
 */
typedef struct tagSOG_XYUse SOG_XYUse;
struct tagSOG_XYUse
{
    GLint           X, Y;               /* The two integers...               */
    GLboolean       Use;                /* ...and a single boolean.          */
};

/*
 * These types allow us to be a little more abstract in
 * handling time-values.
 */
#if TARGET_HOST_WIN32 || TARGET_HOST_WINCE
    typedef DWORD ogTimeType;
#elif TARGET_HOST_UNIX_X11
    typedef struct timeval ogTimeType;
#endif

/*
 * A helper structure holding a timeval and a boolean
 */
typedef struct tagSOG_Time SOG_Time;
struct tagSOG_Time
{
    ogTimeType      Value;
    GLboolean       Set;
};

/*
 * An enumeration containing the state of the GLUT execution:
 * initializing, running, or stopping
 */
typedef enum
{
    GLUT_EXEC_STATE_INIT,
    GLUT_EXEC_STATE_RUNNING,
    GLUT_EXEC_STATE_STOP
} ogExecutionState;


/*
 * This structure holds different OpenGLUT settings
 */
typedef struct tagSOG_State SOG_State;
struct tagSOG_State
{
    SOG_XYUse        Position;             /* The default windows' position  */
    SOG_XYUse        Size;                 /* The default windows' size      */
    unsigned int     DisplayMode;          /* Display mode for new windows   */

    GLboolean        Initialised;          /* OpenGLUT has been initialised  */

    GLboolean        ForceDirectContext;   /* Require direct rendering?      */
    GLboolean        TryDirectContext;     /* Try direct before indirect?    */

    GLboolean        ForceIconic;          /* New top windows are iconified  */
    GLboolean        UseCurrentContext;    /* New windows share with current */

    GLboolean        GLDebugSwitch;        /* OpenGL state debugging switch  */
    GLboolean        XSyncSwitch;          /* X11 sync protocol switch       */

    int              KeyRepeat;            /* Global key repeat mode.        */
    unsigned         Modifiers;            /* Current ALT/SHIFT/CTRL state   */

    GLuint           FPSInterval;          /* Interval between FPS printfs   */
    GLuint           SwapCount;            /* Count of glutSwapBuffer calls  */
    GLuint           SwapTime;             /* Time of last SwapBuffers       */

    SOG_Time         Time;                 /* Time that glutInit was called  */
    SOG_List         Timers;               /* The OpenGLUT timer hooks       */
    SOG_List         FreeTimers;           /* The unused timer hooks         */

    OGCBIdle         IdleCallback;         /* The global idle callback       */

    int              ActiveMenus;          /* Num. of currently active menus */
    OGCBMenuState    MenuStateCallback;    /* Menu callbacks are global      */
    OGCBMenuStatus   MenuStatusCallback;

    SOG_XYUse        GameModeSize;         /* Game mode screen's dimensions  */
    int              GameModeDepth;        /* The pixel depth for game mode  */
    int              GameModeRefresh;      /* The refresh rate for game mode */

    int              ActionOnWindowClose; /* Action when user closes window  */

    ogExecutionState ExecState;           /* Used for GLUT termination       */
    char            *ProgramName;         /* Name of the invoking program    */
    GLboolean       JoysticksInitted;     /* Only init. if we use them       */
    jmp_buf         BackToMainLoop;       /* For our unstructured returns    */
    int             InMainLoop;           /* Can we use BackToMainLoop?      */
};

/*
 * The structure used by display initialization in og_init.c
 */
typedef struct tagSOG_Display SOG_Display;
struct tagSOG_Display
{
#if TARGET_HOST_UNIX_X11
    Display        *Display;            /* The display we are being run in.  */
    int             Screen;             /* The screen we are about to use.   */
    Window          RootWindow;         /* The screen's root window.         */
    int             Connection;         /* The display's connection number   */
    Atom            DeleteWindow;       /* The window deletion atom          */

#ifdef X_XF86VidModeGetModeLine
    /*
     * XF86VidMode may be compilable even if it fails at runtime.  Therefore,
     * the validity of the VidMode has to be tracked
     */
    int             DisplayModeValid;   /* Flag that indicates runtime status*/
    XF86VidModeModeLine DisplayMode;    /* Current screen's display settings */
    unsigned        DisplayModeClock;   /* The display mode's refresh rate   */
    int             DisplayViewPortX;   /* saved X location of the viewport  */
    int             DisplayViewPortY;   /* saved Y location of the viewport  */
    int             DisplayPointerX;    /* saved X location of the pointer   */
    int             DisplayPointerY;    /* saved Y location of the pointer   */

#endif

#elif TARGET_HOST_WIN32 || TARGET_HOST_WINCE
    HINSTANCE        Instance;          /* The application's instance        */
    DEVMODE         DisplayMode;        /* Desktop's display settings        */

#endif

    int             ScreenWidth;        /* The screen's width in pixels      */
    int             ScreenHeight;       /* The screen's height in pixels     */
    int             ScreenWidthMM;      /* The screen's width in milimeters  */
    int             ScreenHeightMM;     /* The screen's height in milimeters */
};


/*
 * The user can create any number of timer hooks
 */
typedef struct tagSOG_Timer SOG_Timer;
struct tagSOG_Timer
{
    SOG_Node        Node;
    int             ID;                 /* The timer ID integer              */
    OGCBTimer       Callback;           /* The timer callback                */
    long            TriggerTime;        /* The timer trigger time            */
};

/*
 * Make OpenGLUT window handle and context types so that we don't need so
 * much conditionally-compiled code later in the library.
 */
#if TARGET_HOST_UNIX_X11
    typedef Window     SOG_WindowHandleType;
    typedef GLXContext SOG_WindowContextType;
#elif TARGET_HOST_WIN32 || TARGET_HOST_WINCE
    typedef HWND    SOG_WindowHandleType;
    typedef HGLRC   SOG_WindowContextType;
#endif

/*
 * A window and its OpenGL context. The contents of this structure
 * are highly dependant on the target operating system we aim at...
 */
typedef struct tagSOG_Context SOG_Context;
struct tagSOG_Context
{
    SOG_WindowHandleType  Handle;     /* The window's handle                 */
    SOG_WindowContextType Context;    /* The window's OpenGL/WGL context     */

#if TARGET_HOST_UNIX_X11
    XVisualInfo          *VisualInfo; /* The window's visual information     */
    Pixmap                Pixmap;     /* Used for offscreen rendering        */

    SOG_WindowHandleType  PrevFocus;  /* The previous focus window           */
    int                   PrevFocusReturnTo;

#elif TARGET_HOST_WIN32 || TARGET_HOST_WINCE
    HDC                   Device;     /* The window's device context         */
    GLubyte              *Bits;       /* For offscreen Bitmap rendering      */
    HBITMAP               Bitmap;     /* For offscreen Bitmap rendering      */
#endif

    int             DoubleBuffered;   /* Treat the window as double-buffered */
};

/*
 * Window state description. This structure should be kept portable.
 */
typedef struct tagSOG_WindowState SOG_WindowState;
struct tagSOG_WindowState
{
    int             Width;              /* Window's width in pixels          */
    int             Height;             /* The same about the height         */
    int             OldWidth;           /* Window width from before a resize */
    int             OldHeight;          /*   "    height  "    "    "   "    */

    GLboolean       Redisplay;          /* Do we have to redisplay?          */
    GLboolean       Visible;            /* Is the window visible now         */

    int             Cursor;             /* The currently selected cursor     */

    long            JoystickPollRate;   /* The joystick polling rate         */
    long            JoystickLastPoll;   /* When the last poll happened       */

    int             MouseX, MouseY;     /* The most recent mouse position    */

    GLboolean       IgnoreKeyRepeat;    /* Whether to ignore key repeat.     */
    GLboolean       KeyRepeating;       /* Currently in repeat mode          */

    GLboolean       IsGameMode;         /* Is this the game mode window?     */
    GLboolean       NeedToResize;       /* Do we need to resize the window?  */
    GLboolean       IsOffscreen;        /* Tags a `window' as on/offscreen.  */
};


/*
 * SET_WCB() is used as:
 *
 *     SET_WCB( window, Visibility, func );
 *
 * ...where {window} is the OpenGLUT window to set the callback,
 *          {Visibility} is the window-specific callback to set,
 *          {func} is a function-pointer.
 *
 * Originally, {FETCH_WCB( ... ) = func} was rather sloppily used,
 * but this can cause warnings because the FETCH_WCB() macro type-
 * casts its result, and a type-cast value shouldn't be an lvalue.
 *
 * The {if( FETCH_WCB( ... ) != func )} test is to do type-checking
 * and for no other reason.  Since it's hidden in the macro, the
 * ugliness is felt to be rather benign.
 */
#define SET_WCB(window,cbname,func)                            \
do                                                             \
{                                                              \
    SOG_Window *win = &window;                                 \
    if( FETCH_WCB( *win, cbname ) != func )                    \
        ((win->CallBacks[CB_ ## cbname]) = (void *) func);     \
} while( 0 )

/*
 * FETCH_WCB() is used as:
 *
 *     FETCH_WCB( window, Visibility );
 *
 * ...where {window} is the OpenGLUT window to fetch the callback from,
 *          {Visibility} is the window-specific callback to fetch.
 *
 * The result is correctly type-cast to the callback function pointer
 * type.
 */
#define FETCH_WCB(window,cbname) \
    ((OGCB ## cbname)((window).CallBacks[CB_ ## cbname]))

/*
 * INVOKE_WCB() is used as:
 *
 *     INVOKE_WCB( window, Visibility, ( status ) );
 *
 * ...where {window} is the OpenGLUT window,
 *          {Visibility} is the window-specific callback,
 *          {(status)} is the parameter list.
 *
 * The callback is invoked as:
 *
 *    callback( status );
 *
 * ...so the parentheses are REQUIRED in the {arg_list}.
 *
 * NOTE that it does a sanity-check and also sets the
 * current window.
 *
 */
#define INVOKE_WCB(window,cbname,arg_list)    \
do                                            \
{                                             \
    SOG_Window *win = &window;                \
    if( FETCH_WCB( *win, cbname ) )           \
    {                                         \
        ogSetWindow( win );                   \
        FETCH_WCB( *win, cbname ) arg_list;   \
    }                                         \
} while( 0 )

/*
 * The window callbacks the user can supply us with. Should be kept portable.
 *
 * This enumeration provides the OpenGLUT CallBack numbers.
 * The symbolic constants are indices into a window's array of
 * function callbacks.  The names are formed by splicing a common
 * prefix onto the callback's base name.  (This was originally
 * done so that an early stage of development could live side-by-
 * side with the old callback code.  The old callback code used
 * the bare callback's name as a structure member, so I used a
 * prefix for the array index name.)
 *
 * XXX For consistancy, perhaps the prefix should match the
 * XXX FETCH* and INVOKE* macro suffices.  I.e., WCB_, rather than
 * XXX CB_.
 */
enum
{
    CB_Display,
    CB_Reshape,
    CB_Keyboard,
    CB_KeyboardUp,
    CB_Special,
    CB_SpecialUp,
    CB_Mouse,
    CB_MouseWheel,
    CB_Motion,
    CB_Passive,
    CB_Entry,
    CB_Visibility,
    CB_WindowStatus,
    CB_Joystick,
    CB_Destroy,

    /* Presently ignored */
    CB_Select,
    CB_OverlayDisplay,
    CB_SpaceMotion,
    CB_SpaceRotation,
    CB_SpaceButton,
    CB_Dials,
    CB_ButtonBox,
    CB_TabletMotion,
    CB_TabletButton,

    /* Always make this the LAST one */
    TOTAL_CALLBACKS
};


/*
 * This structure holds the OpenGL rendering context for all the menu windows
 */
typedef struct tagSOG_MenuContext SOG_MenuContext;
struct tagSOG_MenuContext
{
#if TARGET_HOST_UNIX_X11
    XVisualInfo        *VisualInfo;   /* The window's visual information  */
#endif
    SOG_WindowContextType Context;    /* The menu window's OpenGL context */
};

/*
 * This structure describes a menu
 */
typedef struct tagSOG_Window SOG_Window;
typedef struct tagSOG_MenuEntry SOG_MenuEntry;
typedef struct tagSOG_Menu SOG_Menu;
struct tagSOG_Menu
{
    SOG_Node            Node;
    void               *UserData;     /* User data passed back at callback   */
    int                 ID;           /* The global menu ID                  */
    SOG_List            Entries;      /* The menu entries list               */
    OGCBMenu            Callback;     /* The menu callback                   */
    OGCBDestroy         Destroy;      /* Destruction callback                */
    GLboolean           IsActive;     /* Is the menu selected?               */
    int                 Width;        /* Menu box width in pixels            */
    int                 Height;       /* Menu box height in pixels           */
    int                 X, Y;         /* Menu box raster position            */

    SOG_MenuEntry      *ActiveEntry;  /* Currently active entry in the menu  */
    SOG_Window         *Window;       /* Window for menu                     */
    SOG_Window         *ParentWindow; /* Window in which the menu is defined */
};

/*
 * This is a menu entry
 */
struct tagSOG_MenuEntry
{
    SOG_Node            Node;
    int                 ID;                     /* The menu entry ID (local) */
    int                 Ordinal;                /* The menu's ordinal number */
    char               *Text;                   /* The text to be displayed  */
    SOG_Menu           *SubMenu;                /* Optional sub-menu tree    */
    GLboolean           IsActive;               /* Is the entry highlighted? */
    int                 Width;                  /* Label's width in pixels   */
};

/*
 * A window, making part of OpenGLUT windows hierarchy.
 * Should be kept portable.
 */
struct tagSOG_Window
{
    SOG_Node            Node;
    int                 ID;                     /* Window's ID number        */

    SOG_Context         Window;                       /* Window and OpenGL context */
    SOG_WindowState     State;                        /* The window state          */
    void               *CallBacks[ TOTAL_CALLBACKS ]; /* Array of window callbacks */
    void               *UserData;                     /* For use by user           */

    SOG_Menu           *Menu[ FREEGLUT_MAX_MENUS ];   /* Menus appended to window  */
    SOG_Menu           *ActiveMenu;                   /* The window's active menu  */

    SOG_Window         *Parent;                 /* The parent to this window */
    SOG_List            Children;               /* The subwindows d.l. list  */

    GLboolean           IsMenu;                 /* Set to 1 if we are a menu */
    GLboolean           IsUnmanaged;            /* GL_TRUE: Borderless|menu  */
    GLboolean           IsBorderless;           /* GL_TRUE: GLUT_BORDERLESS  */
    GLboolean           IsClientMenu;           /* GL_TRUE: Menu Window      */
};


/*
 * A linked list structure of windows
 */
typedef struct tagSOG_WindowList SOG_WindowList;
struct tagSOG_WindowList
{
    SOG_Node    node;
    SOG_Window *window;
};

/*
 * This holds information about all the windows, menus etc.
 */
typedef struct tagSOG_Structure SOG_Structure;
struct tagSOG_Structure
{
    SOG_List         Windows;     /* The global windows list            */
    SOG_List         Menus;       /* The global menus list              */
    SOG_List         WindowsToDestroy;

    SOG_Window      *Window;      /* The currently active win.          */
    SOG_Menu        *Menu;        /* Same, but menu...                  */

    SOG_MenuContext *MenuContext; /* OpenGL rendering context for menus */

    SOG_Window      *GameMode;    /* The game mode window               */

    int              WindowID;    /* The new current window ID          */
    int              MenuID;      /* The new current menu ID            */
};

/*
 * This structure is used for the enumeration purposes.
 * You can easily extend its functionalities by declaring
 * a structure containing enumerator's contents and custom
 * data, then casting its pointer to (SOG_Enumerator *).
 */
typedef struct tagSOG_Enumerator SOG_Enumerator;
struct tagSOG_Enumerator
{
    GLboolean   found;                          /* Used to terminate search  */
    void       *data;                           /* Custom data pointer       */
};
typedef void (* OGCBenumerator  )( SOG_Window *, SOG_Enumerator * );

/*
 * The bitmap font structure
 */
typedef struct tagSOG_Font SOG_Font;
struct tagSOG_Font
{
    char           *Name;         /* The source font name             */
    int             Quantity;     /* Number of chars in font          */
    int             Height;       /* Height of the characters         */
    const GLubyte **Characters;   /* The characters mapping           */

    float           xorig, yorig; /* Relative origin of the character */
};

/*
 * The stroke font structures
 */

typedef struct tagSOG_StrokeVertex SOG_StrokeVertex;
struct tagSOG_StrokeVertex
{
    GLfloat         X, Y;
};

typedef struct tagSOG_StrokeStrip SOG_StrokeStrip;
struct tagSOG_StrokeStrip
{
    int                     Number;
    const SOG_StrokeVertex *Vertices;
};

typedef struct tagSOG_StrokeChar SOG_StrokeChar;
struct tagSOG_StrokeChar
{
    GLfloat         Right;
    int             Number;
    const SOG_StrokeStrip* Strips;
};

typedef struct tagSOG_StrokeFont SOG_StrokeFont;
struct tagSOG_StrokeFont
{
    char           *Name;                       /* The source font name      */
    int             Quantity;                   /* Number of chars in font   */
    GLfloat         Height;                     /* Height of the characters  */
    const SOG_StrokeChar **Characters;          /* The characters mapping    */
};

/* -- GLOBAL VARIABLES EXPORTS --------------------------------------------- */

/*
 * OpenGLUT display related stuff (initialized once per session)
 */
extern SOG_Display ogDisplay;

/*
 * OpenGLUT internal structure
 */
extern SOG_Structure ogStructure;

/*
 * The current OpenGLUT settings
 */
extern SOG_State ogState;


/* -- PRIVATE MACRO DEFINITIONS -------------------------------------------- */

/*
 * A call to this function makes us sure that the Display and Structure
 * subsystems have been properly initialized and are ready to be used.
 */
#define  OPENGLUT_READY ( ogState.Initialised )
#define  freeglut_assert_ready  assert( OPENGLUT_READY )
#define  OPENGLUT_ASSERT_READY  assert( OPENGLUT_READY )
#define  OPENGLUT_REQUIRE_READY(func)                          \
    if( !OPENGLUT_READY )                                      \
        ogError(                                               \
            "OpenGLUT must be initialized when calling %s().", \
            func                                               \
        )

/*
 * A call to those macros assures us that there is a current
 * window and menu set, respectively:
 */
#define  freeglut_assert_window assert( ogStructure.Window )
#define  freeglut_assert_menu   assert( ogStructure.Menu )

/* -- PRIVATE FUNCTION DECLARATIONS ---------------------------------------- */

/*
 * The deinitialize function gets called on leaving glutMainLoop().
 * It cleans up everything inside of OpenGLUT.
 */
void ogDeinitialize( void );

/*
 * These two functions are used to create/destroy the OpenGLUT internal
 * structures. This actually happens when calling glutInit() and when
 * quitting the glutMainLoop() (which actually happens, when all windows
 * have been closed).
 */
void ogCreateStructure( void );
void ogDestroyStructure( void );

/*
 * A helper function to check if a display mode is possible to use
 */
#if TARGET_HOST_UNIX_X11
XVisualInfo *ogChooseVisual( void );
#endif

/*
 * The window procedure for Win32 events handling
 */
#if TARGET_HOST_WIN32 || TARGET_HOST_WINCE
LRESULT CALLBACK ogWindowProc(
    HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam
);
GLboolean ogSetupPixelFormat(
    SOG_Window *window, GLboolean checkOnly, unsigned char layer_type
);
#endif


/*
 * This enum is for internal use by ogCreateWindow().
 */
typedef enum EOG_winType
{
    OG_CW_NORMAL,
    OG_CW_GAMEMODE,
    OG_CW_MENU,
    OG_CW_CLIENT_MENU,
    OG_CW_LAST         /* Used for bounds-checking {EOG_winType} vars */
} EOG_winType;
/*
 * Window creation, opening, closing and destruction.
 * Also CallBack clearing/initialization.
 * Defined in og_structure.c, og_window.c.
 */
SOG_Window *ogCreateWindow(
    SOG_Window *parent, const char *title, int x, int y, int w, int h,
    EOG_winType winType
);
void        ogSetWindow ( SOG_Window *window );
void        ogOpenWindow(
    SOG_Window *window, const char *title, int x, int y, int w, int h,
    GLboolean gameMode, GLboolean isSubWindow
);
void        ogCloseWindow( SOG_Window *window );
void        ogAddToWindowDestroyList ( SOG_Window *window );
void        ogCloseWindows( void );
void        ogDestroyWindow( SOG_Window *window );

/*
 * Menu creation and destruction. Defined in og_structure.c
 */
SOG_Menu   *ogCreateMenu( OGCBMenu menuCallback );
void        ogDestroyMenu( SOG_Menu *menu );

/*
 * Joystick device management functions, defined in og_joystick.c
 */
void        ogJoystickInit( void );
void        ogJoystickShutdown( void );
void        ogJoystickOpen( int ident );
void        ogJoystickPollWindow( SOG_Window *window );
int         ogJoystickDetect( void );

/*
 * Helper function to enumerate through all registered windows
 * and one to enumerate all of a window's subwindows...
 *
 * The GFunc callback for those functions will be defined as:
 *
 *      void enumCallback( gpointer window, gpointer enumerator );
 *
 * where window is the enumerated (sub)window pointer (SOG_Window *),
 * and userData is the a custom user-supplied pointer. Functions
 * are defined and exported from og_structure.c file.
 */
void ogEnumWindows( OGCBenumerator enumCallback, SOG_Enumerator *enumerator );
void ogEnumSubWindows(
    SOG_Window *window, OGCBenumerator enumCallback,
    SOG_Enumerator *enumerator
);

/*
 * ogWindowByHandle returns a (SOG_Window *) value pointing to the
 * first window in the queue matching the specified window handle.
 * The function is defined in og_structure.c file.
 */
SOG_Window *ogWindowByHandle( SOG_WindowHandleType hWindow );

/*
 * This function is similiar to the previous one, except it is
 * looking for a specified (sub)window identifier. The function
 * is defined in og_structure.c file.
 */
SOG_Window *ogWindowByID( int windowID );

/*
 * Looks up a menu given its ID.
 */
SOG_Menu *ogMenuByID( int menuID );

/*
 * The menu activation and deactivation the code. This is the meat
 * of the menu user interface handling code...
 */
void ogActivateMenu( SOG_Window* window, int button );
void ogExecuteMenuCallback( SOG_Menu* menu );
GLboolean ogCheckActiveMenu ( SOG_Window *window, SOG_Menu *menu );
void ogDeactivateMenu( SOG_Window *window );
void ogDeactivateSubMenu( SOG_MenuEntry *menuEntry );

/*
 * This is registered as a callback for the windows that the builtin
 * menu system uses.
 *
 * XXX It should be made {static}, and this prototype removed.
 */
void ogDisplayMenu( void );


/*
 * Elapsed time as per glutGet(GLUT_ELAPSED_TIME).
 */
long ogElapsedTime( void );

/*
 * List functions
 */
void ogListInit  (SOG_List *list);
void ogListAppend(SOG_List *list, SOG_Node *node);
void ogListRemove(SOG_List *list, SOG_Node *node);
int  ogListLength(SOG_List *list);
void ogListInsert(SOG_List *list, SOG_Node *next, SOG_Node *node);

/*
 * String functions.
 *
 * (strdup() is not actually a standard function, so we cannot
 *  (strictly legally) depend upon it.  To avoid conflict with
 *  a native strdup(), we use a different name here.)
 */
char *ogStrDup( const char *str );

/*
 * Error Messages functions
 */
void ogError( const char *fmt, ... );
void ogWarning( const char *fmt, ... );

#endif
