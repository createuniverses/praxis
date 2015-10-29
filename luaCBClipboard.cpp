
// Author:  Greg "fugue" Santucci, 2011
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/

#include "luaCB.h"

#ifdef __PRAXIS_LINUX__

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

int X11_SetClipboardText(const char *text)
{
    // printf("X11_SetClipboardText  %s\n", text);
    // fflush(stdout);

    Display *display = g_pAppDisplay;
    Window window = g_pAppWindow;
    Atom format = XA_STRING;

    Atom XA_CLIPBOARD = XInternAtom(display, "CLIPBOARD", 0);

    /* Save the selection on the root window */
    XChangeProperty(display, DefaultRootWindow(display),
        XA_CUT_BUFFER0, format, 8, PropModeReplace,
        (const unsigned char *)text, strlen(text));

    if (XA_CLIPBOARD != None &&
        XGetSelectionOwner(display, XA_CLIPBOARD) != window)
    {
        XSetSelectionOwner(display, XA_CLIPBOARD, window, CurrentTime);
    }

    if (XGetSelectionOwner(display, XA_PRIMARY) != window)
    {
        XSetSelectionOwner(display, XA_PRIMARY, window, CurrentTime);
    }

    return 0;
}

char * X11_GetClipboardText()
{
    Display *display = g_pAppDisplay;
    Window window = g_pAppWindow;
    Atom format = XA_STRING;
    Window owner;
    Atom selection;
    Atom seln_type;
    int seln_format;
    unsigned long nbytes;
    unsigned long overflow;
    unsigned char *src;
    char *text;

    Atom XA_CLIPBOARD = XInternAtom(display, "CLIPBOARD", 0);
    if (XA_CLIPBOARD == None)
    {
        printf("Couldn't access X clipboard\n");
        return strdup("");
    }

    text = NULL;

    /* Get the window that holds the selection */
    owner = XGetSelectionOwner(display, XA_CLIPBOARD);
    if ((owner == None) || (owner == window))
    {
        owner = DefaultRootWindow(display);
        selection = XA_CUT_BUFFER0;
    }
    else
    {
        /* Request that the selection owner copy the data to our window */
        owner = window;
        selection = XInternAtom(display, "PRAXIS_SELECTION", False);
        XConvertSelection(display, XA_CLIPBOARD, format, selection, owner,
            CurrentTime);
        XFlush(display);
    }

    usleep(100000);

    if (XGetWindowProperty(display, owner, selection, 0, INT_MAX/4, False,
            format, &seln_type, &seln_format, &nbytes, &overflow, &src)
            == Success)
    {
        if (seln_type == format) {
            text = (char *)malloc(nbytes+1);
            if (text) {
                memcpy(text, src, nbytes);
                text[nbytes] = '\0';
            }
        }
        XFree(src);
    }

    if (!text)
    {
        text = strdup("");
    }

    return text;
}

#endif // __PRAXIS_LINUX__

int luaCBSetClipboardText(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    HGLOBAL      hGlobal ;
    PTSTR        pGlobal ;

    std::string sText = luaL_checkstring(L, 1);

    hGlobal = GlobalAlloc (GHND | GMEM_SHARE, sText.length() + 1) ;
    pGlobal = (PTSTR)GlobalLock (hGlobal) ;

    for (int i = 0 ; i < sText.length() ; i++)
        *(pGlobal+i) = sText.at(i);

    GlobalUnlock (hGlobal) ;

    OpenClipboard (g_AppHWND) ;

    EmptyClipboard () ;

    SetClipboardData (CF_TEXT, hGlobal) ;

    CloseClipboard () ;
#endif

#ifdef __PRAXIS_LINUX__
    std::string sText = luaL_checkstring(L, 1);
    X11_SetClipboardText(sText.c_str());
#endif

    return 0;
}

int luaCBGetClipboardText(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    HGLOBAL      hGlobal ;
    PTSTR        pGlobal ;

    std::string sText;

    OpenClipboard (g_AppHWND) ;
    if (hGlobal = GetClipboardData (CF_TEXT))
    {
         pGlobal = (PTSTR)GlobalLock (hGlobal) ;
         sText = pGlobal;
         GlobalUnlock(hGlobal);
    }
    CloseClipboard () ;

    lua_pushstring(L, sText.c_str());
#endif

#ifdef __PRAXIS_LINUX__
    //X11_GetClipboardText();
    //usleep(200000);
    lua_pushstring(L, X11_GetClipboardText());
#endif

    return 1;
}

int luaCBClearClipboardText(lua_State * L)
{
#ifdef __PRAXIS_WINDOWS__
    OpenClipboard (g_AppHWND) ;
    EmptyClipboard();
    CloseClipboard();
#endif

#ifdef __PRAXIS_LINUX__
    X11_SetClipboardText("");
#endif

    return 0;
}

void luaInitCallbacksClipboard()
{
    lua_register(g_pLuaState, "getClipboardText",      luaCBGetClipboardText);
    lua_register(g_pLuaState, "setClipboardText",      luaCBSetClipboardText);
    lua_register(g_pLuaState, "clearClipboardText",    luaCBClearClipboardText);
}
