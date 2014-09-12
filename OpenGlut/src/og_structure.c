/*
    \file  og_structure.c
    \brief Window and menu linked-list tree structure
*/
/*
 * Portions copyright (C) 2004, the OpenGLUT project contributors.
 * OpenGLUT branched from freeglut in February, 2004.
 *
 * Copyright (c) 1999-2000 Pawel W. Olszta. All Rights Reserved.
 * Written by Pawel W. Olszta, <olszta@sourceforge.net>
 * Creation date: Sat Dec 18 1999
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
#include <GL/openglut_exp.h>
#include "og_internal.h"

/* -- GLOBAL EXPORTS ------------------------------------------------------- */

/*
 * The SOG_Structure container holds information about windows and menus
 * created between glutInit() and glutMainLoop() return.
 */

SOG_Structure ogStructure =
{
    { NULL, NULL },  /* The list of windows        */
    { NULL, NULL },  /* The list of menus          */
    { NULL, NULL },  /* Windows to Destroy list    */
    NULL,            /* The current window         */
    NULL,            /* The current menu           */
    NULL,            /* The menu OpenGL context    */
    NULL,            /* The game mode window       */
    0,               /* The highest used window ID */
    0                /* The highest used menu ID   */
};


/* -- PRIVATE FUNCTIONS ---------------------------------------------------- */

static void ogClearCallBacks( SOG_Window *window )
{
    if( window )
    {
        int i;
        for( i = 0; i < ( int )TOTAL_CALLBACKS; ++i )
            window->CallBacks[ i ] = NULL;
    }
}

/*
 * This private function creates, opens and adds to the hierarchy
 * an OpenGLUT window complete with OpenGL context and stuff...
 *
 * If parent is set to NULL, the window created will be a topmost one.
 */
SOG_Window *ogCreateWindow(
    SOG_Window *parent,
    const char *title,
    int x, int y, int w, int h,
    EOG_winType winType
)
{
    int fakeArgc = 0;
    GLboolean gameMode = GL_FALSE;
    SOG_Window *window;

    assert( OG_CW_LAST > winType );

    window = ( SOG_Window * )calloc( sizeof( SOG_Window ), 1 );
    if( OG_CW_GAMEMODE == winType )
        gameMode = GL_TRUE;

    ogClearCallBacks( window );

    if( !ogState.Initialised )
        glutInit( &fakeArgc, NULL );

    window->ID = ++ogStructure.WindowID;
    window->State.OldHeight = window->State.OldWidth = -1;

    ogListInit( &window->Children );
    if( !parent )
        ogListAppend( &ogStructure.Windows, &window->Node );
    else
    {
        ogListAppend( &parent->Children, &window->Node );
        window->Parent = parent;
    }

    window->State.Cursor = GLUT_CURSOR_INHERIT;
    window->IsMenu       = ( OG_CW_MENU == winType ) ? GL_TRUE : GL_FALSE;
    window->IsBorderless =
        ( ogState.DisplayMode & GLUT_BORDERLESS ) ? GL_TRUE : GL_FALSE;
    window->IsClientMenu =
        ( OG_CW_CLIENT_MENU == winType ) ? GL_TRUE : GL_FALSE;
    window->IsUnmanaged  = GL_FALSE;
    if( window->IsMenu || window->IsBorderless || window->IsClientMenu )
        window->IsUnmanaged = GL_TRUE;

    window->State.IgnoreKeyRepeat = GL_FALSE;
    window->State.KeyRepeating    = GL_FALSE;

    ogOpenWindow(
        window, title, x, y, w, h, gameMode,
        ( GLboolean )(
            ( !window->IsClientMenu && parent ) ?
            GL_TRUE : GL_FALSE
        )
    );

    return window;
}

/*
 * Create a menu and add it to the menus list
 */
SOG_Menu *ogCreateMenu( OGCBMenu menuCallback )
{
    int x = 100, y = 100, w = 100, h = 100;
    SOG_Window *current_window = ogStructure.Window;

    SOG_Menu *menu = ( SOG_Menu * )calloc( sizeof( SOG_Menu ), 1 );
    int fakeArgc = 0;

    if( !ogState.Initialised )
        glutInit( &fakeArgc, NULL );

    menu->ParentWindow = ogStructure.Window;

    /* Create a window for the menu to reside in; then hide it. */
    ogCreateWindow( NULL, "OpenGLUT menu", x, y, w, h, OG_CW_MENU );
    menu->Window = ogStructure.Window;
    glutDisplayFunc( ogDisplayMenu );
    glutHideWindow( );
    ogSetWindow( current_window );

    menu->ID       = ++ogStructure.MenuID;
    menu->Callback = menuCallback;
    menu->ActiveEntry = NULL;

    ogListInit( &menu->Entries );
    ogListAppend( &ogStructure.Menus, &menu->Node );

    ogStructure.Menu = menu;

    return menu;
}

/*
 * Function to add a window to the linked list of windows to destroy.
 * Subwindows are also added.
 */
void ogAddToWindowDestroyList( SOG_Window* window )
{
    SOG_WindowList *new_list_entry =
        ( SOG_WindowList* )malloc( sizeof( SOG_WindowList ) );
    new_list_entry->window = window;
    ogListAppend( &ogStructure.WindowsToDestroy, &new_list_entry->node );

    /* Reset the current window, if necessary */
    if( ogStructure.Window == window )
        ogStructure.Window = NULL;

    /*
     * Clear all window callbacks except Destroy, which will
     * be invoked later.  We are potentially carrying
     * out an OpenGLUT operation at the behest of a client callback,
     * so we are reluctant to re-enter the client with the Destroy
     * callback.  The others should nevermore be called.
     */
    {
        OGCBDestroy destroy = FETCH_WCB( *window, Destroy );
        ogClearCallBacks( window );
        SET_WCB( *window, Destroy, destroy );
    }
}

/*
 * Close all the windows in the "WindowsToDestroy" list
 */
void ogCloseWindows( )
{
    while( ogStructure.WindowsToDestroy.First )
    {
        SOG_WindowList *window_ptr = ogStructure.WindowsToDestroy.First;
        ogDestroyWindow( window_ptr->window );
        ogListRemove( &ogStructure.WindowsToDestroy, &window_ptr->node );
        free( window_ptr );
    }
}

/*
 * Destroy a window and all of its subwindows.
 */
void ogDestroyWindow( SOG_Window* window )
{
    int menu_index;

    assert( window );
    freeglut_assert_ready;

    while( window->Children.First )
        ogDestroyWindow( ( SOG_Window * )window->Children.First );

    {
        SOG_Window *activeWindow = ogStructure.Window;
        INVOKE_WCB( *window, Destroy, ( ) );
        ogSetWindow( activeWindow );
    }

    if( window->Parent )
        ogListRemove( &window->Parent->Children, &window->Node );
    else
        ogListRemove( &ogStructure.Windows, &window->Node );

    if( window->ActiveMenu )
        ogDeactivateMenu( window );

    for( menu_index = 0; menu_index < 3; menu_index ++ )
        if( window->Menu[ menu_index ] )
            if ( window == window->Menu[ menu_index]->ParentWindow )
                window->Menu[ menu_index ]->ParentWindow = NULL;

    ogClearCallBacks( window );
    ogCloseWindow( window );
    free( window );

    if( ogStructure.Window == window )
        ogStructure.Window = NULL;
}

/*
 * This is a helper static function that removes a menu (given its pointer)
 * from any windows that can be accessed from a given parent...
 */
static void oghRemoveMenuFromWindow( SOG_Window* window, SOG_Menu* menu )
{
    SOG_Window *subWindow;
    int i;

    /* If menu attached to current window, NULL out the pointer. */
    for( i = 0; i < 3; i++ )
        if( window->Menu[ i ] == menu )
            window->Menu[ i ] = NULL;

    /* Recurse over children. */
    for( subWindow = ( SOG_Window * )window->Children.First;
         subWindow;
         subWindow = ( SOG_Window * )subWindow->Node.Next )
        oghRemoveMenuFromWindow( subWindow, menu );
}

/*
 * This is a static helper function that removes menu references
 * from another menu, given two pointers to them...
 */
static void oghRemoveMenuFromMenu( SOG_Menu* from, SOG_Menu* menu )
{
    SOG_MenuEntry *entry;

    for( entry = ( SOG_MenuEntry * )from->Entries.First;
         entry;
         entry = ( SOG_MenuEntry * )entry->Node.Next )
        if( entry->SubMenu == menu )
            entry->SubMenu = NULL;
}

/*
 * This function destroys a menu specified by the parameter. All menus
 * and windows are updated to make sure no dangling pointers remain.
 */
void ogDestroyMenu( SOG_Menu* menu )
{
    SOG_Window *window;
    SOG_Menu *from;

    assert( menu );
    freeglut_assert_ready;

    /* Remove references to this menu  from all windows: */
    for( window = ( SOG_Window * )ogStructure.Windows.First;
         window;
         window = ( SOG_Window * )window->Node.Next )
        oghRemoveMenuFromWindow( window, menu );

    /* Remove menu entries that lead to this menu */
    for( from = ( SOG_Menu * )ogStructure.Menus.First;
         from;
         from = ( SOG_Menu * )from->Node.Next )
        oghRemoveMenuFromMenu( from, menu );

    if( menu->Destroy )
    {
        SOG_Menu *activeMenu = ogStructure.Menu;
        ogStructure.Menu = menu;
        menu->Destroy( );
        ogStructure.Menu = activeMenu;
    }

    /* Now we can remove its entries. */
    while( menu->Entries.First )
    {
        SOG_MenuEntry *entry = ( SOG_MenuEntry * )menu->Entries.First;

        ogListRemove( &menu->Entries, &entry->Node );

        if( entry->Text )
            free( entry->Text );
        entry->Text = NULL;

        free( entry );
    }

    if( ogStructure.Window == menu->Window )
        ogSetWindow( menu->ParentWindow );

    ogDestroyWindow( menu->Window );
    ogListRemove( &ogStructure.Menus, &menu->Node );

    if( ogStructure.Menu == menu )
        ogStructure.Menu = NULL;

    free( menu );
}

/*
 * This function is called by glutInit().   It prepares the internal
 * structure of OpenGLUT to be used in the application. The structure will be
 * destroyed using ogDestroyStructure() on glutMainLoop() return. In that
 * case further use of OpenGLUT should be preceeded with a glutInit() call.
 */
void ogCreateStructure( void )
{
    ogListInit( &ogStructure.Windows );
    ogListInit( &ogStructure.Menus );
    ogListInit( &ogStructure.WindowsToDestroy );
}

/*
 * This function is automatically called on glutMainLoop() return.
 * It should deallocate and destroy all remnants of previous
 * glutInit()-enforced structure initialization...
 */
void ogDestroyStructure( void )
{
    freeglut_assert_ready;

    /* Clean up the WindowsToDestroy list. */
    ogCloseWindows( );

    /* Make sure all windows and menus have been deallocated */
    while( ogStructure.Menus.First )
        ogDestroyMenu( ( SOG_Menu * )ogStructure.Menus.First );

    while( ogStructure.Windows.First )
        ogDestroyWindow( ( SOG_Window * )ogStructure.Windows.First );
}

/*
 * Helper function to enumerate through all registered top-level windows
 */
void ogEnumWindows( OGCBenumerator enumCallback, SOG_Enumerator *enumerator )
{
    SOG_Window *window;

    assert( enumCallback && enumerator );
    freeglut_assert_ready;

    /* Check every of the top-level windows */
    for( window = ( SOG_Window * )ogStructure.Windows.First;
         window;
         window = ( SOG_Window * )window->Node.Next )
    {
        enumCallback( window, enumerator );
        if( enumerator->found )
            return;
    }
}

/*
 * Helper function to enumerate through all a window's subwindows
 * (single level descent)
 */
void ogEnumSubWindows(
    SOG_Window *window, OGCBenumerator enumCallback, SOG_Enumerator *enumerator
)
{
    SOG_Window *child;

    assert( enumCallback && enumerator );
    freeglut_assert_ready;

    for( child = ( SOG_Window * )window->Children.First;
         child;
         child = ( SOG_Window * )child->Node.Next )
    {
        enumCallback( child, enumerator );
        if( enumerator->found )
            return;
    }
}

/*
 * A static helper function to look for a window given its handle
 */
static void oghcbWindowByHandle(
    SOG_Window *window, SOG_Enumerator *enumerator
)
{
    if( enumerator->found )
        return;

    /* Check the window's handle. */
    if( window->Window.Handle == ( SOG_WindowHandleType )( enumerator->data ) )
    {
        enumerator->found = GL_TRUE;
        enumerator->data = window;
        return;
    }

    /* Otherwise, check this window's children */
    ogEnumSubWindows( window, oghcbWindowByHandle, enumerator );
}

/*
 * ogWindowByHandle returns a {SOG_Window *} value pointing to the
 * first window in the queue matching the specified window handle.
 * The function is defined in og_structure.c file.
 */
SOG_Window *ogWindowByHandle( SOG_WindowHandleType hWindow )
{
    SOG_Enumerator enumerator;

    enumerator.found = GL_FALSE;
    enumerator.data = ( void * )hWindow;
    ogEnumWindows( oghcbWindowByHandle, &enumerator );

    if( enumerator.found )
        return ( SOG_Window * )enumerator.data;

    return NULL;
}

/*
 * A static helper function to look for a window given its ID
 */
static void oghcbWindowByID( SOG_Window *window, SOG_Enumerator *enumerator )
{
    /* Make sure we do not overwrite our precious results... */
    if( enumerator->found )
        return;

    /* Check the window's handle. */
    if( window->ID == *( int * )( enumerator->data ) )
    {
        enumerator->found = GL_TRUE;
        enumerator->data = window;
        return;
    }

    /* Recurse */
    ogEnumSubWindows( window, oghcbWindowByID, enumerator );
}

/*
 * This function is similiar to the previous one, except it is
 * looking for a specified (sub)window identifier. The function
 * is defined in og_structure.c file.
 */
SOG_Window *ogWindowByID( int windowID )
{
    SOG_Enumerator enumerator;

    enumerator.found = GL_FALSE;
    enumerator.data = ( void * ) &windowID;
    ogEnumWindows( oghcbWindowByID, &enumerator );
    if( enumerator.found )
        return ( SOG_Window * ) enumerator.data;

    return NULL;
}

/*
 * Looks up a menu given its ID. This is easier than ogWindowBy*()
 * as all menus are placed in one doubly linked list...
 */
SOG_Menu *ogMenuByID( int menuID )
{
    SOG_Menu *menu = NULL;

    freeglut_assert_ready;

    for( menu = ( SOG_Menu * )ogStructure.Menus.First;
         menu;
         menu = ( SOG_Menu * )menu->Node.Next )
        if( menu->ID == menuID )
            return menu;

    return NULL;
}


void ogListInit( SOG_List *list )
{
    list->First = NULL;
    list->Last = NULL;
}

void ogListAppend( SOG_List *list, SOG_Node *node )
{
    if ( list->Last )
    {
        SOG_Node *ln = ( SOG_Node * )list->Last;
        ln->Next = node;
        node->Prev = ln;
    }
    else
    {
        node->Prev = NULL;
        list->First = node;
    }

    node->Next = NULL;
    list->Last = node;
}

void ogListRemove( SOG_List *list, SOG_Node *node )
{
    if( node->Next )
        ( ( SOG_Node * )node->Next )->Prev = node->Prev;
    if( node->Prev )
        ( ( SOG_Node * )node->Prev )->Next = node->Next;
    if( ( ( SOG_Node * )list->First ) == node )
        list->First = node->Next;
    if( ( ( SOG_Node * )list->Last ) == node )
        list->Last = node->Prev;
}

int ogListLength( SOG_List *list )
{
    SOG_Node *node;
    int length = 0;

    for( node =( SOG_Node * )list->First;
         node;
         node = ( SOG_Node * )node->Next )
        ++length;

    return length;
}

void ogListInsert( SOG_List *list, SOG_Node *next, SOG_Node *node )
{
    SOG_Node *prev;

    node->Next = next;
    if( next )
    {
        prev = next->Prev;
        next->Prev = node;
    }
    else
    {
        prev = list->Last;
        list->Last = node;
    }

    node->Prev = prev;
    if( prev )
        prev->Next = node;
    else
        list->First = node;
}
