
// Author:  Greg "fugue" Santucci, 2015
// Email:   thecodewitch@gmail.com
// Web:     http://createuniverses.blogspot.com/

#include "luaCB.h"

extern float g_fEdLeftMargin;
extern float g_fEdRightMargin;
extern float g_fEdBottomMargin;
extern float g_fEdTopMargin;

int luaCBNewBuffer(lua_State * L)
{
    g_pWorld->NewEditor();

    int n = lua_gettop(L);
    if(n>=1)
    {
        std::string sName = luaL_checkstring(L, 1);
        g_pWorld->GetEditor()->SetName(sName);
    }

    return 0;
}

int luaCBCloseBuffer(lua_State * L)
{
    g_pWorld->CloseEditor();

    return 0;
}

int luaCBSetBufferText(lua_State * L)
{
    int n = lua_gettop(L);
    if(n<1 || n>2) luaL_error(L, "1 or 2 arguments expected.");

    GLEditor * pEditor = g_pWorld->GetEditor();
    std::string sText;

    if(n==2)
    {
        std::string sName = luaL_checkstring(L, 1);

        pEditor = g_pWorld->GetEditor(sName);
        if(pEditor == 0)
            luaL_error(L, "No editor by the name %s", sName.c_str());

        sText = luaL_checkstring(L, 2);
    }
    else
    {
        sText = luaL_checkstring(L, 1);
    }

    pEditor->SetText(sText);

    return 0;
}

int luaCBInsertBufferText(lua_State * L)
{
    // I can just make this take 2 arguments, the first being the buffer, or even just the buffer name.

    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    std::string sText = luaL_checkstring(L, 1);

    g_pWorld->GetEditor()->InsertText(sText);

    return 0;
}

int luaCBGetBufferText(lua_State * L)
{
    lua_pushstring(L, g_pWorld->GetEditor()->GetAllText().c_str());

    return 1;
}

int luaCBLoadBuffer(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    std::string sFilename = luaL_checkstring(L, 1);

    //g_pWorld->NewEditor();
    g_pWorld->GetEditor()->Load(sFilename);

    return 0;
}

int luaCBReadFile(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    std::string sFilename = luaL_checkstring(L, 1);

    std::string sFileText = GLEditor::ReadFileToString(sFilename);

    lua_pushstring(L, sFileText.c_str());
    return 1;
}

int luaCBSaveBuffer(lua_State *L)
{
    int n = lua_gettop(L);
    if(n>=1)
    {
        std::string sFilename = luaL_checkstring(L, 1);
        g_pWorld->GetEditor()->SaveAs(sFilename);
    }
    else
    {
        g_pWorld->GetEditor()->Save();
    }

    return 0;
}

int luaCBGetNumBuffers(lua_State * L)
{
    lua_pushnumber(L, g_pWorld->m_buffers.size());
    return 1;
}

int luaCBGetBufferName(lua_State * L)
{
    int n = lua_gettop(L);
    if(n>=1)
    {
        int id = luaL_checknumber(L, 1);
        if(id < 0 || id >= g_pWorld->m_buffers.size())
        {
            lua_pushstring(L, "???");
        }
        else
        {
            GLEditor * p = g_pWorld->m_buffers[id];
            lua_pushstring(L, p->GetName().c_str());
        }
    }
    else
    {
        lua_pushstring(L, g_pWorld->GetEditor()->GetName().c_str());
    }

    return 1;
}

int luaCBSetBufferName(lua_State * L)
{
    std::string sName = luaL_checkstring(L, 1);
    g_pWorld->GetEditor()->SetName(sName);

    return 0;
}

int luaCBGetParentBufferName(lua_State * L)
{
    lua_pushstring(L, g_pWorld->GetEditor()->GetParentName().c_str());
    return 1;
}

int luaCBSwitchToBuffer(lua_State * L)
{
    std::string sName = luaL_checkstring(L, 1);
    g_pWorld->SwitchToEditor(sName);
    return 0;
}

int luaCBGotoBufferStart(lua_State * L)
{
    g_pWorld->GetEditor()->MoveCursorToStart();

    return 0;
}

int luaCBGotoBufferEnd(lua_State * L)
{
    g_pWorld->GetEditor()->MoveCursorToEnd();

    return 0;
}

int luaCBParseParentheses(lua_State * L)
{
    int bias = luaL_checknumber(L, 1);
    GLEditor::IndexPair parens = g_pWorld->GetEditor()->ParseParentheses(bias);
    lua_pushnumber(L, parens.n1);
    lua_pushnumber(L, parens.n2);


    // Temporary, this should be moved to a separate lua function
    g_pWorld->GetEditor()->m_ParenthesesHighlight[0] = parens.n1;
    g_pWorld->GetEditor()->m_ParenthesesHighlight[1] = parens.n2;
    return 2;
}

int luaCBEdSetHighlight(lua_State * L)
{
    int n1 = luaL_checknumber(L, 1);
    int n2 = luaL_checknumber(L, 2);
    g_pWorld->GetEditor()->m_ParenthesesHighlight[0] = n1;
    g_pWorld->GetEditor()->m_ParenthesesHighlight[1] = n2;
    return 0;
}

int luaCBSetSearchText(lua_State * L)
{
    return 0;
}

int luaCBGetSearchText(lua_State * L)
{
    return 0;
}

int luaCBFindNext(lua_State * L)
{
    return 0;
}

int luaCBFindPrevious(lua_State * L)
{
    return 0;
}

int luaCBGetEditorSelectedText(lua_State * L)
{
    lua_pushstring(L, g_pWorld->GetEditor()->GetText().c_str());

    return 1;
}

int luaCBGetEditorSExpr(lua_State * L)
{
    lua_pushstring(L, g_pWorld->GetEditor()->GetSExpr().c_str());

    return 1;
}

int luaCBGetEditorLineStart(lua_State * L)
{
    // optional position argument
    lua_pushnumber(L, g_pWorld->GetEditor()->LineStart(g_pWorld->GetEditor()->GetPosition()));
    return 1;
}

int luaCBGetEditorLineEnd(lua_State * L)
{
    lua_pushnumber(L, g_pWorld->GetEditor()->LineEnd(g_pWorld->GetEditor()->GetPosition()));
    return 1;
}

int luaCBGetEditorLineText(lua_State * L)
{
    lua_pushstring(L, g_pWorld->GetEditor()->GetCurrentLineText().c_str());
    return 1;
}

int luaCBGotoEditorLine(lua_State * L)
{
    return 0;
}

int luaCBGetEditorLine(lua_State * L)
{
    return 0;
}

int luaCBEdGetRenderMode(lua_State * L)
{
    lua_pushnumber(L, GLEditor::m_nRenderMode);
    return 1;
}

int luaCBEdSetRenderMode(lua_State * L)
{
    int nRenderMode = luaL_checknumber(L, 1);
    GLEditor::SetRenderMode(nRenderMode);
    return 0;
}

int luaCBEdGetFlashRate(lua_State * L)
{
    lua_pushnumber(L, 0);
    return 1;
}

int luaCBEdSetFlashRate(lua_State * L)
{
    return 0;
}

int luaCBEdGetTopPosition(lua_State * L)
{
    lua_pushnumber(L, g_pWorld->GetEditor()->GetFirstVisiblePosition());
    return 1;
}

int luaCBEdGetBottomPosition(lua_State * L)
{
    lua_pushnumber(L, g_pWorld->GetEditor()->GetLastVisiblePosition());
    return 1;
}

int luaCBEdGetVisibleLines(lua_State * L)
{
    lua_pushnumber(L, g_pWorld->GetEditor()->m_VisibleLines);
    return 1;
}

int luaCBEdSetVisibleLines(lua_State * L)
{
    int nVisibleLines = luaL_checknumber(L, 1);
    g_pWorld->GetEditor()->m_VisibleLines = nVisibleLines;
    return 0;
}

int luaCBEdGetPosition(lua_State * L)
{
    lua_pushnumber(L, g_pWorld->GetEditor()->m_Position);
    return 1;
}

int luaCBEdSetPosition(lua_State * L)
{
    int nPosition = luaL_checknumber(L, 1);
    g_pWorld->GetEditor()->m_Position = nPosition;
    return 0;
}

int luaCBEdInsertNewline(lua_State *L)
{
    g_pWorld->GetEditor()->InsertNewline();
    return 0;
}

int luaCBEdGetHighlightAnchor(lua_State * L)
{
    lua_pushnumber(L, g_pWorld->GetEditor()->m_HighlightAnchor);
    return 1;
}

int luaCBEdSetHighlightAnchor(lua_State * L)
{
    int nAnchor = luaL_checknumber(L, 1);
    g_pWorld->GetEditor()->m_HighlightAnchor = nAnchor;
    g_pWorld->GetEditor()->m_Selection = true;
    return 0;
}

int luaCBEdGetLeftPosition(lua_State * L)
{
    lua_pushnumber(L, g_pWorld->GetEditor()->m_LeftTextPosition);
    return 1;
}

int luaCBEdSetVisColumns(lua_State * L)
{
    int arg = luaL_checknumber(L,1);
    GLEditor::m_VisibleColumns = arg;
    GLEditor::m_bUpdateRequired = true;
    return 0;
}

int luaCBEdSetVisLines(lua_State * L)
{
    int arg = luaL_checknumber(L,1);
    GLEditor::m_VisibleLines = arg;
    GLEditor::m_bUpdateRequired = true;
    return 0;
}

int luaCBEdGetVisColumns(lua_State * L)
{
    lua_pushnumber(L, GLEditor::m_VisibleColumns);
    return 1;
}

int luaCBEdGetVisLines(lua_State * L)
{
    lua_pushnumber(L, GLEditor::m_VisibleLines);
    return 1;
}

int luaCBEdSetLeftMargin(lua_State * L)
{
    g_fEdLeftMargin = luaL_checknumber(L, 1);
    return 0;
}

int luaCBEdSetRightMargin(lua_State * L)
{
    g_fEdRightMargin = luaL_checknumber(L, 1);
    return 0;
}

int luaCBEdSetTopMargin(lua_State * L)
{
    g_fEdTopMargin = luaL_checknumber(L, 1);
    return 0;
}

int luaCBEdSetBottomMargin(lua_State * L)
{
    g_fEdBottomMargin = luaL_checknumber(L, 1);
    return 0;
}

int luaEdResizeTexture(lua_State * L)
{
    int arg = luaL_checknumber(L,1);
    GLEditor::m_nDesiredTextureSize = arg;
    GLEditor::m_bUpdateRequired = true;
    return 0;
}

int luaEdForceUpdate(lua_State * L)
{
    GLEditor::m_bUpdateRequired = true;
    return 0;
}

int luaCBGetBufferBB(lua_State * L)
{
    float minX,minY,maxX,maxY;
    g_pWorld->GetEditor()->GetBB(minX,minY,maxX,maxY);
    lua_pushnumber(L, minX);
    lua_pushnumber(L, minY);
    lua_pushnumber(L, maxX);
    lua_pushnumber(L, maxY);
    return 4;
}

int luaCBSelectLines(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=3) luaL_error(L, "3 arguments expected.");

    std::string sText = luaL_checkstring(L, 1);
    //int nNumLines = luaL_checknumber(L, 2);
    int nFirstLine = luaL_checknumber(L, 2);
    int nLastLine  = luaL_checknumber(L, 3);
    //std::string sOutput = World::SelectBeginLines(sText, nNumLines);
    //std::string sOutput = World::SelectEndLines(sText, nNumLines);
    std::string sOutput = GLEditor::ReadLinesFromString(sText, nFirstLine, nLastLine);
    lua_pushstring(L, sOutput.c_str());
    return 1;
}

int luaCBReadFileToString(lua_State * L)
{
    int n = lua_gettop(L);
    if(n!=1) luaL_error(L, "1 argument expected.");

    std::string sFilename = luaL_checkstring(L, 1);
    //int nFirstLine = luaL_checknumber(L, 2);
    //int nLastLine  = luaL_checknumber(L, 3);

    std::string sText = GLEditor::ReadFileToString(sFilename);
    //std::string sLines = GLEditor::ReadLinesFromString(sText, nFirstLine, nLastLine);

    //lua_pushstring(L, sLines.c_str());
    lua_pushstring(L, sText.c_str());

    return 1;
}

int luaCBTurnOnNativeEditorControl(lua_State * L)
{
    GLEditor::m_bNativeControl = true;
    return 0;
}

int luaCBTurnOffNativeEditorControl(lua_State * L)
{
    GLEditor::m_bNativeControl = false;
    return 0;
}

int luaCBEdSetCharWidth(lua_State * L)
{
    GLEditor::m_CharWidth = luaL_checknumber(L, 1);
    return 0;
}

int luaCBEdGetCharWidth(lua_State * L)
{
    lua_pushnumber(L, GLEditor::m_CharWidth);
    return 1;
}

int luaCBEdGetStdCharWidth(lua_State * L)
{
    lua_pushnumber(L, GLEditor::m_PolyGlyph->CharacterWidth('#'));
    return 1;
}

int luaCBEdSetCharHeight(lua_State * L)
{
    GLEditor::m_CharHeight = luaL_checknumber(L, 1);
    return 0;
}

int luaCBEdGetCharHeight(lua_State * L)
{
    lua_pushnumber(L, GLEditor::m_CharHeight);
    return 1;
}

int luaCBEdGetStdCharHeight(lua_State * L)
{
    lua_pushnumber(L, GLEditor::m_PolyGlyph->CharacterHeight('#'));
    return 1;
}

int luaCBEdStrokeCharacter(lua_State * L)
{
    std::string c = luaL_checkstring(L, 1);
    float dx = luaL_checknumber(L, 2);
    float dy = luaL_checknumber(L, 3);
    g_pWorld->GetEditor()->StrokeCharacter(c.c_str()[0], dx, dy);
    return 0;
}

int luaCBEdBackspace(lua_State * L)
{
    g_pWorld->GetEditor()->Backspace();
    return 0;
}

int luaCBEdDelete(lua_State * L)
{
    g_pWorld->GetEditor()->Delete();
    return 0;
}

int luaCBEdUseVertexArrays(lua_State * L)
{
    extern bool g_bPolyGlyphUsesVertexArrays;

    int nOption = luaL_checkinteger(L, 1);
    if(nOption > 0)
    {
        GLEditor::m_PolyGlyph->ClearCache();
        g_bPolyGlyphUsesVertexArrays = true;
    }
    else
    {
        GLEditor::m_PolyGlyph->ClearCache();
        g_bPolyGlyphUsesVertexArrays = false;
    }

    return 0;
}

void luaInitCallbacksEditor()
{
    luaCall("function edKeyDown(k) end");
    luaCall("function edKeyUp(k) end");

    luaCall("function edKeyDownSpecial(k) end");

    //luaCall("function edKeyDown(k) print(\"kd:\" .. k) end");
    //luaCall("function edKeyUp(k) end");

    //luaCall("function edKeyDownSpecial(k) print(\"special:\" .. k) end");

    luaCall("function returnPressed() edInsertNewline() end");

    luaCall("function edRenderChar(c,n,xp,yp) edStrokeCharacter(c,0,0) end");

    lua_register(g_pLuaState, "newBuffer",             luaCBNewBuffer);
    lua_register(g_pLuaState, "closeBuffer",           luaCBCloseBuffer);
    lua_register(g_pLuaState, "setBufferText",         luaCBSetBufferText);
    lua_register(g_pLuaState, "insertBufferText",      luaCBInsertBufferText);
    lua_register(g_pLuaState, "getBufferText",         luaCBGetBufferText);
    lua_register(g_pLuaState, "getNumBuffers",         luaCBGetNumBuffers);
    lua_register(g_pLuaState, "loadBuffer",            luaCBLoadBuffer);
    lua_register(g_pLuaState, "saveBuffer",            luaCBSaveBuffer);
    lua_register(g_pLuaState, "getBufferName",         luaCBGetBufferName);
    lua_register(g_pLuaState, "setBufferName",         luaCBSetBufferName);
    lua_register(g_pLuaState, "getParentBufferName",   luaCBGetParentBufferName);
    lua_register(g_pLuaState, "switchToBuffer",        luaCBSwitchToBuffer);
    lua_register(g_pLuaState, "readFile",              luaCBReadFile);

    lua_register(g_pLuaState, "edParseParentheses",    luaCBParseParentheses);
    lua_register(g_pLuaState, "edSetHighlight",        luaCBEdSetHighlight);

    lua_register(g_pLuaState, "edGetRenderMode",       luaCBEdGetRenderMode);
    lua_register(g_pLuaState, "edSetRenderMode",       luaCBEdSetRenderMode);

    lua_register(g_pLuaState, "edSetVisColumns",       luaCBEdSetVisColumns);
    lua_register(g_pLuaState, "edSetVisLines",         luaCBEdSetVisLines);

    lua_register(g_pLuaState, "edGetVisColumns",       luaCBEdGetVisColumns);
    lua_register(g_pLuaState, "edGetVisLines",         luaCBEdGetVisLines);

    lua_register(g_pLuaState, "edSetLeftMargin",       luaCBEdSetLeftMargin);
    lua_register(g_pLuaState, "edSetRightMargin",      luaCBEdSetRightMargin);
    lua_register(g_pLuaState, "edSetTopMargin",        luaCBEdSetTopMargin);
    lua_register(g_pLuaState, "edSetBottomMargin",     luaCBEdSetBottomMargin);

    lua_register(g_pLuaState, "edResizeTexture",       luaEdResizeTexture);
    lua_register(g_pLuaState, "edForceUpdate",         luaEdForceUpdate);

    lua_register(g_pLuaState, "edGetTopPosition",      luaCBEdGetTopPosition);
    lua_register(g_pLuaState, "edGetBottomPosition",   luaCBEdGetBottomPosition);
    lua_register(g_pLuaState, "edGetLeftPosition",     luaCBEdGetLeftPosition);
    lua_register(g_pLuaState, "edGetPosition",         luaCBEdGetPosition);
    lua_register(g_pLuaState, "edSetPosition",         luaCBEdSetPosition);
    lua_register(g_pLuaState, "edGetAnchor",           luaCBEdGetHighlightAnchor);
    lua_register(g_pLuaState, "edSetAnchor",           luaCBEdSetHighlightAnchor);
    lua_register(g_pLuaState, "edInsertNewline",       luaCBEdInsertNewline);

    lua_register(g_pLuaState, "edGetFlashRate",        luaCBEdGetFlashRate);
    lua_register(g_pLuaState, "edSetFlashRate",        luaCBEdSetFlashRate);

    lua_register(g_pLuaState, "gotoBufferStart",       luaCBGotoBufferStart);
    lua_register(g_pLuaState, "gotoBufferEnd",         luaCBGotoBufferEnd);

    lua_register(g_pLuaState, "getEditorLineStart",    luaCBGetEditorLineStart);
    lua_register(g_pLuaState, "getEditorLineEnd",      luaCBGetEditorLineEnd);
    lua_register(g_pLuaState, "getEditorLineText",     luaCBGetEditorLineText);

    lua_register(g_pLuaState, "setSearchText",         luaCBSetSearchText);
    lua_register(g_pLuaState, "getSearchText",         luaCBGetSearchText);
    lua_register(g_pLuaState, "findNext",              luaCBFindNext);
    lua_register(g_pLuaState, "findPrevious",          luaCBFindPrevious);

    lua_register(g_pLuaState, "getSelectedText",       luaCBGetEditorSelectedText);
    lua_register(g_pLuaState, "getSExpr",              luaCBGetEditorSExpr);
    lua_register(g_pLuaState, "gotoLine",              luaCBGotoEditorLine);
    lua_register(g_pLuaState, "getLineNumber",         luaCBGetEditorLine);

    lua_register(g_pLuaState, "selectLines",           luaCBSelectLines);
    lua_register(g_pLuaState, "readFile",              luaCBReadFileToString);

    lua_register(g_pLuaState, "edNativeControlOn",     luaCBTurnOnNativeEditorControl);
    lua_register(g_pLuaState, "edNativeControlOff",    luaCBTurnOffNativeEditorControl);

    lua_register(g_pLuaState, "edSetCharWidth",        luaCBEdSetCharWidth);
    lua_register(g_pLuaState, "edGetCharWidth",        luaCBEdGetCharWidth);
    lua_register(g_pLuaState, "edGetStdCharWidth",     luaCBEdGetStdCharWidth);

    lua_register(g_pLuaState, "edSetCharHeight",       luaCBEdSetCharHeight);
    lua_register(g_pLuaState, "edGetCharHeight",       luaCBEdGetCharHeight);
    lua_register(g_pLuaState, "edGetStdCharHeight",    luaCBEdGetStdCharHeight);

    lua_register(g_pLuaState, "edStrokeCharacter",     luaCBEdStrokeCharacter);

    lua_register(g_pLuaState, "edUseVertexArrays",     luaCBEdUseVertexArrays);

    lua_register(g_pLuaState, "edBackspace",           luaCBEdBackspace);
    lua_register(g_pLuaState, "edDelete",              luaCBEdDelete);
}
