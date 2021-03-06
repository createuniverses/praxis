// Copyright (C) 2005 Dave Griffiths
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#ifndef _GL_EDITOR_H_
#define _GL_EDITOR_H_

#include <string>
#include "PolyGlyph.h"

using namespace std;

class PraxisTexture;

class GLEditor
{
public:
	GLEditor();
	virtual ~GLEditor();

    void Update();
    void Render();

    // Phase this out, move it more and more into Lua.
    void Handle(int key, int special);

    void InsertNewline();
    void Delete();
    void Backspace(); // probe for references

    void InsertTab();
    void InsertKey(int key);

	void Reshape(unsigned int w,unsigned int h);

	string GetText();
	string GetAllText() { return m_Text; }
    string GetLuaBlock();
    string GetSelectedText();
    string GetCurrentLineText();
    string GetLineText(int nPosition);
    string GetLineTextToPosition(int nPosition);
    string GetLineTextFromPosition(int nPosition);
    string GetSExpr();
	void ClearAllText();

    int GetPosition() { return m_Position; }

	void SetText(const string& s);
    void InsertText(const string & s);
    void InsertTextAt(const string &s, int pos);

    void Load(const string & sFilename);
    void Save();
    void SaveAs(const string & sFilename);

    string GetName()                             { return m_sName;  }
    void   SetName(const string & sName)         { m_sName = sName; }

    string GetParentName()                             { return m_sParentName;        }
    void   SetParentName(const string & sParentName)   { m_sParentName = sParentName; }

    static string ReadFileToString(const string & sFilename);
    static string ReadLinesFromString(const string & sText, int nFirstLine, int nLastLine);

    int m_Width;
    int m_Height;

    void StrokeCharacter(wchar_t c, float dx = 0, float dy = 0);
    //float StrokeWidth(wchar_t c);

    void SetCharColor(int r, int g, int b, int a);
    float m_fCharRed;
    float m_fCharGreen;
    float m_fCharBlue;
    float m_fCharAlpha;

    static PolyGlyph *m_PolyGlyph;

    static int m_VisibleLines;
    static int m_VisibleColumns;

    static int m_nRenderMode;
    enum RenderMode
    {
        RenderMode_Direct_Polyglyph,
        RenderMode_Direct_Stroke,
        RenderMode_Direct_Bitmap,
        RenderMode_Texture_Polyglyph,
        RenderMode_Texture_Stroke,
        RenderMode_Texture_Bitmap
    };

    static void SetRenderMode(int nRenderMode);
    static void ResizeTexture(int nNewSize);

    static bool m_bUpdateRequired;

    static PraxisTexture * m_pTexture;
    static int m_nDesiredTextureSize;

    static float m_CursorWidth;
    static float m_CharWidth;
    static float m_CharHeight;

    static bool m_bNativeControl;

//protected:

    struct IndexPair
    {
        IndexPair() { n1 = -1; n2 = -1; }
        int n1;
        int n2;
    };

	void DrawCharBlock();
	void DrawCursor();
	void ProcessTabs();
    IndexPair ParseParentheses(int bias = 0);
    IndexPair ParseOpenParentheses(int pos, int type, int bias = 0);
    IndexPair ParseCloseParentheses(int pos, int type, int bias = 0);

    void ParseLuaBlock();

    int LineLength(int pos);
    unsigned int LineStart(int pos);
    unsigned int LineEnd(int pos);

    int GetLine(int nPosition);
    int GetColumn(int nPosition);
    int GetUp(int nPosition);
    int GetDown(int nPosition);
    int GetLeft(int nPosition);
    int GetRight(int nPosition);

    int CurrentLine();
    int CurrentColumn();

	void SetCurrentLine(int line);
    int GetLinePosition(int line);
    int GetFirstVisiblePosition();
    int GetLastVisiblePosition();

    void MoveViewUp();
    void MoveViewDown();

    void MoveCursorUp();
    void MoveCursorDown();
    void MoveCursorLeft();
    void MoveCursorRight();

    void MoveCursorToStart();
    void MoveCursorToEnd();

    void MoveCursorToStartOfLine();
    void MoveCursorToEndOfLine();

    void MoveCursorUpAPage();
    void MoveCursorDownAPage();

    void EraseSelection();

    int GetIndent(int nPosition);

    string m_sName;
    string m_sParentName;

	string m_Text;
	static string m_CopyBuffer;

	unsigned int m_Position;
    unsigned int m_HighlightAnchor;
    unsigned int SelectionBegin() { if(m_Position < m_HighlightAnchor) return m_Position; else return m_HighlightAnchor; }
    unsigned int SelectionEnd() { if(m_Position > m_HighlightAnchor) return m_Position; else return m_HighlightAnchor; }
	bool m_Selection;
    int m_ParenthesesHighlight[2];
    int m_LuaBlockHighlight[2];
    string m_OpenChars;
	string m_CloseChars;

	unsigned int m_LeftTextPosition;
	unsigned int m_TopTextPosition;

    void GetBB(float & minX, float & minY, float & maxX, float & maxY);

    void RenderBuffer(bool bBackground);
    void RenderTexture();
    void RenderChars();
};

#endif
