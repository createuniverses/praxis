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

#define GLEDITOR_DELETE 127
#define GLEDITOR_BACKSPACE 8

#define GLEDITOR_RETURN 13
#define GLEDITOR_CUT 24 
#define GLEDITOR_COPY 3 
#define GLEDITOR_PASTE 22 
#define GLEDITOR_PLUS 61 
#define GLEDITOR_MINUS 45 
#define GLEDITOR_SAVE 19

using namespace std;

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
    void Backspace();

    void InsertTab();
    void InsertKey(int key);

	void Reshape(unsigned int w,unsigned int h);
	void BlowupCursor();

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
	void Reset();

    void Load(const string & sFilename);
    void Save();
    void SaveAs(const string & sFilename);

    string GetName()                             { return m_sName;  }
    void   SetName(const string & sName)         { m_sName = sName; }

    string GetParentName()                             { return m_sParentName;        }
    void   SetParentName(const string & sParentName)   { m_sParentName = sParentName; }

    static string ReadFileToString(const string & sFilename);
    static string ReadLinesFromString(const string & sText, int nFirstLine, int nLastLine);
    //static bool   FileExists(const std::string& sFilename);

	float m_PosX,m_PosY;
	float m_Scale;
	float m_CursorMaxWidth;
	float m_CursorMaxHeight;

    int m_Width;
    int m_Height;

    void StrokeCharacter(wchar_t c, float dx = 0, float dy = 0);
    float StrokeWidth(wchar_t c);

    static PolyGlyph *m_PolyGlyph;

    float m_TextColourRed;
    float m_TextColourGreen;
    float m_TextColourBlue;
    float m_TextColourAlpha;
    float m_CursorColourRed;
    float m_CursorColourGreen;
    float m_CursorColourBlue;
    float m_CursorColourAlpha;
    float m_Alpha;

    bool m_DoAutoScale;
    float m_AutoScaleWidth;
    float m_AutoScaleHeight;
    float m_AutoScaleError;

    float m_AutoScaleDrift;
    float m_MinScale;
    float m_MaxScale;

    float m_YDrift;

    unsigned int m_VisibleLines;
    unsigned int m_VisibleColumns;

    static float m_fFlashRate;
    static float m_fBlowupFlashes;

    static bool m_DoEffects;
    static float m_EffectJiggleSize;

    static float m_EffectWaveSize;
    static float m_EffectWaveWavelength;
    static float m_EffectWaveSpeed;
    static float m_EffectWaveTimer;

    static float m_EffectRippleSize;
    static float m_EffectRippleCenterX;
    static float m_EffectRippleCenterY;
    static float m_EffectRippleWavelength;
    static float m_EffectRippleSpeed;
    static float m_EffectRippleTimer;

    static float m_EffectSwirlSize;
    static float m_EffectSwirlCenterX;
    static float m_EffectSwirlCenterY;
    static float m_EffectSwirlRotation;

    static int m_nRenderMode;
    enum RenderModes
    {
        RenderMode_Polyglyph,
        RenderMode_Stroke,
        RenderMode_Bitmap
    };

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
	int OffsetToCurrentLineStart();
	int NextLineLength(int pos);
	int PreviousLineLength(int pos);
	int LineLength(int pos);
	unsigned int LineStart(int pos);
	unsigned int LineEnd(int pos);
    IndexPair ParseParentheses(int bias = 0);
    IndexPair ParseOpenParentheses(int pos, int type, int bias = 0);
    IndexPair ParseCloseParentheses(int pos, int type, int bias = 0);

    void ParseLuaBlock();

	int GetCurrentLine();
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


    //void BBExpand(float x, float y);
    //void BBClear() { m_BBMinX=m_BBMinY=m_BBMaxX=m_BBMaxY=0; }

    string m_sName;
    string m_sParentName;

	string m_Text;
	static string m_CopyBuffer;
	unsigned int m_Position;
    unsigned int m_HighlightAnchor;
    unsigned int SelectionBegin() { if(m_Position < m_HighlightAnchor) return m_Position; else return m_HighlightAnchor; }
    unsigned int SelectionEnd() { if(m_Position > m_HighlightAnchor) return m_Position; else return m_HighlightAnchor; }
	unsigned int m_DesiredXPos;
	bool m_Selection;
	float m_CursorWidth;
	float m_CharWidth;
	float m_CharHeight;
    int m_ParenthesesHighlight[2];
    int m_LuaBlockHighlight[2];
    string m_OpenChars;
	string m_CloseChars;
	unsigned int m_LeftTextPosition;
	unsigned int m_TopTextPosition;

    int m_CurrentVisibleColumns;
    int m_CurrentVisibleLines;

    bool m_DebugBB;
    void GetBB(float & minX, float & minY, float & maxX, float & maxY);

	float m_Delta;
	float m_Flash;
	bool m_BlowupCursor;
	float m_Blowup;
};

#endif
