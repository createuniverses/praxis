#include <GL/openglut.h>

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

#include <sys/stat.h>

#include <stdlib.h>
#include <math.h>
#include <float.h>

#include "UT_String.h"
//#include "UT_Functions.h"
#include "luaInterface.h"

#include "GLEditor.h"
#include "PolyGlyph.h"
#include "assert.h"

// static so we share between workspaces
string GLEditor::m_CopyBuffer;

int GLEditor::m_nRenderMode = GLEditor::RenderMode_Polyglyph;

float GLEditor::m_fFlashRate(0.7f);
//float GLEditor::m_fFlashRate(2.7f);
//float GLEditor::m_fFlashRate(0.05f);

// max time in secs for cursor to "blow up"
float GLEditor::m_fBlowupFlashes(1.8f);

// Need to make these effects settable from lua obviously.
bool GLEditor::m_DoEffects(false);
float GLEditor::m_EffectJiggleSize(0.0);

float GLEditor::m_EffectWaveSize(0.05);
float GLEditor::m_EffectWaveWavelength(1.0);
float GLEditor::m_EffectWaveSpeed(20.0);
float GLEditor::m_EffectWaveTimer(0.0);

float GLEditor::m_EffectRippleSize(0.0);
float GLEditor::m_EffectRippleCenterX(0.0);
float GLEditor::m_EffectRippleCenterY(0.0);
float GLEditor::m_EffectRippleWavelength(1.0);
float GLEditor::m_EffectRippleSpeed(1.0);
float GLEditor::m_EffectRippleTimer(0.0);

float GLEditor::m_EffectSwirlSize(0.0);
float GLEditor::m_EffectSwirlCenterX(0.0);
float GLEditor::m_EffectSwirlCenterY(0.0);
float GLEditor::m_EffectSwirlRotation(1.0);

PolyGlyph * GLEditor::m_PolyGlyph = 0;

GLEditor::GLEditor():
    m_PosX(0),
    m_PosY(0),
    m_Scale(1),
    m_CursorMaxWidth(40.0f),
    m_CursorMaxHeight(40.0f),
    m_Position(0),
    m_HighlightAnchor(0),
    m_DesiredXPos(0),
    m_Selection(false),
    m_CursorWidth(0),
    m_CharWidth(0),
    m_CharHeight(0),
    m_OpenChars("([<{"),
    m_CloseChars(")]>}"),
    m_TopTextPosition(0),
    m_LeftTextPosition(0),
    m_CurrentVisibleColumns(0),
    m_CurrentVisibleLines(0),
    m_Width(0),
    m_Height(0),
    m_Delta(0.0f),
    m_Flash(0.0f),
    m_BlowupCursor(false),
    m_Blowup(0.0f),

    m_TextColourRed(1),
    m_TextColourGreen(1),
    m_TextColourBlue(1),
    m_TextColourAlpha(1),
    m_CursorColourRed(1),
    m_CursorColourGreen(1),
    m_CursorColourBlue(0),
    m_CursorColourAlpha(0.5),
    m_Alpha(1.0),

    //m_DebugBB(true),
    m_DebugBB(false),

    m_DoAutoScale(false),
    //m_DoAutoScale(true),
    m_AutoScaleWidth(70000.0f),
    m_AutoScaleHeight(50000.0f),
    m_AutoScaleError(5000.0f),
    m_AutoScaleDrift(0.9),

    m_MinScale(0.5),
    m_MaxScale(5.0),

    m_YDrift(1.0),

    m_VisibleLines(20),
    m_VisibleColumns(60)
{
    if(m_PolyGlyph == 0)
        m_PolyGlyph = new PolyGlyph("Bitstream-Vera-Sans-Mono.ttf");

    m_CharWidth=StrokeWidth('#')+1;
    m_CharHeight=m_PolyGlyph->CharacterHeight('#') * 1.5;
    m_CursorWidth=m_CharWidth/3.0f;

    m_ParenthesesHighlight[0]=-1;
    m_ParenthesesHighlight[1]=-1;

    m_LuaBlockHighlight[0]=-1;
    m_LuaBlockHighlight[1]=-1;

    m_sName = "Untitled";
    m_sParentName = "";

    m_CurrentVisibleColumns = m_VisibleColumns;
    m_CurrentVisibleLines = m_VisibleLines;

    float minX,minY,maxX,maxY; GetBB(minX,minY,maxX,maxY);
    m_PosY = (maxY-minY)/2 - maxY;

//    std::cout << minX << "," <<
//                 minY << "," <<
//                 maxX << "," <<
//                 maxY << std::endl;
}

GLEditor::~GLEditor()
{
    //delete m_PolyGlyph;
}

void GLEditor::Reset()
{
    m_PosX=m_PosY=0;
    float minX,minY,maxX,maxY; GetBB(minX,minY,maxX,maxY);
    m_PosY = (maxY-minY)/2 - maxY;

    m_Scale=1;
    m_TextColourRed=1;
    m_TextColourGreen=1;
    m_TextColourBlue=1;
    m_BlowupCursor=false;
    m_Blowup=0.0f;
}

void GLEditor::Load(const string & sFilename)
{
    ClearAllText();

    SetText(ReadFileToString(sFilename));
    SetName(sFilename);
}

string GLEditor::ReadFileToString(const string & sFilename)
{
    string text,line;
    std::ifstream file (sFilename.c_str());
    if (file.is_open())
    {
        while ( !file.eof() )
        {
            std::getline(file,line);

            // Prune the '\r' if present
            if(line.length() > 0)
                if(line[line.length()-1] == '\r')
                    line = line.substr(0, line.length()-1);

            line = line + std::string("\n");
            text = text + line;
        }
        file.close();
    }
    return text;
}

string GLEditor::ReadLinesFromString(const string & sText, int nFirstLine, int nLastLine)
{
    string::size_type nBegin = 0;
    for(int i = 1; i < nFirstLine; i++)
    {
        nBegin = sText.find("\n",nBegin)+1;
    }

    string::size_type nEnd = 0;
    for(int i = 0; i < nLastLine; i++)
    {
        nEnd = sText.find("\n",nEnd)+1;
        if(nEnd == 0)
            nEnd = sText.length();
    }

    return sText.substr(nBegin, nEnd - nBegin);
}

static bool FileExists(const std::string& sFilename)
{
    struct stat buf;
    if (stat(sFilename.c_str(), &buf) != -1)
        return true;

    return false;
}

int g_nSaveNumber = 0;

static string MakeSaveFilename()
{
    stringstream ss;
    ss << "save";
    ss << g_nSaveNumber;
    ss << ".lua";

    string sFilename = ss.str();

    return sFilename;
}

static string MakeBackupFilename(const string & sName, int nNumber)
{
    stringstream ss;
#ifdef __PRAXIS_WINDOWS__
    ss << "bak\\";
#endif
#ifdef __PRAXIS_LINUX__
    ss << "bak/";
#endif
    ss << sName;
    ss << ".bak";
    ss << nNumber;

    string sFilename = ss.str();

    return sFilename;
}

static string MakeAvailableBackupFilename(const string & sName)
{
    int i = 1;

    string sFilename = MakeBackupFilename(sName, i);

    while(FileExists(sFilename))
    {
        i++;
        sFilename = MakeBackupFilename(sName, i);
    }

    return sFilename;
}

void GLEditor::SaveAs(const string & sFilename)
{
    std::ofstream file(sFilename.c_str());

    file << m_Text;

    file.close();

    SetName(sFilename);
}

void GLEditor::Save()
{
    if(FileExists(m_sName))
    {
        string sFilename = MakeAvailableBackupFilename(m_sName);

        rename(m_sName.c_str(), sFilename.c_str());
    }

    SaveAs(m_sName);
}

void GLEditor::Reshape(unsigned int w,unsigned int h)
{
    m_Width=w;
    m_Height=h;
}

void GLEditor::BlowupCursor()
{
    m_BlowupCursor=true;
    m_Blowup=0.0f;
}


int GLEditor::GetCurrentLine()
{
    int ret=0;
    for (unsigned int i=0; i<m_Position; i++)
    {
        if (m_Text[i]=='\n') ret++;
    }
    return ret;
}

int GLEditor::GetFirstVisiblePosition()
{
    return m_TopTextPosition;
}

int GLEditor::GetLastVisiblePosition()
{
    // Count m_VisibleLines down from m_TopTextPosition

    int nPosition = m_TopTextPosition;

    for(int i = 0; i < m_VisibleLines-1; i++) // do 1 less line
    {
        nPosition = LineEnd(nPosition) + 1;

        // nPosition must always be a valid position for the string insert function.
        // the insert function accepts from 0 to the length.
        if(nPosition > m_Text.length())
            nPosition = m_Text.length();
    }

    nPosition = LineEnd(nPosition); // Should always be at the end of a line.

    return nPosition;
}

void GLEditor::SetCurrentLine(int line)
{
    m_Position=0;
    int count=0;
    for (unsigned int i=0; i<m_Text.size(); i++)
    {
        if (m_Text[i]=='\n') count++;
        if (count<=line) m_Position++;
    }
    if (m_Position<GetFirstVisiblePosition())
        m_TopTextPosition=LineStart(m_Position);
    if (m_Position>GetLastVisiblePosition())
        m_TopTextPosition=LineEnd(m_TopTextPosition)+1; // This is the offending line!!
    m_Position=LineStart(m_Position);
}

int GLEditor::GetLinePosition(int line)
{
    int position=0;
    int count=0;
    for (unsigned int i=0; i<m_Text.size(); i++)
    {
        if (m_Text[i]=='\n') count++;
        if (count<=line) position++;
    }
    position=LineStart(position);

    return position;
}


void GLEditor::SetText(const string& s)
{
    m_Text=s;

    ProcessTabs();

    Update();

//    m_Position=0;
//    m_PosX=m_PosY=0;
//    //SetCurrentLine(0);
//    m_Selection=false;
}

void GLEditor::InsertText(const string & s)
{
    m_Text.insert(m_Position, s);
    m_Position += s.length();

    if(m_Position>GetLastVisiblePosition())
        m_TopTextPosition=LineEnd(m_TopTextPosition)+1;

    Update();
}

void GLEditor::ClearAllText()
{
    m_Text="";
    m_Position=0;
    m_PosX=m_PosY=0;
    float minX,minY,maxX,maxY; GetBB(minX,minY,maxX,maxY);
    m_PosY = (maxY-minY)/2 - maxY;
    SetCurrentLine(0);
    m_Selection=false;
}

void SetNaturalRasterPos()
{
  GLint viewport[4];
  GLdouble modelMatrix[16];
  GLdouble projMatrix[16];
  GLdouble winX, winY, winZ;

  glGetDoublev(GL_PROJECTION_MATRIX, projMatrix);
  glGetDoublev(GL_MODELVIEW_MATRIX, modelMatrix);
  glGetIntegerv(GL_VIEWPORT, viewport);

  gluProject(0,0,0, modelMatrix, projMatrix, viewport, &winX, &winY, &winZ);

  glRasterPos2d(winX, winY);
}


void GLEditor::StrokeCharacter(wchar_t c, float dx, float dy)
{
    switch(m_nRenderMode)
    {
    case RenderMode_Polyglyph:
        m_PolyGlyph->Render(c,m_TextColourRed,m_TextColourGreen,
                            m_TextColourBlue,m_TextColourAlpha*m_Alpha,
                            dx, dy);
        break;

    case RenderMode_Stroke:
        glLineWidth(1.0f);
        glColor3f(1.0f, 1.0f, 1.0f);
        glPushMatrix();

        glScalef(30,30,30);
        glutStrokeCharacter(GLUT_STROKE_MONO_ROMAN, c);

        glPopMatrix();
        glTranslatef(StrokeWidth(c),0,0);
        glLineWidth(1.0f);
        break;

    case RenderMode_Bitmap:
        glLineWidth(1.0f);
        glColor3f(1.0f, 1.0f, 1.0f);
        glPushMatrix();

        SetNaturalRasterPos();
        glutBitmapCharacter(GLUT_BITMAP_9_BY_15, c);

        glPopMatrix();
        glTranslatef(StrokeWidth(c),0,0);
        glLineWidth(1.0f);
        break;
    }
}

float GLEditor::StrokeWidth(wchar_t c)
{
    return m_PolyGlyph->CharacterWidth(c);
}

string GLEditor::GetText()
{
    //if (m_Selection) return m_Text.substr(m_HighlightStart,m_HighlightEnd-m_HighlightStart);
    if (m_Selection)
        return m_Text.substr(SelectionBegin(),SelectionEnd()-SelectionBegin());
    if(m_LuaBlockHighlight[0] != -1 && m_LuaBlockHighlight[1] != -1)
        return m_Text.substr(m_LuaBlockHighlight[0], m_LuaBlockHighlight[1] - m_LuaBlockHighlight[0]);

    return m_Text;
}

string GLEditor::GetLuaBlock()
{
    if(m_LuaBlockHighlight[0] != -1 && m_LuaBlockHighlight[1] != -1)
        return m_Text.substr(m_LuaBlockHighlight[0], m_LuaBlockHighlight[1] - m_LuaBlockHighlight[0]);

    return "";
}

string GLEditor::GetSelectedText()
{
    if (m_Selection)
        return m_Text.substr(SelectionBegin(),SelectionEnd()-SelectionBegin());

    return "";
}

string GLEditor::GetCurrentLineText()
{
    return GetLineText(m_Position);
}

string GLEditor::GetLineText(int nPosition)
{
    int nStart = LineStart(nPosition);
    int nEnd   = LineEnd(nPosition);

    return m_Text.substr(nStart, nEnd - nStart);
}

string GLEditor::GetLineTextToPosition(int nPosition)
{
    int nStart = LineStart(nPosition);
    int nEnd   = nPosition;

    return m_Text.substr(nStart, nEnd - nStart);
}

string GLEditor::GetLineTextFromPosition(int nPosition)
{
    int nStart = nPosition;
    int nEnd   = LineEnd(nPosition);

    return m_Text.substr(nStart, nEnd - nStart);
}

string GLEditor::GetSExpr()
{
    if (m_ParenthesesHighlight[0]<m_ParenthesesHighlight[1])
    {
        return m_Text.substr(m_ParenthesesHighlight[0],m_ParenthesesHighlight[1]+1-m_ParenthesesHighlight[0]);
    }
    return "";
}

void GLEditor::DrawCharBlock()
{		
    glBegin(GL_QUADS);
    glVertex3f(m_CharWidth * 1.00,  -m_CharHeight * 0.1,    0);
    glVertex3f(m_CharWidth * 1.00,   m_CharHeight * 0.9,    0);
    glVertex3f(0,                    m_CharHeight * 0.9,    0);
    glVertex3f(0,                   -m_CharHeight * 0.1,    0);
    glEnd();
}

void GLEditor::DrawCursor()
{
    if (m_BlowupCursor)
    {
        // set this to zero when starting
        m_Blowup +=m_Delta;
        if (m_Blowup >= m_fBlowupFlashes)
        {
            m_BlowupCursor = false;
        }
        else
        {
            float maxCW = (m_fBlowupFlashes - m_Blowup)/m_fBlowupFlashes*(m_CursorMaxWidth*m_CursorWidth*0.5f)+m_CursorWidth*0.5f;

            float maxCH = (m_fBlowupFlashes - m_Blowup)/m_fBlowupFlashes*(m_CursorMaxHeight*m_CharHeight)+m_CharHeight;
            glColor4f(0,1,0,0.7*m_Blowup/m_fBlowupFlashes);
            glBegin(GL_QUADS);
            glVertex2f(maxCW,-0.5f*(maxCH-m_CharHeight));
            glVertex2f(maxCW,0.5f*(maxCH+m_CharHeight));
            glVertex2f(-maxCW,0.5f*(maxCH+m_CharHeight));
            glVertex2f(-maxCW,-0.5f*(maxCH-m_CharHeight));
            glEnd();
        }
    }
    else
    {
#if 0
        float half = m_CursorWidth/2.0f;
        glBegin(GL_QUADS);
        glVertex2f(half,0);
        glVertex2f(half,m_CharHeight);
        glVertex2f(-half,m_CharHeight);
        glVertex2f(-half,0);
        glEnd();
#else
        m_Flash+=m_Delta;
        if (m_Flash > m_fFlashRate) m_Flash=0;

        if (m_Flash > (m_fFlashRate * 0.5f))
        {
            float half = m_CursorWidth/2.0f;
            glBegin(GL_QUADS);
            glVertex2f(half,0);
            glVertex2f(half,m_CharHeight);
            glVertex2f(-half,m_CharHeight);
            glVertex2f(-half,0);
            glEnd();
        }
#endif
    }
}

void GLEditor::GetBB(float &minX, float &minY, float &maxX, float &maxY)
{
    minX = 0.0f;
    minY = -(m_CurrentVisibleLines-1) * m_CharHeight;
    maxX = m_CurrentVisibleColumns * m_CharWidth;
    maxY = m_CharHeight;
}

void GLEditor::Update()
{
    // Keep m_Position in bounds of the file
    if (m_Position<0) m_Position=0;
    if (m_Position>m_Text.size()) m_Position=m_Text.size();

    // Move the view toward the cursor.
    // The fix will be instant if the cursor is above the view,...
    if (m_Position<m_TopTextPosition)
        m_TopTextPosition=LineStart(m_Position);

    // ...gradual if the cursor is below.
    // Could add a while loop, but I want to see the effect.
    //if(m_Position>GetLastVisiblePosition())
    while(m_Position>GetLastVisiblePosition()) // beware infinite loops?
        m_TopTextPosition=LineEnd(m_TopTextPosition)+1;

    // Set the left text position according to the position of the cursor
    // within the current line
    int nOffset = OffsetToCurrentLineStart();
    if (nOffset>m_VisibleColumns)
        m_LeftTextPosition=nOffset-m_VisibleColumns;
    else
        m_LeftTextPosition=0;

    // Update the bounding box. This used to be done in Render(), but I
    // don't like Render() having any side effects.

    // I should [it it back in render, because at the moment there
    // is duplication

#if 0
//    m_CurrentVisibleColumns = 0;
//    m_CurrentVisibleLines = 1;
    m_CurrentVisibleColumns = m_VisibleColumns;
    m_CurrentVisibleLines   = m_VisibleLines;

    //int n = m_TopTextPosition;
    int lineCharsCount = 0;
    int linesCount = 1; // always at least 1 line

    for(int n=GetFirstVisiblePosition(); n < GetLastVisiblePosition(); n++)
    //while (n<m_Text.size() && m_CurrentVisibleLines<m_VisibleLines)
    {
        if(m_Text[n]=='\n')
        {
            lineCharsCount = 0;
            linesCount++;
        }
        else
        {
            lineCharsCount++;
        }

        m_CurrentVisibleColumns = max(m_CurrentVisibleColumns, lineCharsCount);
        m_CurrentVisibleLines   = max(m_CurrentVisibleLines,   linesCount);

        //n++;
    }
#endif

    m_CurrentVisibleColumns = m_VisibleColumns;
    m_CurrentVisibleLines   = m_VisibleLines;

    m_LuaBlockHighlight[0]=-1;
    m_LuaBlockHighlight[1]=-1;

    // temporarily do this manually
//    IndexPair paren = ParseParentheses();
//    m_ParenthesesHighlight[0] = paren.n1;
//    m_ParenthesesHighlight[1] = paren.n2;

    ParseLuaBlock();

    m_Delta=0.02;

    float minX,minY,maxX,maxY; GetBB(minX,minY,maxX,maxY);
#if 0
    //m_PosY=m_PosY*(1-m_YDrift*m_Delta) - (minY+(maxY-minY)/2)*m_YDrift*m_Delta;

    //m_PosY = (minY+(maxY-minY)/2);

    if (m_DoAutoScale)
    {
        float boxwidth=(maxX-minX)*m_Scale;
        float boxheight=(maxY-minY)*m_Scale;

        // Only make it bigger if they are both too small
        // Always make it smaller if either are too big

        if (boxwidth  > m_AutoScaleWidth+m_AutoScaleError ||
            boxheight > m_AutoScaleHeight+m_AutoScaleError)
            m_Scale*=1-m_AutoScaleDrift*m_Delta;
        else if (boxwidth  < m_AutoScaleWidth-m_AutoScaleError &&
                 boxheight < m_AutoScaleHeight-m_AutoScaleError)
            m_Scale*=1+m_AutoScaleDrift*m_Delta;
    }
    else
    {
        m_Scale=m_MinScale;
    }

    if (m_Scale>m_MaxScale) m_Scale=m_MaxScale; // clamp
    if (m_Scale<m_MinScale) m_Scale=m_MinScale; // clamp

    if (m_DoEffects)
    {
        m_EffectWaveTimer   += m_Delta * m_EffectWaveSpeed;
        m_EffectRippleTimer += m_Delta * m_EffectRippleSpeed;
    }

    m_PosY = (minY+(maxY-minY)/2);
    m_PosY = (-minY+(maxY-minY)/2);
    m_PosY = (maxY-minY)/2;
#endif

    m_Scale=m_MinScale;
}

#if 1

void GLEditor::Render()
{
    glViewport(0, 0, m_Width, m_Height);
    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    glLoadIdentity();
    glOrtho(-50,50,-37.5,37.5,0,10);

    glMatrixMode(GL_MODELVIEW);
    glDisable(GL_TEXTURE_2D);


    glPushMatrix();
    glDisable(GL_LIGHTING);
    glDisable(GL_DEPTH_TEST);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glPolygonMode(GL_FRONT,GL_FILL);

    glLoadIdentity();

    if (m_DebugBB)
    {
        glColor3f(1,1,1);
        glBegin(GL_LINE_LOOP);
        glVertex3f(-49,-36.5,0);
        glVertex3f(49,-36.5,0);
        glVertex3f(49,36.5,0);
        glVertex3f(-49,36.5,0);
        glEnd();

        glColor3f(0,1,0);
        glPushMatrix();
        glTranslatef(-45,0,0);
        glScalef(0.001f,0.001f,1);
        glBegin(GL_LINE_LOOP);
        glVertex3f(0,-m_AutoScaleHeight/2.0f,0);
        glVertex3f(m_AutoScaleWidth,-m_AutoScaleHeight/2.0f,0);
        glVertex3f(m_AutoScaleWidth,m_AutoScaleHeight/2.0f,0);
        glVertex3f(0,m_AutoScaleHeight/2.0f,0);
        glEnd();
        glPopMatrix();
    }

    glTranslatef(-48,0,0);
    //glScalef(0.001f*m_Scale,0.001f*m_Scale,1);
    glScalef(0.001f*m_Scale,0.001f*m_Scale,1);

    // m_PosY is the only one that changes
    glTranslatef(m_PosX,m_PosY,0);

    glPushMatrix();

    if (m_DebugBB)
    {
        float minX,minY,maxX,maxY; GetBB(minX,minY,maxX,maxY);

        glColor3f(1,0,0);
        glBegin(GL_LINE_LOOP);
        glVertex3f(minX,minY,0);
        glVertex3f(maxX,minY,0);
        glVertex3f(maxX,maxY,0);
        glVertex3f(minX,maxY,0);
        glEnd();
    }

    // Draw a background.
    //if(false)
    {
        float minX,minY,maxX,maxY; GetBB(minX,minY,maxX,maxY);

        //glColor3f(0,0,0);
        glColor4f(0,0,0, 0.7);
        glBegin(GL_QUADS);
        glVertex3f(minX,minY,0);
        glVertex3f(maxX,minY,0);
        glVertex3f(maxX,maxY,0);
        glVertex3f(minX,maxY,0);
        glEnd();
    }

    unsigned int xcount=0;
    float xpos=0;
    float ypos=0;
    bool drawncursor=false;

    //unsigned int n=m_TopTextPosition;
    //int lineCount=0;

    for(int n=GetFirstVisiblePosition(); n < GetLastVisiblePosition(); n++)
    //while (n<m_Text.size() && lineCount<m_VisibleLines)
    {
        if (m_Position==n) // draw cursor
        {
            glColor4f(m_CursorColourRed,m_CursorColourGreen,m_CursorColourBlue,m_Alpha*m_CursorColourAlpha);
            DrawCursor();
            glColor4f(0.7,0.7,0.7,1);
            drawncursor=true;
        }

        if(m_Text[n]!='\n')
        {
            if ((int)n>=m_ParenthesesHighlight[0] &&
                    (int)n<=m_ParenthesesHighlight[1]) // draw parentheses highlight
            {
                glColor4f(0,0.5,1,0.5*m_Alpha);
                DrawCharBlock();
                glColor4f(0.7,0.7,0.7,1);
            }

            if ((int)n>=m_LuaBlockHighlight[0] &&
                    (int)n<=m_LuaBlockHighlight[1]) // draw lua block highlight
            {
                glColor4f(0,1,0.5,0.2*m_Alpha);
                DrawCharBlock();
                glColor4f(0.7,0.7,0.7,1);
            }

            if (m_Selection && n>=SelectionBegin() && n<SelectionEnd()) // draw selection highlight
            {
                glColor4f(0,1,0,0.5*m_Alpha);
                DrawCharBlock();
                glColor4f(0.7,0.7,0.7,1);
            }
        }

        if(m_Text[n]=='\n')
        {
            // Put the matrix back to the top left of the document
            glPopMatrix();
            glPushMatrix();

            xpos=0;
            xcount=0;
            ypos-=m_CharHeight;
            //lineCount++;

            // Move the matrix down to the next line to be drawn
            glTranslatef(0,ypos,0);
            //glTranslatef(0,-lineCount * m_CharHeight,0);
        }
        else
        {
            if (xcount>=m_LeftTextPosition && xcount < m_LeftTextPosition + m_VisibleColumns)
            {
                float dx = 0;
                float dy = 0;

                if (m_DoEffects)
                {
                    // current letter coordinate transformed to viewport
                    float xp = -48 + 0.001f * m_Scale * (xpos + m_PosX);
                    float yp = 0.001f * m_Scale * (ypos + m_PosY);
                    /* jiggle */
                    if (fabs(m_EffectJiggleSize) > FLT_EPSILON)
                    {
                        float jdx = 10000 * ((float)rand() / (float)RAND_MAX - .5);
                        float jdy = 10000 * ((float)rand() / (float)RAND_MAX - .5);
                        dx += m_EffectJiggleSize * jdx;
                        dy += m_EffectJiggleSize * jdy;
                    }

                    /* wave */
                    if (fabs(m_EffectWaveSize) > FLT_EPSILON)
                    {
                        dy += m_EffectWaveSize * 10000 * sin(m_EffectWaveTimer +
                                                             .1 * m_EffectWaveWavelength * xp);
                    }

                    /* ripple */
                    if (fabs(m_EffectRippleSize) > FLT_EPSILON)
                    {
                        // center coordinate transformed to viewport
                        float cx = -50.0 + 100.0 * m_EffectRippleCenterX / m_Width;
                        float cy = 37.5 - 75.0 * m_EffectRippleCenterY / m_Height;
                        float rdx = xp - cx;
                        float rdy = yp - cy;

                        float d = m_EffectRippleSize * 200 * sin(m_EffectRippleTimer -
                                                                 .5 * m_EffectRippleWavelength * sqrt(rdx * rdx + rdy * rdy));
                        dx += d * rdx;
                        dy += d * rdy;
                    }

                    /* swirl */
                    if (fabs(m_EffectSwirlSize) > FLT_EPSILON)
                    {
                        float sx = -50.0 + 100.0 * m_EffectSwirlCenterX / m_Width;
                        float sy = 37.5 - 75.0 * m_EffectSwirlCenterY / m_Height;
                        float sdx = xp - sx;
                        float sdy = yp - sy;
                        float a = m_EffectSwirlRotation * exp( - (sdx * sdx + sdy * sdy) /
                                                               (m_EffectSwirlSize * m_EffectSwirlSize));
                        float u =  sdx * cos(a) - sdy * sin(a);
                        float v =  sdx * sin(a) + sdy * cos(a);

                        dx += (sx + u + 48) / (m_Scale * 0.001f)  - xpos - m_PosX;
                        dy += (sy + v) / (m_Scale * 0.001f)  - ypos - m_PosY;
                    }
                }

                StrokeCharacter(m_Text[n], dx, dy);

                xpos+=m_CharWidth;
                //std::cout << xpos << std::endl;
            }

            xcount++;
        }

        //n++;
    }

    // draw cursor if we have no text, or if we're at the end of the buffer
    if (!drawncursor)
    {
        glColor4f(m_CursorColourRed,m_CursorColourGreen,m_CursorColourBlue,m_Alpha*m_CursorColourAlpha);
        DrawCursor();
        glColor4f(0.7,0.7,0.7,1);
    }

    glPopMatrix();
    glPopMatrix();

    glEnable(GL_LIGHTING);
    glEnable(GL_DEPTH_TEST);

    glMatrixMode(GL_PROJECTION);
    glPopMatrix();
    glMatrixMode(GL_MODELVIEW);
}

#else

void GLEditor::Render()
{
    glViewport(0, 0, m_Width, m_Height);
    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    glLoadIdentity();
    glOrtho(-50,50,-37.5,37.5,0,10);

    glMatrixMode(GL_MODELVIEW);
    glDisable(GL_TEXTURE_2D);


    glPushMatrix();
    glDisable(GL_LIGHTING);
    glDisable(GL_DEPTH_TEST);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glPolygonMode(GL_FRONT,GL_FILL);

    glLoadIdentity();

    if (m_DebugBB)
    {
        glColor3f(1,1,1);
        glBegin(GL_LINE_LOOP);
        glVertex3f(-49,-36.5,0);
        glVertex3f(49,-36.5,0);
        glVertex3f(49,36.5,0);
        glVertex3f(-49,36.5,0);
        glEnd();

        glColor3f(0,1,0);
        glPushMatrix();
        glTranslatef(-45,0,0);
        glScalef(0.001f,0.001f,1);
        glBegin(GL_LINE_LOOP);
        glVertex3f(0,-m_AutoScaleHeight/2.0f,0);
        glVertex3f(m_AutoScaleWidth,-m_AutoScaleHeight/2.0f,0);
        glVertex3f(m_AutoScaleWidth,m_AutoScaleHeight/2.0f,0);
        glVertex3f(0,m_AutoScaleHeight/2.0f,0);
        glEnd();
        glPopMatrix();
    }

    glTranslatef(-48,0,0);
    glScalef(0.001f*m_Scale,0.001f*m_Scale,1);

    glTranslatef(m_PosX,m_PosY,0);

    glPushMatrix();

    if (m_DebugBB)
    {
        float minX,minY,maxX,maxY; GetBB(minX,minY,maxX,maxY);

        glColor3f(1,0,0);
        glBegin(GL_LINE_LOOP);
        glVertex3f(minX,minY,0);
        glVertex3f(maxX,minY,0);
        glVertex3f(maxX,maxY,0);
        glVertex3f(minX,maxY,0);
        glEnd();
    }

    unsigned int xcount=0;
    float xpos=0;
    float ypos=0;
    bool drawncursor=false;

    //unsigned int n=m_TopTextPosition;
    //int lineCount=0;

    for(int n=GetFirstVisiblePosition(); n < GetLastVisiblePosition(); n++)
    //while (n<m_Text.size() && lineCount<m_VisibleLines)
    {
        if (m_Position==n) // draw cursor
        {
            glColor4f(m_CursorColourRed,m_CursorColourGreen,m_CursorColourBlue,m_Alpha*m_CursorColourAlpha);
            DrawCursor();
            glColor4f(0.7,0.7,0.7,1);
            drawncursor=true;
        }

        if ((int)n>=m_ParenthesesHighlight[0] &&
                (int)n<=m_ParenthesesHighlight[1]) // draw parentheses highlight
        {
            glColor4f(0,0.5,1,0.5*m_Alpha);
            DrawCharBlock();
            glColor4f(0.7,0.7,0.7,1);
        }

        if ((int)n>=m_LuaBlockHighlight[0] &&
                (int)n<=m_LuaBlockHighlight[1]) // draw lua block highlight
        {
            glColor4f(0,1,0.5,0.2*m_Alpha);
            DrawCharBlock();
            glColor4f(0.7,0.7,0.7,1);
        }

        if (m_Selection && n>=SelectionBegin() && n<SelectionEnd()) // draw selection highlight
        {
            glColor4f(0,1,0,0.5*m_Alpha);
            DrawCharBlock();
            glColor4f(0.7,0.7,0.7,1);
        }

        if(m_Text[n]=='\n')
        {
            // Put the matrix back to the top left of the document
            glPopMatrix();
            glPushMatrix();

            xpos=0;
            xcount=0;
            ypos-=m_CharHeight;
            //lineCount++;

            // Move the matrix down to the next line to be drawn
            glTranslatef(0,ypos,0);
            //glTranslatef(0,-lineCount * m_CharHeight,0);
        }
        else
        {
            if (xcount>=m_LeftTextPosition)
            {
                float dx = 0;
                float dy = 0;

                if (m_DoEffects)
                {
                    // current letter coordinate transformed to viewport
                    float xp = -48 + 0.001f * m_Scale * (xpos + m_PosX);
                    float yp = 0.001f * m_Scale * (ypos + m_PosY);
                    /* jiggle */
                    if (fabs(m_EffectJiggleSize) > FLT_EPSILON)
                    {
                        float jdx = 10000 * ((float)rand() / (float)RAND_MAX - .5);
                        float jdy = 10000 * ((float)rand() / (float)RAND_MAX - .5);
                        dx += m_EffectJiggleSize * jdx;
                        dy += m_EffectJiggleSize * jdy;
                    }

                    /* wave */
                    if (fabs(m_EffectWaveSize) > FLT_EPSILON)
                    {
                        dy += m_EffectWaveSize * 10000 * sin(m_EffectWaveTimer +
                                                             .1 * m_EffectWaveWavelength * xp);
                    }

                    /* ripple */
                    if (fabs(m_EffectRippleSize) > FLT_EPSILON)
                    {
                        // center coordinate transformed to viewport
                        float cx = -50.0 + 100.0 * m_EffectRippleCenterX / m_Width;
                        float cy = 37.5 - 75.0 * m_EffectRippleCenterY / m_Height;
                        float rdx = xp - cx;
                        float rdy = yp - cy;

                        float d = m_EffectRippleSize * 200 * sin(m_EffectRippleTimer -
                                                                 .5 * m_EffectRippleWavelength * sqrt(rdx * rdx + rdy * rdy));
                        dx += d * rdx;
                        dy += d * rdy;
                    }

                    /* swirl */
                    if (fabs(m_EffectSwirlSize) > FLT_EPSILON)
                    {
                        float sx = -50.0 + 100.0 * m_EffectSwirlCenterX / m_Width;
                        float sy = 37.5 - 75.0 * m_EffectSwirlCenterY / m_Height;
                        float sdx = xp - sx;
                        float sdy = yp - sy;
                        float a = m_EffectSwirlRotation * exp( - (sdx * sdx + sdy * sdy) /
                                                               (m_EffectSwirlSize * m_EffectSwirlSize));
                        float u =  sdx * cos(a) - sdy * sin(a);
                        float v =  sdx * sin(a) + sdy * cos(a);

                        dx += (sx + u + 48) / (m_Scale * 0.001f)  - xpos - m_PosX;
                        dy += (sy + v) / (m_Scale * 0.001f)  - ypos - m_PosY;
                    }
                }

                StrokeCharacter(m_Text[n], dx, dy);

                xpos+=m_CharWidth;
            }

            xcount++;
        }

        //n++;
    }

    // draw cursor if we have no text, or if we're at the end of the buffer
    if (!drawncursor)
    {
        glColor4f(m_CursorColourRed,m_CursorColourGreen,m_CursorColourBlue,m_Alpha*m_CursorColourAlpha);
        DrawCursor();
        glColor4f(0.7,0.7,0.7,1);
    }

    glPopMatrix();
    glPopMatrix();

    glEnable(GL_LIGHTING);
    glEnable(GL_DEPTH_TEST);

    glMatrixMode(GL_PROJECTION);
    glPopMatrix();
    glMatrixMode(GL_MODELVIEW);
}

#endif

void GLEditor::MoveViewUp()
{
}

void GLEditor::MoveViewDown()
{
}

void GLEditor::MoveCursorUp()
{
    if ((int)LineStart(m_Position) > 0) // if we're not on the first line
    {
        int nOffset = OffsetToCurrentLineStart();
        m_Position = LineStart(m_Position);
        m_Position--;
        int nLineEnd = m_Position;
        m_Position = LineStart(m_Position);
        int nLineBegin = m_Position;
        if(nLineBegin + nOffset > nLineEnd)
            m_Position = nLineEnd;
        else
            m_Position = nLineBegin + nOffset;
    }

    // This is jumping 2 lines when at the end of the file - investigate!!
//    if ((int)LineStart(m_Position) > 0) // if we're not on the first line
//    {
//        unsigned int previouslinelength=PreviousLineLength(m_Position);
//        if (previouslinelength<m_DesiredXPos) m_Position=LineStart(m_Position)-1; // end of previous
//        else m_Position=LineStart(LineStart(m_Position)-1)+m_DesiredXPos; // start of previous+offset
//    }
}

void GLEditor::MoveCursorDown()
{
    if (LineEnd(m_Position) < m_Text.size()) // if we're not on the last line
    {
        int nOffset = OffsetToCurrentLineStart();
        m_Position = LineEnd(m_Position);
        m_Position++;
        int nLineBegin = m_Position;
        int nLineEnd = LineEnd(m_Position);
        if(nLineBegin + nOffset > nLineEnd)
            m_Position = nLineEnd;
        else
            m_Position = nLineBegin + nOffset;
    }

    // This can't go down to the bottom when on the second last line, and the bottom line doesn't have enough characters!
//    if (LineEnd(m_Position) < m_Text.size()) // if we're not on the last line
//    {
//        unsigned int nextlinelength=NextLineLength(m_Position);
//        if (nextlinelength<m_DesiredXPos) m_Position=LineEnd(LineEnd(m_Position)+1); // end of next
//        else m_Position=LineStart(LineEnd(m_Position)+1)+m_DesiredXPos; // start of next+offset
//    }
}

void GLEditor::MoveCursorLeft()
{
    if (m_Position>0) m_Position--;
    m_DesiredXPos=OffsetToCurrentLineStart();
}

void GLEditor::MoveCursorRight()
{
    if (!m_Text.empty()) m_Position++;
    m_DesiredXPos=OffsetToCurrentLineStart();
}

void GLEditor::MoveCursorToStart()
{
    m_Position = 0;
}

void GLEditor::MoveCursorToEnd()
{
    m_Position = m_Text.length();
}

void GLEditor::MoveCursorToStartOfLine()
{
    m_Position=LineStart(m_Position);
    m_DesiredXPos=OffsetToCurrentLineStart();
}

void GLEditor::MoveCursorToEndOfLine()
{
    m_Position=LineEnd(m_Position);
    m_DesiredXPos=OffsetToCurrentLineStart()+1;
}

void GLEditor::MoveCursorUpAPage()
{
    for (unsigned int n=0; n<m_VisibleLines+1; n++)
    {
        MoveCursorUp();
    }
}

void GLEditor::MoveCursorDownAPage()
{
    for (unsigned int n=0; n<m_VisibleLines+1; n++)
    {
        MoveCursorDown();
    }
}

void GLEditor::InsertNewline()
{
    EraseSelection();

    m_Text.insert(m_Position,"\n");
    m_Position++;

    if(m_Position>GetLastVisiblePosition())
        m_TopTextPosition=LineEnd(m_TopTextPosition)+1;
}

void GLEditor::Backspace()
{
    // Now backspace
    if(m_Position>0)
    {
        m_Text.erase(m_Position-1,1);
        m_Position--;
    }
}


void GLEditor::InsertTab()
{
    m_Text.insert(m_Position,"  ");
    m_Position+=2;
}

void GLEditor::InsertKey(int key)
{
    if(key != 0)
    {
        if (m_Selection)
        {
            m_Text.erase(SelectionBegin(),SelectionEnd()-SelectionBegin());
            if (m_Position>=SelectionEnd())
            {
                m_Position-=SelectionEnd()-SelectionBegin();
            }
            m_Selection=false;
        }

        string temp(" "); temp[0] = key;
        m_Text.insert(m_Position,temp);
        m_Position++;
    }
}

void GLEditor::EraseSelection()
{
    if (m_Selection)
    {
        m_Text.erase(SelectionBegin(),SelectionEnd()-SelectionBegin());
        if (m_Position>=SelectionEnd())
        {
            m_Position-=SelectionEnd()-SelectionBegin();
        }
        m_Selection=false;
    }
}

int GLEditor::GetIndent(int nPosition)
{
    int nCurrentIndent = 0;
    {
        int nPos = LineStart(nPosition);
        while(m_Text[nPos] == ' ' && nPos < LineEnd(nPosition))
        {
            nPos++;
            nCurrentIndent++;
        }
    }
    return nCurrentIndent;
}

void GLEditor::Handle(int key, int special)
{
    int mod = glutGetModifiers();

    // cout << "GLEditor::Handle(" << key << ", " << special << ") mod: " << mod << endl;

    // Activation of selection - not already active and shift is down
    if (key==0 && !m_Selection && mod&GLUT_ACTIVE_SHIFT)
    {
        m_HighlightAnchor=m_Position;
        m_Selection=true;
    }

    // Deactivation of selection - active and shift is up
    if (key==0 && m_Selection && !mod&GLUT_ACTIVE_SHIFT)
    {
        m_Selection=false;
    }

    if (mod&GLUT_ACTIVE_CTRL)
    {
        switch(special)
        {
        // should make these callable from lua
        // then it can be called in response to a funxtion key press,
        // because tablet keyboard does not have home and end

        case GLUT_KEY_HOME:        MoveCursorToStart();   break;
        case GLUT_KEY_END:         MoveCursorToEnd();     break;
        }

        switch (key)
        {
        // Don't have use for a clear all text keyboard shortcut.
        // I can do that with setBufferText("") if I want to.
        // I need Ctrl-Q to interrupt infinite loops.
//        case 17:
//            ClearAllText();
//        break;

        case GLEDITOR_CUT:
            if(m_Selection)
            {
                m_CopyBuffer=m_Text.substr(SelectionBegin(),SelectionEnd()-SelectionBegin());
                EraseSelection();
            }
            break;
        case GLEDITOR_COPY:
            if(m_Selection)
                m_CopyBuffer=m_Text.substr(SelectionBegin(),SelectionEnd()-SelectionBegin());
            break;
        case GLEDITOR_PASTE:
            m_Text.insert(m_Position,m_CopyBuffer);
            m_Selection=false;
            m_Position+=m_CopyBuffer.size();
            break;
        case GLEDITOR_SAVE:
            Save();
            break;

        /*case GLEDITOR_PLUS: // zoom in
            m_Scale*=1.1f;
        break;
        case GLEDITOR_MINUS: // zoom out
            m_Scale/=1.1f;
        break;*/

        default: break;
        }
    }
    else
    {
        switch(special)
        {
        case GLUT_KEY_HOME:        MoveCursorToStartOfLine();   break;
        case GLUT_KEY_END:         MoveCursorToEndOfLine();     break;
        case GLUT_KEY_RIGHT:       MoveCursorRight();           break;
        case GLUT_KEY_LEFT:        MoveCursorLeft();            break;
        case GLUT_KEY_UP:          MoveCursorUp();              break;
        case GLUT_KEY_DOWN:        MoveCursorDown();            break;
        case GLUT_KEY_PAGE_UP:     MoveCursorUpAPage();         break;
        case GLUT_KEY_PAGE_DOWN:   MoveCursorDownAPage();       break;
        case GLUT_KEY_TAB:         InsertTab();                 break;
        }

        switch(key)
        {
        case '\t':
        {
            InsertTab();
        }
        break;

        case GLEDITOR_DELETE:
        {
            if (m_Selection)
            {
                EraseSelection();
            }
            else
            {
                m_Text.erase(m_Position,1);
            }
        }
        break;

        case GLEDITOR_BACKSPACE:
        {
            if (m_Selection)
            {
                EraseSelection();
            }
            else
            {
                if(m_Position>0)
                {
                    m_Text.erase(m_Position-1,1);
                    m_Position--;
                }
            }
        }
        break;

        case 27: // Esc - app will quit anyway
        {
            // panic editor reset :)
            m_Position=0;
            m_TopTextPosition=0;
        }
        break;

        case GLEDITOR_RETURN:
        {
            luaCall("returnPressed()");

            // Now, reimplement everything below more cleanly in Lua
            // InsertNewline();
#if 0

            // ok, need a test suite for all this
            // then rewrite this in Lua

            int nStandardIndentIncrement = 2;

            int nCurrentIndent = GetIndent(m_Position);
            std::vector<std::string> sTokens;
            // Need a get current line string function
            std::string sLine = GetCurrentLineText();
            utCommaSeparatedToStringVector(sLine, sTokens);
            // GetTokens(m_Position, &sTokens);


            std::string sFirstToken;
            if(sTokens.size() > 0) sFirstToken = sTokens[0];
            std::cout << "First token = " << sFirstToken << std::endl;

//            bool bFirstTokenIsLambda = false;
//            bool bMatchingTokenIsLambda;
            // Questions I'll assume I have an answer to
            bool bOuterMatchingParen = false;
            int nOuterMatchingIndent = 0;
            std::string sOuterMatchingToken;
            bool bOuterMatchingTokenIsLambda = false;
            {
                InsertKey(')');
                ParseParentheses();

                // I *really*, REALLY should put this in a function
                // GetIndent
                // I've already mentioned this.
                // "In programming just as in sorcery and magic, if you give it a name, you can control it."
                bOuterMatchingParen = m_ParenthesesHighlight[0] < m_ParenthesesHighlight[1];
                if(bOuterMatchingParen)
                {
//                    int nPos = LineStart(m_ParenthesesHighlight[0]);
//                    while(m_Text[nPos] == ' ' && nPos < LineEnd(m_ParenthesesHighlight[0]))
//                    {
//                        nPos++;
//                        nOuterMatchingIndent++;
//                    }

                    nOuterMatchingIndent = m_ParenthesesHighlight[0] - LineStart(m_ParenthesesHighlight[0]);

//                    if(m_Text[nPos] != ' ')
//                    {
//                        while(m_Text[nPos] != ' ' && nPos < LineEnd(m_ParenthesesHighlight[0]))
//                        {
//                            sOuterMatchingToken = sOuterMatchingToken + m_Text[nPos];
//                            nPos++;
//                        }
//                    }

                    std::vector<std::string> sTokens;
                    std::string sLine = GetLineTextFromPosition(m_ParenthesesHighlight[0]);
                    utCommaSeparatedToStringVector(sLine, sTokens);
                    if(sTokens.size() > 0) sOuterMatchingToken = sTokens[0];

                    if(sOuterMatchingToken == "(lambda")
                        bOuterMatchingTokenIsLambda = true;
                    if(sOuterMatchingToken == "(let")
                        bOuterMatchingTokenIsLambda = true;
                    if(sOuterMatchingToken == "(define-macro")
                        bOuterMatchingTokenIsLambda = true;
                    if(sOuterMatchingToken == "(define-bacro")
                        bOuterMatchingTokenIsLambda = true;
                }

                // Now backspace
                if(m_Position>0)
                {
                    m_Text.erase(m_Position-1,1);
                    m_Position--;
                }

                // mini emacs?

                // Parse parentheses again for the benefit of subsequent calculations
                ParseParentheses();
                // todo ParseParentheses needs to reset results if no
                // parens found - done
            }

//            int nSecondTokenIndent = -1;
//            {
//                int nPos = LineStart(m_Position) + nCurrentIndent;
//                while(m_Text[nPos] != ' ' && nPos < LineEnd(m_Position))
//                {
//                    nPos++;
//                }

//                if(m_Text[nPos] == ' ')
//                {
//                    while(m_Text[nPos] == ' ' && nPos < LineEnd(m_Position))
//                    {
//                        nPos++;
//                    }
//                    nSecondTokenIndent = nPos - LineStart(m_Position);
//                }
//            }

            bool bMatchingParen = m_ParenthesesHighlight[0] < m_ParenthesesHighlight[1];
            int nMatchingIndent = m_ParenthesesHighlight[0] - LineStart(m_ParenthesesHighlight[0]);

            std::cout << "(" << m_ParenthesesHighlight[0] << "," << m_ParenthesesHighlight[1] << ")" << std::endl;

            int nLastTokenIndent = -1;
            {
                int nPos = LineEnd(m_Position) - 1;
                if(m_Text[nPos] != ' ')
                {
                    while(m_Text[nPos] != ' ' && nPos >= LineStart(m_Position))
                    {
                        nPos--;
                    }

                    nPos++;
                    nLastTokenIndent = nPos - LineStart(m_Position);

                    if(nLastTokenIndent == nCurrentIndent)
                    {
                        nLastTokenIndent = -1;
                    }

                    if(nLastTokenIndent != -1)
                    {
                        bool bStartOfLastTokenIsLeftParen = m_Text[nLastTokenIndent] == '(';
                        if(!(bMatchingParen && (nMatchingIndent == nLastTokenIndent)))
                        {
                            if(bStartOfLastTokenIsLeftParen)
                            {
                                nLastTokenIndent = nLastTokenIndent + nStandardIndentIncrement;
                            }
                        }
                    }
                }
            }

            int nNewIndent = nCurrentIndent + nStandardIndentIncrement;

            bool bMatchingParenOnDifferentLine = bMatchingParen && (LineStart(m_ParenthesesHighlight[0]) != LineStart(m_Position));
            bool bMatchingParenOnCurrentLine   = bMatchingParen && (LineStart(m_ParenthesesHighlight[0]) == LineStart(m_Position));
            bool bStartOfLineIsLeftParen       = m_Text[LineStart(m_Position) + nCurrentIndent] == '(';

            //bool bMatchingParenAtStartOfLine   = bMatchingParen && (m_ParenthesesHighlight[0] == (LineStart(m_Position) + nCurrentIndent));

            // I should add a GetIndent(int nPosition) function
            // Also:
            // GetLineNumber(int nPosition)
            // GetIndent(int nLineNumber)

            // Add Lua handlers here!!
            // FOR YOUR HEALTH
            InsertNewline();

            // Lisp indentation scheme
            //
            // For now, lets assume return is pressed with the cursor at the end of the line.
            // We'll handle the other case later.
            //
            // If there is a matching left paren on a different line, use its indentation on the new line.
            // If there is no matching left paren, then the indentation of the next line is a step deeper than this line.
            //
            // That's it.
            //
            // do sicp in lua
            //

            int nIndent = 0;
            if(bMatchingParenOnDifferentLine)
            {
                nIndent = nMatchingIndent;
            }
            else if(bMatchingParenOnCurrentLine)
            {
                // if(nCurrentIndent == 0) // this should be irrelevant
                // if you give it a name, you can control it.

                std::cout << "sOuterMatchingToken " << sOuterMatchingToken << std::endl;

                // If the first token is define, then
                if(sOuterMatchingToken == "(define") // or lambda, let, define-macro, define - isfirst tokenlambda
                {
                    std::cout << "nOuterMatchingIndent " << nOuterMatchingIndent << std::endl;
                    std::cout << "nStandardIndentIncrement " << nStandardIndentIncrement << std::endl;
                    nIndent = nOuterMatchingIndent + nStandardIndentIncrement;
                }
                else
                {
                    nIndent = nMatchingIndent;
                }
            }
            else
            {
                // No matching paren.
                // If there is a left paren at the start of the line
                //   If there are other tokens, use the new indent of the last token.
                //   If there are no other tokens, use the new indent.
                // If no left paren at the start of the line, use the current indent.

                if(bStartOfLineIsLeftParen)
                {
                    if(nLastTokenIndent != -1)
                    {
                        nIndent = nLastTokenIndent;
                    }
                    else
                    {
                        nIndent = nNewIndent;
                    }
                }
                else
                {
                    nIndent = nCurrentIndent;
                }
            }

            for(int i = 0; i < nIndent; i++)
            {
                InsertKey(' ');
            }
#endif
        }
        break;

        default: InsertKey(key); break;
        }
    }

    Update();
}

void GLEditor::ProcessTabs()
{
    size_t pos=m_Text.find("\t",0);
    while (pos!=string::npos)
    {
        m_Text.erase(pos,1);
        m_Text.insert(pos,"  ");
        pos=m_Text.find("\t",pos);
    }
}


int GLEditor::OffsetToCurrentLineStart()
{
    return m_Position-LineStart(m_Position);
}

int GLEditor::NextLineLength(int pos)
{
    size_t nextlinestart=m_Text.find("\n",m_Position);
    if (nextlinestart!=string::npos)
    {
        return LineLength(nextlinestart+1);
    }
    return 0;

}

int GLEditor::PreviousLineLength(int pos)
{
    size_t previouslineend=string::npos;
    if (pos>0) previouslineend=m_Text.rfind("\n",pos-1);
    if (previouslineend!=string::npos)
    {
        return LineLength(previouslineend);
    }
    return 0;
}

int GLEditor::LineLength(int pos)
{
    unsigned int linestart=LineStart(pos);
    unsigned int lineend=LineEnd(pos);
    return lineend-linestart;
}

unsigned int GLEditor::LineStart(int pos)
{
    if(pos > m_Text.size())
        pos = m_Text.size();

    unsigned int linestart=string::npos;

    if (pos>0)
    {
        if(pos >= m_Text.size())
        {
            linestart=m_Text.rfind("\n",m_Text.size());
        }
        else
        {
            // take one off if we're over a newline
            if (m_Text[pos]=='\n') linestart=m_Text.rfind("\n",pos-1);
            else linestart=m_Text.rfind("\n",pos);
        }
    }

    if (linestart!=string::npos)
        linestart++; // move the start off the newline
    else
        linestart=0; // if are on the first line, set the start to 0

    return linestart;
}

unsigned int GLEditor::LineEnd(int pos)
{
    if (m_Text.empty()) return 0;
    size_t end = m_Text.find("\n",pos);
    if (end==string::npos) end=m_Text.size();
    return end;
}

void GLEditor::ParseLuaBlock()
{
    m_LuaBlockHighlight[0]=-1;
    m_LuaBlockHighlight[1]=-1;
    //return;

//    if(m_Position >= m_Text.size())
//        return;

    // search backward from current cursor position until:
    // "function" or "do" found at the beginning of a line
    // search forward until an "end" is found at the beginning
    // of a line.

    {
        int nSamplePos = LineStart(m_Position);
        bool bFirstLine = true;
        bool bTopReached = false;
        bool bMatch = false;
        bool bNoMatch = false;
        while(bMatch == false && bNoMatch == false && !bTopReached)
        {
            if(m_Text.substr(nSamplePos, 8) == "function") bMatch = true;
            if(m_Text.substr(nSamplePos, 3) == "for")      bMatch = true;
            if(m_Text.substr(nSamplePos, 2) == "do")       bMatch = true;
            if(m_Text.substr(nSamplePos, 2) == "if")       bMatch = true;
            if(m_Text.substr(nSamplePos, 5) == "while")    bMatch = true;

            if(!bFirstLine)
                if(m_Text.substr(nSamplePos, 3) == "end")
                    bNoMatch = true;

            if(!bMatch)
            {
                if(nSamplePos == 0)
                    bTopReached = true;

                // move up a line
                nSamplePos--;
                nSamplePos = LineStart(nSamplePos);

                bFirstLine = false;
            }
        }

        if(bMatch)
            m_LuaBlockHighlight[0] = nSamplePos;
    }

    {
//        int nSamplePos = LineEnd(m_Position);
//        nSamplePos++;
        int nSamplePos = LineStart(m_Position);
        bool bFirstLine = true;
        bool bBottomReached = false;
        bool bMatch = false;
        bool bNoMatch = false;
        while(bMatch == false && bNoMatch == false && !bBottomReached)
        {
            if(!bFirstLine)
            {
                if(m_Text.substr(nSamplePos, 8) == "function") bNoMatch = true;
                if(m_Text.substr(nSamplePos, 3) == "for")      bNoMatch = true;
                if(m_Text.substr(nSamplePos, 2) == "do")       bNoMatch = true;
                if(m_Text.substr(nSamplePos, 2) == "if")       bNoMatch = true;
                if(m_Text.substr(nSamplePos, 5) == "while")    bNoMatch = true;
            }

            if(m_Text.substr(nSamplePos, 3) == "end")      bMatch = true;

            if(!bMatch)
            {
                // move down a line
                nSamplePos = LineEnd(nSamplePos);
                nSamplePos++;

                if(nSamplePos >= m_Text.size())
                    bBottomReached = true;

                bFirstLine = false;
            }
        }

        nSamplePos = LineEnd(nSamplePos);

        if(bMatch)
            m_LuaBlockHighlight[1] = nSamplePos;
    }

}

GLEditor::IndexPair GLEditor::ParseParentheses(int bias)
{
//    IndexPair p;
//    m_ParenthesesHighlight[0]=-1;
//    m_ParenthesesHighlight[1]=-1;

    if(m_Position < m_Text.size())
    {
        // parse the parentheses
        int type=0;
        for (string::iterator i=m_OpenChars.begin(); i!=m_OpenChars.end(); i++)
        {
            if (m_Text[m_Position]==*i) return ParseOpenParentheses(m_Position,type, bias);
            type++;
        }
    }

    if (m_Position > 0)
    {
        int type=0;
        for (string::iterator i=m_CloseChars.begin(); i!=m_CloseChars.end(); i++)
        {
            if (m_Text[m_Position-1]==*i) return ParseCloseParentheses(m_Position-1,type, bias);
            type++;
        }
    }

    return IndexPair();
}

GLEditor::IndexPair GLEditor::ParseOpenParentheses(int pos, int type, int bias)
{
    IndexPair p;
    // looking for a close, so search forward
    int stack=bias, start_pos = pos;
    pos++;
    while(stack!=-1 && pos<(int)m_Text.size())
    {
        if (m_Text[pos]==m_OpenChars[type]) stack++;
        if (m_Text[pos]==m_CloseChars[type]) stack--;
        pos++;
    }
    if (stack==-1)
    {
        p.n1 = start_pos;
        p.n2 = pos-1;

//        m_ParenthesesHighlight[0]=start_pos;
//        m_ParenthesesHighlight[1]=pos-1;
    }

    return p;
}

GLEditor::IndexPair GLEditor::ParseCloseParentheses(int pos, int type, int bias)
{
    IndexPair p;
    // looking for a open, so search backward
    int stack=bias, start_pos = pos;
    pos--;
    while(stack!=-1 && pos>=0)
    {
        if (m_Text[pos]==m_CloseChars[type]) stack++;
        if (m_Text[pos]==m_OpenChars[type]) stack--;
        pos--;
    }
    if (stack==-1)
    {
        p.n1 = pos+1;
        p.n2 = start_pos;

//        m_ParenthesesHighlight[0]=pos+1;
//        m_ParenthesesHighlight[1]=start_pos;
    }

    return p;
}
