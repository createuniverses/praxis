-- renderchars.lua

--[[
    glPushMatrix();

    unsigned int xcount=0;
    float xpos=0;
    float ypos=0;
    bool drawncursor=false;

    for(int n=GetFirstVisiblePosition(); n < GetLastVisiblePosition(); n++)
    {
        if(m_Text[n]!='\n')
        {
            if ((int)n>=m_ParenthesesHighlight[0] &&
                    (int)n<=m_ParenthesesHighlight[1]) // draw parentheses highlight
            {
                glColor4f(0,0,1,0.5f);
                DrawCharBlock();
                glColor4f(0.7,0.7,0.7,1);
            }

            if ((int)n>=m_LuaBlockHighlight[0] &&
                    (int)n<=m_LuaBlockHighlight[1]) // draw lua block highlight
            {
                //glColor4f(0,1,1,0.5f);
                int nDistToStart = n - m_LuaBlockHighlight[0];
                int nDistToEnd   = m_LuaBlockHighlight[1] - n;
                float fAlpha1 = 0.5f - (nDistToStart * 0.1f);
                float fAlpha2 = 0.5f - (nDistToEnd * 0.1f);
                float fAlpha = fAlpha1;
                if(fAlpha2 > fAlpha1) fAlpha = fAlpha2;

                if(fAlpha > 0.0f)
                {
                    glColor4f(0,1,1, fAlpha);
                    DrawCharBlock();
                    glColor4f(0.7,0.7,0.7,1);
                }
            }

            if (m_Selection && n>=SelectionBegin() && n<SelectionEnd()) // draw selection highlight
            {
                glColor4f(0,1,0,0.5f);
                DrawCharBlock();
                glColor4f(0.7,0.7,0.7,1);
            }
        }

        if (m_Position==n) // draw cursor
        {
            DrawCursor();
            glColor4f(0.7,0.7,0.7,1);
            drawncursor=true;
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
                if(m_bNativeControl)
                {
                    float dx = 0;
                    float dy = 0;
                    StrokeCharacter(m_Text[n], dx, dy);
                }
                else
                {
                    // Call Lua for each character rendered so effects can be implemented in Lua
                    stringstream ss;
                    //ss << "edRenderChar([[" << m_Text[n] << "]]," << xpos << "," << ypos << ")";
                    ss << "edRenderChar(string.char(" << (int)m_Text[n] << ")," << n << "," << xpos << "," << ypos << ")";
                    luaCall(ss.str());
                }

                xpos+=m_CharWidth;
            }

            xcount++;
        }
    }

    // draw cursor if we have no text, or if we're at the end of the buffer
    if (!drawncursor)
    {
        DrawCursor();
        glColor4f(0.7,0.7,0.7,1);
    }

    glPopMatrix();
]]