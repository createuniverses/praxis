-- praxis:

--edNativeControlOn()
edNativeControlOff()
clearError()

edGetFirstVisiblePosition = edGetTopPosition
edGetLastVisiblePosition = edGetBottomPosition

function glColor4f(r,g,b,a)
  r = r * 255
  g = g * 255
  b = b * 255
  a = a * 255
  glColor(r,g,b,a)
end

glTranslatef = glTranslate

function glVertex2f(x,y)
  glVertex(x,y,0)
end

glVertex3f = glVertex

function edDrawCursor()
  --do return end
  
  local charWidth = edGetCharWidth()
  local cursorWidth = charWidth / 3.0
  local charHeight = edGetCharHeight()
  
  glColor4f(1.0, 1.0, 0.0, 1.0);
  local half = cursorWidth * 0.5;
  
  glBegin(GL_QUADS);
    glVertex2f(half,0);
    glVertex2f(half,charHeight);
    glVertex2f(-half,charHeight);
    glVertex2f(-half,0);
  glEnd();
end

clearError()


function edDrawCharBlock()
  --do return end
  
  local charWidth = edGetCharWidth()
  local charHeight = edGetCharHeight()
  
  glBegin(GL_QUADS)
    glVertex3f(charWidth * 1.0,  -charHeight * 0.1,    0);
    glVertex3f(charWidth * 1.0,   charHeight * 0.9,    0);
    glVertex3f(0,                 charHeight * 0.9,    0);
    glVertex3f(0,                -charHeight * 0.1,    0);
  glEnd();
end

function edRenderChars()
  glPushMatrix()

  local xcount=0
  local xpos=0
  local ypos=0
  local drawncursor=false
  
  local ph1,ph2 = edGetParenHighlight()
  local lb1,lb2 = edGetLuaBlockPosition()
  local sel1,sel2,sel3 = edGetSelectionPositions()
  
  local pos = edGetPosition()
  local leftpos = edGetLeftPosition()
  local rightpos = leftpos+edGetVisColumns()
  
  local charHeight = edGetCharHeight()
  local charWidth = edGetCharWidth()

  for n=edGetFirstVisiblePosition(),edGetLastVisiblePosition()-1,1 do
    local c = edGetCharAt(n)
    c = string.char(c)
    
    if c ~= "\n" then
      if n >= ph1 and n <= ph2 then
        glColor4f(0,0,1,0.5);
        edDrawCharBlock();
        glColor4f(0.7,0.7,0.7,1);
      end
      
      if n >= lb1 and n <= lb2 then
        local nDistToStart = n - lb1;
        local nDistToEnd   = lb2 - n;
        local fAlpha1 = 0.5 - (nDistToStart * 0.1);
        local fAlpha2 = 0.5 - (nDistToEnd * 0.1);
        local fAlpha = fAlpha1;
        if fAlpha2 > fAlpha1 then fAlpha = fAlpha2 end
        if fAlpha > 0.0 then
          glColor4f(0,1,1, fAlpha);
          edDrawCharBlock();
          glColor4f(0.7,0.7,0.7,1);
        end
      end
      
      if edIsSelectionActive() and n >= sel1 and n < sel2 then
        glColor4f(0,1,0,0.5);
        edDrawCharBlock();
        glColor4f(0.7,0.7,0.7,1);
      end
    end
    
    if n == pos then
        edDrawCursor();
        glColor4f(0.7,0.7,0.7,1);
        drawncursor=true;
    end

    if c=="\n" then
      -- Put the matrix back to the top left of the document
      glPopMatrix();
      glPushMatrix();

      xpos=0;
      xcount=0;
      ypos = ypos - charHeight;

      -- Move the matrix down to the next line to be drawn
      glTranslatef(0,ypos,0);
    else
      if xcount>=leftpos and xcount<rightpos then
        edRenderChar(c,n,xpos,ypos)
        xpos = xpos + charWidth;
      end
      xcount = xcount + 1;
    end
  end

  -- draw cursor if we have no text, or if we're at the end of the buffer
  if drawncursor == false then
    edDrawCursor();
    glColor4f(0.7,0.7,0.7,1);
  end

  glPopMatrix();
end
