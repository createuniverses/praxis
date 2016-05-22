
textshader = {}

textshader.cursor   = { row = 1.0, col = 5.0 }
textshader.selstart = { row = 1.0, col = 7.0 }
textshader.selend   = { row = 3.0, col = 2.0 }
textshader.blkstart = { row = 0.0, col = 0.0 }
textshader.blkend   = { row = 0.0, col = 0.0 }

function edGetVisCharAt(i)
  return edGetCharAt(edGetTopPosition() + i)
end

function edGetVisLength()
  return edGetBottomPosition() - edGetTopPosition()
end

function edGetVisPosition()
  return edGetPosition() - edGetTopPosition()
end

function edGetVisSelectBegin()
  if edIsSelectionActive() then
    local b,e,a = edGetSelectionPositions()
    return b - edGetTopPosition()
  else
    return -1
  end
end

function edGetVisSelectEnd()
  if edIsSelectionActive() then
    local b,e,a = edGetSelectionPositions()
    return e - edGetTopPosition()
  else
    return -1
  end
end

function edGetVisBlockBegin()
  local b,e = edGetLuaBlockPosition()
  return b-edGetTopPosition()
end

function edGetVisBlockEnd()
  local b,e = edGetLuaBlockPosition()
  return e-edGetTopPosition()-1
end

function compiletextshader()
  textshader.prog,shadres = glCreateProgram(
    readFile("textshader-vertex.glsl"),
    readFile("textshader-fragment.glsl"))
  assertglshader(shadres)
  
  glGetError()
  
  textshader.uloc = {}
  local u = textshader.uloc
  
  u.frame       = glGetUniformLocation(textshader.prog, "iFrame")          assertgl()
  u.resolution  = glGetUniformLocation(textshader.prog, "iResolution")     assertgl()
  u.mouse       = glGetUniformLocation(textshader.prog, "iMouse")          assertgl()
  u.sampler     = glGetUniformLocation(textshader.prog, "iChannel0")       assertgl()
  u.sampler1    = glGetUniformLocation(textshader.prog, "iChannel0")       assertgl()
  u.sampler2    = glGetUniformLocation(textshader.prog, "iChannel1")       assertgl()
  u.globaltime  = glGetUniformLocation(textshader.prog, "iGlobalTime")     assertgl()
  
  u.cursorpos   = glGetUniformLocation(textshader.prog, "iCursorPos")      assertgl()
  u.selstart    = glGetUniformLocation(textshader.prog, "iSelectionStart") assertgl()
  u.selend      = glGetUniformLocation(textshader.prog, "iSelectionEnd")   assertgl()
  u.blkstart    = glGetUniformLocation(textshader.prog, "iBlockStart")     assertgl()
  u.blkend      = glGetUniformLocation(textshader.prog, "iBlockEnd")       assertgl()
end

function createtexttexture(s)
  textshader.stringtex = glTexWBMakeTexture(GL_RED, GL_RED, GL_UNSIGNED_BYTE)
  glTexWBClear()
  glTexWBWriteToTexture(textshader.stringtex)
end

function loadfonttexture()
  glTexWBLoadFromFile("fonttexture.bin")
  textshader.fonttex = glTexWBMakeTexture(GL_RGBA32F_ARB, GL_RGBA, GL_FLOAT)
end

function textshaderwriteline(text, linenum)
  glTexWBLoadFromString(text, 0)
  glTexWBWriteToTexture(textshader.stringtex,0,linenum,#text,1)
end

function textshaderwritetext(text)
  local row = 0
  local col = 0
  for i=1,#text,1 do
    if text:at(i) == '\n' then
      for j=col,100,1 do
        glTexWBSetByte(string.byte(' '), 512*row+j)
      end
      row = row + 1
      col = 0
    else
      glTexWBSetByte(string.byte(text:at(i)), 512*row+col)
      col = col + 1
    end
  end
  glTexWBWriteToTexture(textshader.stringtex)
end

function textshader_writebuffer(at, len)
  glTexWBClear()
  
  textshader.cursor.row = -1
  textshader.cursor.col = -1
  textshader.selstart.row = -1.0
  textshader.selstart.col = -1.0
  textshader.selend.row = -1.0
  textshader.selend.col = -1.0
  textshader.blkstart.row = -1.0
  textshader.blkstart.col = -1.0
  textshader.blkend.row = -1.0
  textshader.blkend.col = -1.0
  
  local newline = string.byte('\n')
  local space = string.byte(' ')
  local row = 0
  local col = 0
  local xcount = 0
  for i=0,len-1,1 do
    if i == edGetVisPosition() then
      textshader.cursor.row = row
      textshader.cursor.col = col
    end
    if i == edGetVisSelectBegin() then
      textshader.selstart.row = row
      textshader.selstart.col = col
    end
    if i == edGetVisSelectEnd() then
      textshader.selend.row = row
      textshader.selend.col = col
    end
    if i == edGetVisBlockBegin() then
      textshader.blkstart.row = row
      textshader.blkstart.col = col
    end
    if i == edGetVisBlockEnd() then
      textshader.blkend.row = row
      textshader.blkend.col = col
    end
    
    if at(i) == newline then
      for j=col,100,1 do
        glTexWBSetByte(space, 512*row+j)
      end
      row = row + 1
      col = 0
      xcount = 0
    else
      if xcount >= edGetLeftPosition() then
        glTexWBSetByte(at(i), 512*row+col)
        col = col + 1
      end
      xcount = xcount + 1
    end
  end
  for j=col,100,1 do
    glTexWBSetByte(space, 512*row+j)
  end
  glTexWBWriteToTexture(textshader.stringtex)
  
  if textshader.cursor.row == -1 then
    textshader.cursor.row = row
    textshader.cursor.col = col
  end
  
  if edGetVisSelectBegin() <= 0 then
      textshader.selstart.row = 0
      textshader.selstart.col = 0
  end

  if edGetVisSelectBegin() > len then
      textshader.selstart.row = row+1
      textshader.selstart.col = 0
  end
  
  if edGetVisSelectBegin() == len then
      textshader.selstart.row = row
      textshader.selstart.col = col
  end

  if edGetVisSelectEnd() > len then
      textshader.selend.row = row+1
      textshader.selend.col = 0
  end
  
  if edGetVisSelectEnd() == len then
      textshader.selend.row = row
      textshader.selend.col = col
  end
  
  if edGetVisBlockBegin() <= 0 then
      textshader.blkstart.row = 0
      textshader.blkstart.col = 0
  end
  
  if edGetVisBlockEnd() > len then
      textshader.blkend.row = row+1
      textshader.blkend.col = 1000
  end
  
  if edGetVisBlockEnd() == len then
      textshader.blkend.row = row
      textshader.blkend.col = col
  end
  
  -- need to traverse entire buffer for selection row col info
  -- not just visible area.
end

function use_text_shader(shader)
  local u = shader.uloc
  
  glUseProgram(shader.prog)
  assertgl()
  
  glUniformf(u.resolution, 512, 512);
  assertgl()
  
  glActiveTexture(0);
  glBindTexture(shader.fonttex)    assertgl()
  
  glActiveTexture(1)
  glBindTexture(shader.stringtex)  assertgl()
  
  glUniformi(u.sampler1, 0)        assertgl()
  glUniformi(u.sampler2, 1)        assertgl()
  
  glUniformf(u.cursorpos, shader.cursor.col,    shader.cursor.row)    assertgl()
  glUniformf(u.selstart,  shader.selstart.col,  shader.selstart.row)  assertgl()
  glUniformf(u.selend,    shader.selend.col,    shader.selend.row)    assertgl()
  glUniformf(u.blkstart,  shader.blkstart.col,  shader.blkstart.row)  assertgl()
  glUniformf(u.blkend,    shader.blkend.col,    shader.blkend.row)    assertgl()
end

function textshader.update()
  --textshaderwritetext(getBufferText())
  --textshader_writebuffer(edGetCharAt, edGetBufferLength())
  
  --[[
  textshader_writebuffer(
    function (i)
      return edGetCharAt(edGetTopPosition() + i) end,
    edGetBottomPosition() - edGetTopPosition())
    ]]
  
  textshader_writebuffer(edGetVisCharAt, edGetVisLength())
end

function textshader.render()
  use_text_shader(textshader)

  local h  = 5
  local qs = 100
  
  beginQuadGL()
    colorGL(255,255,255,255)
    vectorGL(    0,  h,   0)
    vectorGL( qs*2,  h,   0)
    vectorGL( qs*2,  h,  qs)
    vectorGL(    0,  h,  qs)
  endGL()
  
  glUseProgram(0)
end

compiletextshader()
loadfonttexture()
createtexttexture()

setMaxFramerate(30)
edSetVisLines(40)
edSetVisColumns(120)

enableStdMouseCam()

--setCamPos(50,60,50)
setCamPos(100,60,50)
--setCamPos(100,58,50)
lookDown()

textshaderwidget = WidgetLib2.newSimple("textshader")
textshaderwidget.render = function (o) textshader.render() end
textshaderwidget.update = function (o) textshader.update() end
Widgets["textshader"] = textshaderwidget



