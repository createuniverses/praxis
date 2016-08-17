
textshader = {}

textshader.cursor   = { row = 1.0, col = 5.0 }
textshader.selstart = { row = 1.0, col = 7.0 }
textshader.selend   = { row = 3.0, col = 2.0 }
textshader.blkstart = { row = 0.0, col = 0.0 }
textshader.blkend   = { row = 0.0, col = 0.0 }

function edLineCount(pos)
  local count = 1
  local size = edGetBufferLength()
  --local pos = edGetPosition()
  for i=0,pos-1,1 do
    local n = edGetCharAt(i)
    local c = string.char(n)
    if n == 10 then count = count + 1 end
  end
  return count
end

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
    return e - edGetTopPosition()-1
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
  
  u.topleft     = glGetUniformLocation(textshader.prog, "iTopLeft")        assertgl()
  u.cursorpos   = glGetUniformLocation(textshader.prog, "iCursorPos")      assertgl()
  u.font        = glGetUniformLocation(textshader.prog, "iFont")           assertgl()
  u.textgrid    = glGetUniformLocation(textshader.prog, "iTextGrid")       assertgl()
  u.background  = glGetUniformLocation(textshader.prog, "iBackground")     assertgl()

  u.topleftv    = glGetUniformLocation(textshader.prog, "iTopLeftV")       assertgl()
end

function createtexttexture()
  glTexWBSetCurrent(0)
  textshader.stringtex = glTexWBMakeTexture(GL_RED, GL_RED, GL_UNSIGNED_BYTE)
  glTexWBClear()
  glTexWBWriteToTexture(textshader.stringtex)
end

function loadfonttexture()
  glTexWBSetCurrent(1)
  glTexWBLoadFromFile("fonttexture.bin")
  textshader.fonttex = glTexWBMakeTexture(GL_RGBA32F_ARB, GL_RGBA, GL_FLOAT)
end

function createbackgroundtexture()
  glTexWBSetCurrent(2)
  textshader.bgtex = glTexWBMakeTexture(GL_RGBA32F_ARB, GL_RGBA, GL_FLOAT)
  glTexWBClear()
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

function numtostr(n, w)
  local s = tostring(n)
  while #s < w do
    s = " " .. s
  end
  return s
end

textshader_charprocs = {}

function textshader_charprocs.init()
  textshader.cursor.row = -1
  textshader.cursor.col = -1
  
  -- clear the background colour workbench
  glTexWBSetCurrent(2)
  glTexWBClear()
  
  -- use the text workbench
  glTexWBSetCurrent(0)
end

function textshader_charprocs.procchar(i,row,col)
    setcharbgcol_wbonly(0,0,0,0.7,512*row+col)
    
    if i >= edGetVisBlockBegin() and i <= edGetVisBlockEnd() then
      setcharbgcol_wbonly(0.6,0,1,1, 512*row+col)
    end

    if i >= edGetVisSelectBegin() and i <= edGetVisSelectEnd() then
      setcharbgcol_wbonly(1,0,0,1, 512*row+col)
    end

    if i == edGetVisPosition() then
      textshader.cursor.row = row
      textshader.cursor.col = col
    end
end

function textshader_charprocs.final(len,row,col)
  if textshader.cursor.row == -1 then
    textshader.cursor.row = row
    textshader.cursor.col = col
  end
  
  glTexWBSetCurrent(2)
  glTexWBWriteToTexture(textshader.bgtex)
  
  glTexWBSetCurrent(0)  
end

textshader_charprocs.getleftpos = edGetLeftPosition

do
  textshader_charprocs_plain =
  {
    init = textshader_charprocs.init,
    procchar = function (i,row,col) end,
    final = textshader_charprocs.final,
    --final = function (len, row, col) end,
    getleftpos = function () return 0 end
  }
end

function textshader_writebuffer_main()
  texwb_writetext(edGetVisCharAt, edGetVisLength, 10, textshader_charprocs)
end

function setcharbgcol(r,g,b,a,pos)
  glTexWBSetCurrent(2)
  glTexWBSetPixel(r,g,b,a, pos)
  glTexWBWriteToTexture(textshader.bgtex)
  glTexWBSetCurrent(0)
end

function setcharbgcol_wbonly(r,g,b,a,pos)
  glTexWBSetCurrent(2)
  glTexWBSetPixel(r,g,b,a, pos)
  glTexWBSetCurrent(0)
end

function texwb_writetext(at, lenfn, startline, charprocs, margin)
  charprocs.init()
  
  glTexWBClear()
  
  local len = lenfn()
  local newline = string.byte('\n')
  local space = string.byte(' ')
  local row = 0
  local col = margin
  local xcount = 0
  for i=0,len-1,1 do
    charprocs.procchar(i,row,col)
    
    if at(i) == newline then
      for j=col,100,1 do
        glTexWBSetByte(space, 512*row+j)
      end
      local lnstr = string.format("%4d", startline + row)
      glTexWBSetByte(string.byte(lnstr:at(4)), 512*row+3)
      glTexWBSetByte(string.byte(lnstr:at(3)), 512*row+2)
      glTexWBSetByte(string.byte(lnstr:at(2)), 512*row+1)
      glTexWBSetByte(string.byte(lnstr:at(1)), 512*row+0)
      row = row + 1
      col = margin
      xcount = 0
    else
      if xcount >= charprocs.getleftpos() then
        glTexWBSetByte(at(i), 512*row+col)
        col = col + 1
      end
      xcount = xcount + 1
    end
  end
  
  -- last line
  for j=col,100,1 do
    glTexWBSetByte(space, 512*row+j)
  end
  local lnstr = string.format("%4d", startline + row)
  glTexWBSetByte(string.byte(lnstr:at(4)), 512*row+3)
  glTexWBSetByte(string.byte(lnstr:at(3)), 512*row+2)
  glTexWBSetByte(string.byte(lnstr:at(2)), 512*row+1)
  glTexWBSetByte(string.byte(lnstr:at(1)), 512*row+0)
  
  glTexWBWriteToTexture(textshader.stringtex)

  charprocs.final(len,row,col)
end

function use_text_shader(shader, left, top)
  local u = shader.uloc
  
  glUseProgram(shader.prog)
  assertgl()

  do
    glUniformf(u.topleft, left, top)
    glUniformf(u.topleftv, 100, 100)
    assertgl()
  end
  
  glActiveTexture(0);
  glBindTexture(shader.fonttex)    assertgl()
  
  glActiveTexture(1)
  glBindTexture(shader.stringtex)  assertgl()
  
  glActiveTexture(2)
  glBindTexture(shader.bgtex)      assertgl()
  
  glUniformi(u.font,       0)      assertgl()
  glUniformi(u.textgrid,   1)      assertgl()
  glUniformi(u.background, 2)      assertgl()
  
  glUniformf(u.cursorpos, shader.cursor.col,    shader.cursor.row)    assertgl()
end


compiletextshader()
loadfonttexture()
createtexttexture()
createbackgroundtexture()

glTexWBSetCurrent(0)

setMaxFramerate(30)
edSetVisLines(80)
edSetVisColumns(80)

edNativeControlOff()
function edPrerenderBg()
  local x,y,w,h = getWindowRect()
  h = h - 50
  
  local topline = edLineCount(edGetTopPosition())
  texwb_writetext(edGetVisCharAt, edGetVisLength, topline, textshader_charprocs, 5)
  
  use_text_shader(textshader, 30, h)
  
  local nl = math.floor(h / 12) - 2
  edSetVisLines(nl)
end

function edPostrenderBg()
  glUseProgram(0)
end

edSetLeftMargin(0)
edSetBottomMargin(0)
edSetTopMargin(1)
edSetRightMargin(1.0)














