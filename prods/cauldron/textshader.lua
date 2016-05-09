
textshader = {}

textshader.docshader    = {}

shaderheader2 = [[
uniform vec2       iCursorPos;
uniform vec2       iSelectionStart;
uniform vec2       iSelectionEnd;
]]

textshader.cursor   = { row = 1.0, col = 5.0 }
textshader.selstart = { row = 1.0, col = 7.0 }
textshader.selend   = { row = 3.0, col = 2.0 }

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

textshader.docshader.extrauniforms = function ()
  local u = textshader.docshader.uloc
  glUniformf(u.cursorpos, textshader.cursor.col,    textshader.cursor.row)    assertgl()
  glUniformf(u.selstart,  textshader.selstart.col,  textshader.selstart.row)  assertgl()
  glUniformf(u.selend,    textshader.selend.col,    textshader.selend.row)    assertgl()
end


function textshader_assembleshadersource(file)
  local s = shaderheader .. shaderheader2 .. readFile(file) .. shaderfooter
  return s
end

function compiletextshader()
  local g = textshader
  
  g.docshader.prog,shadres = glCreateProgram(
    shaderpassthruvertex,
    textshader_assembleshadersource("textshader-image.glsl"))
  assertglshader(shadres)
  
  gather_shader_uniforms(g.docshader)
  
  local u = g.docshader.uloc

  u.cursorpos  = glGetUniformLocation(g.docshader.prog, "iCursorPos")      assertgl()
  u.selstart   = glGetUniformLocation(g.docshader.prog, "iSelectionStart") assertgl()
  u.selend     = glGetUniformLocation(g.docshader.prog, "iSelectionEnd")   assertgl()
end

function maketextshaderfbos()
  local g = textshader
  local sz = 512
  fbotest = makefbo(sz,sz, GL_NEAREST)
end

function updatetextshadertext(s)
  stringtex = glStringToTexture(
    s,
    GL_RGBA8UI_EXT,
    GL_RGBA_INTEGER_EXT,
    GL_UNSIGNED_BYTE)
end

function updatetextshadertext(s)
  stringtex = glStringToTexture(
    s,
    GL_ALPHA8UI_EXT,
    GL_ALPHA_INTEGER_EXT,
    GL_UNSIGNED_BYTE)
end

function updatetextshadertext(s)
  stringtex = glStringToTexture(
    s,
    GL_RED,
    GL_RED,
    GL_UNSIGNED_BYTE)
end

function loadfonttexture()
  glTexWBLoadFromFile("test2.bin")
  fonttex = glTexWBMakeTexture(GL_RGBA32F_ARB, GL_RGBA, GL_FLOAT)
end

function textshaderwriteline(text, linenum)
  glTexWBLoadFromString(text, 0)
  --glTexWBWriteToTexture(stringtex,0,linenum,math.ceil(#text / 4),1)
  glTexWBWriteToTexture(stringtex,0,linenum,#text,1)
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
  glTexWBWriteToTexture(stringtex)
end

function textshader_writebuffer(at, len)
  glTexWBClear()
  textshader.cursor.row = -1
  textshader.cursor.col = -1
  textshader.selstart.row = -1.0
  textshader.selstart.col = -1.0
  textshader.selend.row = -1.0
  textshader.selend.col = -1.0
  local newline = string.byte('\n')
  local space = string.byte(' ')
  local row = 0
  local col = 0
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
    if at(i) == newline then
      for j=col,100,1 do
        glTexWBSetByte(space, 512*row+j)
      end
      row = row + 1
      col = 0
    else
      glTexWBSetByte(at(i), 512*row+col)
      col = col + 1
    end
  end
  for j=col,100,1 do
    glTexWBSetByte(space, 512*row+j)
  end
  glTexWBWriteToTexture(stringtex)
  
  if textshader.cursor.row == -1 then
    textshader.cursor.row = row
    textshader.cursor.col = col
  end
  
  if edGetVisSelectBegin() <= 0 then
      textshader.selstart.row = 0
      textshader.selstart.col = 0
  end

  if edGetVisSelectEnd() >= len then
      textshader.selend.row = row
      textshader.selend.col = 1000.0 -- col
  end
  
  -- need to traverse entire buffer for selection row col info
  -- not just visible area.
end

compiletextshader()
maketextshaderfbos()

loadfonttexture()
updatetextshadertext("abcdefghijklmnopqrstuvwxyz 0123456789 ")
updatetextshadertext("0123456789 !@#$%^&*()_+")

setMaxFramerate(30)
enableStdMouseCam()

function prerender()
  local g = textshader
  render_to_fbo_with_input(fbotest, g.docshader, { texId = fonttex }, { texId = stringtex })
end

function clamp(x,min,max)
  if x < min then return min end
  if x > max then return max end
  return x
end

function update()
  WidgetLib.callAll("update")

  for i=1,#skythings,1 do
    local thing = skythings[i]
    local planepos = vec3d(transform.getTranslation(airplane.lspace))
    local tween = thing.p - planepos
    local dist = Vector3D.magnitude(tween)
    tween = Vector3D.normalize(tween)
    if dist < (30 + thing.r) then
      thing.p = planepos + (tween * (30+thing.r))
    end
  end

  --textshaderwritetext(getBufferText())
  --textshader_writebuffer(edGetCharAt, edGetBufferLength())
  
  --[[
  textshader_writebuffer(
    function (i)
      return edGetCharAt(edGetTopPosition() + i) end,
    edGetBottomPosition() - edGetTopPosition())
    ]]
  
  textshader_writebuffer(edGetVisCharAt, edGetVisLength())
  
  local r,g,b = getClearColor()
  r = clamp(r-20,0,255)
  g = clamp(g-20,0,255)
  b = clamp(b-20,0,255)
  setClearColor(r,g,b)
end

edSetVisLines(40)



