
textshader = {}

textshader.docshader    = {}

function compiletextshader()
  local g = textshader
  
  g.docshader.prog,shadres = glCreateProgram(
    shaderpassthruvertex,
    assembleshadersource("textshader-image.glsl"))
  assertglshader(shadres)
  
  gather_shader_uniforms(g.docshader)
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
