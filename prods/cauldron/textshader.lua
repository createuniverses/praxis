-- Beginnings of a text shader
-- Takes a font fbo and texture encoding a string as input

textshader = textshader or {}

--textshader = {}

textshader.docshader    = {}
textshader.fontshader   = textshader.fontshader or {}
textshader.stringshader = {}
textshader.copyshader   = {}

if textshader.fontshader.prog == nil then
  printf("Compiling font shader...\n")
  -- textshader-font-mix is SIGNIFICANTLY faster on that first frame than textshader-font-choose

  textshader.fontshader.prog,shadres = glCreateProgram(
    --shadermvpvertex,
    shaderpassthruvertex300es,
    --assembleshadersource("textshader-font-mix.glsl"))
    assembleshadersource300es("textshader-font-choose.glsl"))
    --assembleshadersource("textshader-font-origfix.glsl"))
  
  assertglshader(shadres)
  
  printf("Compiling font shader...Done.\n")
else
  printf("Skipping compilation of font shader.\n")
end

textshader.docshader.prog,shadres = glCreateProgram(
  shaderpassthruvertex300es,
  assembleshadersource300es("textshader-image-uinttexture.glsl"))
  --assembleshadersource300es("textshader-image-inttexture.glsl"))
  --assembleshadersource300es("textshader-image.glsl"))
assertglshader(shadres)

printf("Compiling all shaders...Done.\n")

local function preparething()
  local g = textshader

  local sz = 512
  
  g.fbo_font_curr   = makefbo(sz,sz, GL_NEAREST)
  g.fbo_font_prev   = makefbo(sz,sz, GL_NEAREST)
  
  fbotest = makefbo(sz,sz, GL_NEAREST)
  
  gather_shader_uniforms(g.docshader)
  gather_shader_uniforms(g.fontshader)
  
end

preparething()

--stringtex = glStringToTexture("Ok, this is fantastic!! A text editor in a shader!", GL_RGBA32F_ARB, GL_INT)
--stringtex = glStringToTexture("Ok, this is fantastic!! A text editor in a shader!", GL_RGBA32F_ARB, GL_FLOAT)
--stringtex = glStringToTexture("Ok,_this_is_fantastic!!_A_text_editor_in_a_shader!  ", GL_RGBA32F_ARB, GL_UNSIGNED_INT)
--stringtex = glStringToTexture("Ok,_this_is_fantastic!!_A_text_editor_in_a_shader!  sdsdsdsds", GL_RGBA32F_ARB, GL_FLOAT)
--stringtex = glStringToTexture("Ok,_this_is_fantastic!!_A_text editor_in_a_shader!___")
--stringtex = glStringToTexture("1234567 ")


--stringtex = glStringToTexture("1234567 ", GL_RGBA32F_ARB, GL_FLOAT-5)
--stringtex = glStringToTexture("1234567 ", 36249, GL_INT)

--stringtex = glStringToTexture("abcdefg ", GL_RGBA32F_ARB, GL_UNSIGNED_INT)


setMaxFramerate(30)
enableStdMouseCam()

numtextshaderrenderings = 0

function prerender()
  
  local g = textshader
  
  if numtextshaderrenderings < 2 then
  printf("Rendering font...\n")
    render_to_fbo_with_input(g.fbo_font_curr,   g.fontshader,   g.fbo_font_prev)
  printf("Rendering font...Done\n")
  end
  
  --glDisable(GL_BLEND)
  
  render_to_fbo_with_input(fbotest, g.docshader, g.fbo_font_curr, { texId = stringtex })
  
  g.fbo_font_curr, g.fbo_font_prev = g.fbo_font_prev, g.fbo_font_curr
  
  numtextshaderrenderings = numtextshaderrenderings + 1
end

