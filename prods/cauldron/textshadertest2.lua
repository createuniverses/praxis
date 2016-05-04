-- praxis:
newfont = glStringToTexture(" ", GL_RGBA32F_ARB, GL_RGBA, GL_FLOAT)
printf(newfont.."\n")

-- praxis:
numtextshaderrenderings = 0
function prerender()
  if numtextshaderrenderings < 2 then
    printf("hello from the new one\n")
  end
  
  local g = textshader
  
  render_to_fbo_with_input(fbotest, g.docshader, { texId = newfont },         { texId = stringtex })
  --render_to_fbo_with_input(fbotest, g.docshader, { texId = g.fbo_font_curr.texId }, { texId = stringtex })
  
  g.fbo_font_curr, g.fbo_font_prev = g.fbo_font_prev, g.fbo_font_curr
  
  numtextshaderrenderings = numtextshaderrenderings + 1
end
continue()

-- praxis:
local g = textshader
glSetStringTexture(g.fbo_font_curr.texId, GL_RGBA, GL_FLOAT)
glSaveStringTexture("test2.bin")

-- praxis:
glLoadStringTexture("test2.bin")

-- praxis:
newfont = glCreateTexture("dummy", GL_RGBA32F_ARB, GL_RGBA, GL_FLOAT)

local g = textshader
printf(g.fbo_font_curr.texId)

-- praxis:
clearError()

-- praxis:
printf(getErrorText())
clearError()

local g = textshader

textshader.docshader.prog,shadres = glCreateProgram(
  shaderpassthruvertex330,
  assembleshadersource330("textshader-image-uinttexture.glsl"))
  --shaderpassthruvertex300es,
  --assembleshadersource300es("textshader-image-uinttexture.glsl"))
  
assertglshader(shadres)

gather_shader_uniforms(g.docshader)


-- praxis:
numtextshaderrenderings = 0
--setClipboardText(numtextshaderrenderings)

 stringtex = glStringToTexture(
  "[Greetings] Hello there, giddy restlessness ",
  --"Successful test!! ",
  --readFile("prod.lua"),
  GL_RGBA8UI_EXT,
  GL_RGBA_INTEGER_EXT,
  GL_UNSIGNED_BYTE)

-- praxis:
f7Pressed()
