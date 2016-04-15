-- Beginnings of a text shader
-- Takes a font fbo and texture encoding a string as input

textshader = {}

textshader.docshader    = {}
textshader.fontshader   = {}
textshader.stringshader = {}
textshader.copyshader   = {}

textshader.fontshader.prog,shadres = glCreateProgram(
  shadermvpvertex,
  assembleshadersource("textshader-font.glsl"))
assertglshader(shadres)

textshader.stringshader.prog,shadres = glCreateProgram(
  shadermvpvertex,
  assembleshadersource("textshader-string.glsl"))
assertglshader(shadres)

textshader.docshader.prog,shadres = glCreateProgram(
  shadermvpvertex,
  assembleshadersource("textshader-image.glsl"))
assertglshader(shadres)

local function preparething()
  local g = textshader
  
  g.fbo_font_curr   = makefbo(512,512, GL_NEAREST)
  g.fbo_font_prev   = makefbo(512,512, GL_NEAREST)
  g.fbo_string_curr = makefbo(512,512, GL_NEAREST)
  g.fbo_string_prev = makefbo(512,512, GL_NEAREST)
  
  fbotest = makefbo(512,512, GL_NEAREST)
  
  gather_shader_uniforms(g.docshader)
  gather_shader_uniforms(g.fontshader)
  gather_shader_uniforms(g.stringshader)
end

preparething()

function prerender()
  --do return end
  
  local g = textshader
  
  render_to_fbo_with_input(g.fbo_font_curr,   g.fontshader,   g.fbo_font_prev)
  render_to_fbo_with_input(g.fbo_string_curr, g.stringshader, g.fbo_string_prev)
  render_to_fbo_with_input(fbotest,           g.docshader,    g.fbo_font_curr, g.fbo_string_curr )
  
  g.fbo_font_curr, g.fbo_font_prev = g.fbo_font_prev, g.fbo_font_curr
  g.fbo_text_curr, g.fbo_text_prev = g.fbo_text_prev, g.fbo_text_curr
end


