-- Beginnings of a text shader
-- Takes a font fbo and texture encoding a string as input

if textshader == nil then
textshader = {}
end

textshader.docshader    = {}
if textshader.fontshader == nil then
  textshader.fontshader   = {}
end

textshader.stringshader = {}
textshader.copyshader   = {}

if textshader.fontshader.prog == nil then
printf("Compiling font shader...\n")
textshader.fontshader.prog,shadres = glCreateProgram(
  shadermvpvertex,
  assembleshadersource("textshader-font.glsl"))
assertglshader(shadres)
printf("Compiling font shader...Done.\n")
else
printf("Font shader already exists, skipping...\n")
end

printf("Compiling string shader...\n")

textshader.stringshader.prog,shadres = glCreateProgram(
  shaderpassthruvertex,
  assembleshadersource("textshader-string.glsl"))
assertglshader(shadres)
printf("Compiling string shader...Done.\n")

textshader.docshader.prog,shadres = glCreateProgram(
  shaderpassthruvertex,
  assembleshadersource("textshader-image.glsl"))
assertglshader(shadres)

local function preparething()
  local g = textshader

  --local sz = 256
  local sz = 512
  
  g.fbo_font_curr   = makefbo(sz,sz, GL_NEAREST)
  g.fbo_font_prev   = makefbo(sz,sz, GL_NEAREST)
  g.fbo_string_curr = makefbo(sz,sz, GL_NEAREST)
  g.fbo_string_prev = makefbo(sz,sz, GL_NEAREST)
  
  fbotest = makefbo(sz,sz, GL_NEAREST)
  
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
  g.fbo_string_curr, g.fbo_string_prev = g.fbo_string_prev, g.fbo_string_curr
end


