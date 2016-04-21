-- Beginnings of a text shader
-- Takes a font fbo and texture encoding a string as input

textshader = textshader or {}

textshader.docshader    = {}
textshader.fontshader   = textshader.fontshader or {}
textshader.stringshader = {}
textshader.copyshader   = {}

if textshader.fontshader.prog == nil then
printf("Compiling font shader...\n")
-- textshader-font-mix is SIGNIFICANTLY faster on that first frame than textshader-font-choose
textshader.fontshader.prog,shadres = glCreateProgram(
  shadermvpvertex,
  assembleshadersource("textshader-font-mix.glsl"))
assertglshader(shadres)
printf("Compiling font shader...Done.\n")
else
printf("Skipping compilation of font shader.\n")
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
  gather_shader_uniforms(g.stringshader)
  

  gather_shader_uniforms(g.fontshader)
  
end

preparething()

function prerender()
  --do return end
  
  --printf("prerender start\n")
  
  local g = textshader
  
  -- The first render using the font shader is very slow on some computers.
  -- After that the first frame, its fine though.
  -- Also, it isn't necessary to refresh the font FBO after it has been generated,
  -- so this only needs to happen once. Refraining from calling this each frame speeds up
  -- the framerate considerably.
  render_to_fbo_with_input(g.fbo_font_curr,   g.fontshader,   g.fbo_font_prev)
  
  --printf("prerender 2\n")
  render_to_fbo_with_input(g.fbo_string_curr, g.stringshader) --, g.fbo_string_prev)
  --printf("prerender 3\n")
  render_to_fbo_with_input(fbotest,           g.docshader,    g.fbo_font_curr, g.fbo_string_curr )
  --printf("prerender 4\n")
  
  g.fbo_font_curr, g.fbo_font_prev = g.fbo_font_prev, g.fbo_font_curr
  --printf("prerender 5\n")
  g.fbo_string_curr, g.fbo_string_prev = g.fbo_string_prev, g.fbo_string_curr
  
  --printf("prerender end\n")
end

stringtex = glStringToTexture("Ok, this is fantastic!! A text editor in a shader!")

setMaxFramerate(30)
enableStdMouseCam()

numtextshaderrenderings = 0

function prerender()
  
  local g = textshader
  
  if numtextshaderrenderings < 2 then
    render_to_fbo_with_input(g.fbo_font_curr,   g.fontshader,   g.fbo_font_prev)
  end
  
  --glDisable(GL_BLEND)
  
  render_to_fbo_with_input(fbotest, g.docshader, g.fbo_font_curr, { texId = stringtex })
  
  g.fbo_font_curr, g.fbo_font_prev = g.fbo_font_prev, g.fbo_font_curr
  
  numtextshaderrenderings = numtextshaderrenderings + 1
end
