
textshader.fontshader   = textshader.fontshader or {}

numtextshaderrenderings = 0

function compilefontshader()
  local g = textshader
  
  printf("Compiling font shader...\n")
  -- textshader-font-mix is SIGNIFICANTLY faster on that first frame than textshader-font-choose

  g.fontshader.prog,shadres = glCreateProgram(
    shaderpassthruvertex330,
    assembleshadersource330("textshader-font-mix.glsl"))
    --shaderpassthruvertex300es,
    --assembleshadersource300es("textshader-font-mix.glsl"))
  
  assertglshader(shadres)
  
  gather_shader_uniforms(g.fontshader)
  
  printf("Compiling font shader...Done.\n")
end

function textshader_make_font_fbos()
  local g = textshader
  local sz = 512
  g.fbo_font_curr   = makefbo(sz,sz, GL_NEAREST)
  g.fbo_font_prev   = makefbo(sz,sz, GL_NEAREST)
end

function textshader_prerender_font()
  local g = textshader
  render_to_fbo_with_input(g.fbo_font_curr,   g.fontshader,   g.fbo_font_prev)
  g.fbo_font_curr, g.fbo_font_prev = g.fbo_font_prev, g.fbo_font_curr
end
