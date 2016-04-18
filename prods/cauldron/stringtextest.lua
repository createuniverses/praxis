-- praxis:
-- stringtex,stringtexerr = glStringToTexture(readFile("prod.lua"))
stringtex,stringtexerr = glStringToTexture("hello there how are you this is interesting!!")
--stringtex,stringtexerr = glStringToTexture("aZaZ")
print(stringtex)
print(stringtexerr)

-- praxis:
shader_frame_num = 0

setMaxFramerate(60)

function prerender()
  
  local g = textshader
  
  --render_to_fbo_with_input(g.fbo_font_curr,   g.fontshader,   g.fbo_font_prev)  
  --render_to_fbo_with_input(g.fbo_string_curr, g.stringshader, g.fbo_string_prev)
  
  --render_to_fbo_with_input(fbotest,           g.docshader,    { texId = stringtex }, g.fbo_string_curr )
  
  
  --render_to_fbo_with_input(fbotest,           g.docshader,    g.fbo_font_curr, g.fbo_string_curr )
  render_to_fbo_with_input(fbotest,           g.docshader,    g.fbo_font_curr, { texId = stringtex } )
  
  g.fbo_font_curr, g.fbo_font_prev = g.fbo_font_prev, g.fbo_font_curr
  g.fbo_string_curr, g.fbo_string_prev = g.fbo_string_prev, g.fbo_string_curr
end
