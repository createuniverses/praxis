shader300esheader = [[
#version 300 es

precision mediump float;
precision mediump int;

]]

shader330header = [[
#version 330

precision highp float;
precision highp int;

]]

shader330footer = [[

out vec4 myFragColor;

void main()
{
    mainImage(myFragColor, gl_FragCoord.xy );
    //mainImage(gl_FragColor, gl_FragCoord.xy );
}
]]

shader300esfooter = shader330footer

shaderpassthruvertex300es = [[
#version 300 es

in vec4 myVertex;

void main(void)
{ 
    //gl_Position = gl_Vertex;
    gl_Position = myVertex;
    gl_Position.w = 1.0;
}
]]

shaderpassthruvertex330 = [[
#version 330

in vec4 myVertex;

void main(void)
{ 
    //gl_Position = gl_Vertex;
    gl_Position = myVertex;
    gl_Position.w = 1.0;
}
]]

function assembleshadersource300es(file)
  local s = shader300esheader .. shaderheader .. readFile(file) .. shader300esfooter
  return s
end

function assembleshadersource330(file)
  local s = shader330header .. shaderheader .. readFile(file) .. shader330footer
  return s
end

-- commented from textshader.lua
    --shaderpassthruvertex330,
    --assembleshadersource330("textshader-image-uinttexture.glsl"))
    --shaderpassthruvertex300es,
    --assembleshadersource300es("textshader-image-uinttexture.glsl"))
