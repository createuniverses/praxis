
function assertgl()
  local s = glGetError()
  if s ~= "GL_NO_ERROR" then
    error(s)
  end
end

function assertglshader(res)
  if res:sub(1,5) == "Error" then error(res) end
end

if setClearColor_c == nil then
  setClearColor_c = setClearColor
end

function setClearColor(r,g,b)
  if isRunning() then
    setClearColor_c(r,g,b)
  end
end

glColor = colorGL
