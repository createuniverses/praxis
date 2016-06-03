
local methods = {}
local mt = { __index = methods }

local new

function new()
  return setmetatable({}, mt)
end

return { new = new }
