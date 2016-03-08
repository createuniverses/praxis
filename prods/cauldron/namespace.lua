-- namespace.lua

function useNamespace(space)
  setmetatable(_G, {__index = space})
end

function useGlobalNamespace()
  setmetatable(_G, nil)
end
