
sandbox1 = {}

gmeta = {}

function gmeta.__index(tbl,key)
  return sandbox1[key]
end

function gmeta.__newindex(tbl,key,val)
  sandbox1[key] = val
end

setmetatable(_G, gmeta)

function gmetatest1()
  if getmetatable(_G) == nil then
    print2("its nil")
  end
end
