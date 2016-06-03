
thing = dofile("template.lua")

thingy = thing.new()

thingy.name = "fugue"

thingymt = getmetatable(thingy)

function thingymt.__index:hello()
 print2("hello, " .. self.name)
end

thingy:hello()
hello, fugue

