t:forward(10)
clearError()

for i=1,10,1 do
  t:forward(10)
  t:rotate(-14)
end


-- the scheme painter to add pls b0ss

-- setBufferName needs to check that there isn't a file with that name already
-- if there is, rename until it doesn't conflict.

setBufferName("thoughts.lua")



do
  closeBuffer()
  switchToBuffer("pentagon.lua")
end
