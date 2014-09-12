print2(forth("1 2 3 + ."))
5 
do
print2(forth(
[[: test 
    begin 0 until ;]]))
end

print2(forth("test"))
setBufferName("infiniteforth.lua")
